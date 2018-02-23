(in-package :sir-stencilbot)

(deftype game-move ()
  '(member :north :south :east :west :stay))

(deftype game-tile ()
  '(member :air :wall :tavern :mine))

(deftype coordinate ()
  '(unsigned-byte 8))

(deftype hero-life ()
  '(integer 0 100))

(defmacro define-static-reader (struct-name slot-name)
  (let ((reader-name (symbolicate struct-name '#:- slot-name)))
    `(progn
       (declaim (inline ,reader-name))
       (defun ,reader-name (,struct-name)
         (declare (,struct-name ,struct-name))
         (,(symbolicate struct-name '#:-static-state- slot-name)
          (,(symbolicate struct-name '#:-static-state)
           ,struct-name))))))

(defstruct hero-static-state
  (id      nil :type non-negative-fixnum :read-only t)
  (name    nil :type string              :read-only t)
  (user-id nil :type string              :read-only t)
  (elo     nil :type non-negative-fixnum :read-only t)
  (spawn-x nil :type coordinate          :read-only t)
  (spawn-y nil :type coordinate          :read-only t))

(defstruct hero
  (static-state nil :type hero-static-state)
  (last-dir nil :type game-move)
  (gold     nil :type non-negative-fixnum)
  (life     nil :type hero-life)
  (x        nil :type coordinate)
  (y        nil :type coordinate))

(define-static-reader hero id)
(define-static-reader hero name)
(define-static-reader hero elo)
(define-static-reader hero spawn-x)
(define-static-reader hero spawn-y)

(defstruct game-static-state
  (id           nil :type string                         :read-only t)
  (player-id    nil :type (integer 1 3)                  :read-only t)
  (training     nil :type boolean                        :read-only t)
  (board        nil :type (simple-array game-tile (* *)) :read-only t)
  (max-turns    nil :type non-negative-fixnum            :read-only t)
  (mine-positions nil :type (simple-array (cons coordinate coordinate) (*))))

(defstruct game
  (static-state nil :type game-static-state)
  (turn   0   :type non-negative-fixnum)
  (active-hero 1 :type (integer 1 4))
  (mine-owners nil :type (simple-array (integer 0 4) (*)))
  (hero-1 nil :type hero)
  (hero-2 nil :type hero)
  (hero-3 nil :type hero)
  (hero-4 nil :type hero))

(define-static-reader game id)
(define-static-reader game training)
(define-static-reader game board)
(define-static-reader game spawn-points)
(define-static-reader game max-turns)
(define-static-reader game mine-positions)

(defun game-hero (game id)
  (ecase id
    (1 (game-hero-1 game))
    (2 (game-hero-2 game))
    (3 (game-hero-3 game))
    (4 (game-hero-4 game))))

(defun (setf game-hero) (game id hero)
  (ecase id
    (1 (setf (game-hero-1 game) hero))
    (2 (setf (game-hero-2 game) hero))
    (3 (setf (game-hero-3 game) hero))
    (4 (setf (game-hero-4 game) hero))))

(defun game-board-ref (game x y)
  (let ((board (game-board game)))
    (if (array-in-bounds-p board x y)
        (aref board x y)
        :wall)))

(defun game-finished-p (game)
  (= (game-turn game)
     (game-max-turns game)))

(defun advance-game (game move)
  (let* ((game (copy-game game))
         (active (game-active-hero game))
         (hero (copy-hero (game-hero game active))))
    (setf (game-hero game active) hero)
    ;; Hero move
    (multiple-value-bind (new-x new-y)
        (let ((x (hero-x hero))
              (y (hero-y hero)))
          (case move
            (:north (values (1+ x) y))
            (:south (values (1- x) y))
            (:east  (values x (1+ y)))
            (:west  (values x (1- y)))
            (:stay  (values x y))))
      (let ((tile (game-board-ref game new-x new-y)))
        (case tile
          (:air
           (unless (hero-at-coordinates-p game new-x new-y)
             (setf (hero-x hero) new-x)
             (setf (hero-y hero) new-y)))
          (:tavern
           (when (>= (hero-gold hero) 2)
             (setf (hero-life hero)
                   (min (+ (hero-life hero) 50) 100))))
          (:mine
           (if (> (hero-life hero) 20)
               ;; conquer a mine
               (setf (aref (game-mine-owners game)
                           (position (cons new-x new-y)
                                     (game-mine-positions game)
                                     :test #'equal))
                     (hero-id hero))
               ;; die painfully
               (kill-hero game hero 0))
           (break))
          (:wall))))
    ;; Fight
    (damage-neighbors game hero)
    ;; income
    (incf (hero-gold hero)
          (loop for owner across (game-mine-owners game)
                count (= owner (hero-id hero))))
    ;; thirst
    (unless (= 1 (hero-life hero))
      (decf (hero-life hero)))
    (if (= 4 (game-active-hero game))
        (setf (game-active-hero game) 1)
        (incf (game-active-hero game) 1))
    game))

(defun hero-at-coordinates-p (game x y)
  (flet ((collision-with (hero)
           (and (= x (hero-x hero))
                (= y (hero-y hero)))))
    (or (collision-with (game-hero-1 game))
        (collision-with (game-hero-2 game))
        (collision-with (game-hero-3 game))
        (collision-with (game-hero-4 game)))))

(defun damage-neighbors (game hero)
  (let ((hero-x (hero-x hero))
        (hero-y (hero-y hero)))
    (macrolet ((maybe-damage (neighbor-place)
                 `(let ((neighbor ,neighbor-place))
                    (when (xor (= 1 (abs (- hero-x (hero-x neighbor))))
                               (= 1 (abs (- hero-y (hero-y neighbor)))))
                      (setf ,neighbor-place
                            (let ((copy (copy-hero neighbor)))
                              (if (> (hero-life copy) 20)
                                  (decf (hero-life copy) 20)
                                  (kill-hero game copy (hero-id hero)))
                              (setf ,neighbor-place copy)))))))
      (maybe-damage (game-hero-1 game))
      (maybe-damage (game-hero-2 game))
      (maybe-damage (game-hero-3 game))
      (maybe-damage (game-hero-4 game)))))

(defun kill-hero (game looser winner-id)
  (setf (hero-x looser) (hero-spawn-x looser))
  (setf (hero-y looser) (hero-spawn-y looser))
  (setf (hero-life looser) 100)
  (let ((mine-owners (copy-array (game-mine-owners game))))
    (loop for index below (length mine-owners) do
      (when (= (aref mine-owners index)
               (hero-id looser))
        (setf (aref mine-owners index) winner-id)))
    (setf (game-mine-owners game) mine-owners)))
