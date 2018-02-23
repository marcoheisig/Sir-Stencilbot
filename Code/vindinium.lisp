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
  (user-id nil :type (or null string)    :read-only t)
  (elo     nil :type non-negative-fixnum :read-only t)
  (spawn-x nil :type coordinate          :read-only t)
  (spawn-y nil :type coordinate          :read-only t))

(defstruct hero
  (static-state nil :type hero-static-state)
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
  (player-id    nil :type (integer 1 4)                  :read-only t)
  (training     nil :type boolean                        :read-only t)
  (view-url     nil :type string                         :read-only t)
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
(define-static-reader game player-id)
(define-static-reader game training)
(define-static-reader game view-url)
(define-static-reader game board)
(define-static-reader game max-turns)
(define-static-reader game mine-positions)

(defun game-hero (game id)
  (ecase id
    (1 (game-hero-1 game))
    (2 (game-hero-2 game))
    (3 (game-hero-3 game))
    (4 (game-hero-4 game))))

(defun (setf game-hero) (hero game id)
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

(defun next-player (id)
  (if (= 4 id) 1 (1+ id)))

(defun game-equal (game-1 game-2)
  (and
   (equalp (game-mine-owners game-1)
           (game-mine-owners game-2))
   (= (game-turn game-1)
      (game-turn game-2))
   (= (game-active-hero game-1)
      (game-active-hero game-2))
   (hero-equal (game-hero-1 game-1)
               (game-hero-1 game-2))
   (hero-equal (game-hero-2 game-1)
               (game-hero-2 game-2))
   (hero-equal (game-hero-3 game-1)
               (game-hero-3 game-2))
   (hero-equal (game-hero-4 game-1)
               (game-hero-4 game-2))))

(defun hero-equal (hero-1 hero-2)
  (and
   (= (hero-gold hero-1)
      (hero-gold hero-2))
   (= (hero-x hero-1)
      (hero-x hero-2))
   (= (hero-y hero-1)
      (hero-y hero-2))))

(defun copy-full-game (game)
  (let ((new-game (copy-game game)))
    (setf (game-hero-1 new-game) (copy-hero (game-hero-1 new-game)))
    (setf (game-hero-2 new-game) (copy-hero (game-hero-2 new-game)))
    (setf (game-hero-3 new-game) (copy-hero (game-hero-3 new-game)))
    (setf (game-hero-4 new-game) (copy-hero (game-hero-4 new-game)))
    (setf (game-mine-owners new-game)
          (copy-array (game-mine-owners new-game)))))

(defun game-possible-moves (game)
  (declare (ignore game))
  ;; TODO improve this ...
  (list :north :south :east :west :stay))
