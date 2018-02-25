(in-package :vindinium)

(defun game-simulate (game move)
  (declare (optimize speed))
  (let* ((game (copy-game game))
         (active (game-active-id game))
         (hero (copy-hero (game-hero game active))))
    (setf (game-hero game active) hero)
    (setf (game-mine-owners game)
          (copy-array (game-mine-owners game)))
    ;; Hero move
    (multiple-value-bind (new-x new-y)
        (let ((x (hero-x hero))
              (y (hero-y hero)))
          (case move
            (:north (values (1- x) y))
            (:south (values (1+ x) y))
            (:east  (values x (1+ y)))
            (:west  (values x (1- y)))
            (:stay  (values x y))))
      (unless (eq move :stay)
        (let ((tile (game-tile game new-x new-y)))
          (case tile
            (:air
             (unless (hero-at-coordinates-p game new-x new-y)
               (setf (hero-x hero) new-x)
               (setf (hero-y hero) new-y)))
            (:tavern
             (when (>= (hero-gold hero) 2)
               (setf (hero-life hero)
                     (min (+ (hero-life hero) 50) 100))
               (decf (hero-gold hero) 2)))
            (:mine
             (let ((mine-index (position (cons new-x new-y)
                                         (game-mine-positions game)
                                         :test #'equal)))
               ;; heroes cannot conquer their own mines
               (unless (= (aref (game-mine-owners game) mine-index) (hero-id hero))
                 (cond ((> (hero-life hero) 20)
                        ;; conquer a mine
                        (setf (aref (game-mine-owners game) mine-index)
                              (hero-id hero))
                        (decf (hero-life hero) 20))
                       (t
                        ;; die painfully
                        (kill-hero game hero 0))))))
            (:wall)))))
    ;; Fight
    (damage-neighbors game hero)
    ;; income
    (incf (hero-gold hero)
          (loop for owner across (game-mine-owners game)
                count (= owner (hero-id hero))))
    ;; thirst
    (unless (= 1 (hero-life hero))
      (decf (hero-life hero)))
    ;; update game state
    (setf (game-active-id game)
          (next-player (game-active-id game)))
    (incf (game-turn game))
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
    (flet ((maybe-damage (victim-id)
             (let ((victim (game-hero game victim-id)))
               (let ((dx (abs (- hero-x (hero-x victim))))
                     (dy (abs (- hero-y (hero-y victim)))))
                 (when (or (and (= 1 dx) (= 0 dy))
                           (and (= 0 dx) (= 1 dy)))
                     (setf (game-hero game victim-id)
                           (let ((copy (copy-hero victim)))
                             (if (> (hero-life copy) 20)
                                 (decf (hero-life copy) 20)
                                 (kill-hero game copy (hero-id hero)))
                             copy)))))))
      (maybe-damage 1)
      (maybe-damage 2)
      (maybe-damage 3)
      (maybe-damage 4))))

(defun kill-hero (game looser winner-id)
  (setf (hero-x looser) (hero-spawn-x looser))
  (setf (hero-y looser) (hero-spawn-y looser))
  (setf (hero-life looser) 100)
  (let ((mine-owners (game-mine-owners game)))
    (loop for index below (length mine-owners) do
      (when (= (aref mine-owners index)
               (hero-id looser))
        (setf (aref mine-owners index) winner-id))))
  (loop for hero-id from 1 to 4
        unless (= hero-id (hero-id looser)) do
          (let ((other-hero (game-hero game hero-id)))
            (when (and (= (hero-x other-hero) (hero-spawn-x looser))
                       (= (hero-y other-hero) (hero-spawn-y looser)))
              (kill-hero game other-hero (hero-id looser))))))
