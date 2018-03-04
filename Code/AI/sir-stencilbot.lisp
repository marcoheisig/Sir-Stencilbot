(in-package :vindinium/sir-stencilbot)

;;; A hash table from positions to distance maps. The idea is that for the
;;; duration of one game, the distance map of each tile is only computed
;;; once.
(defvar *distance-maps* nil)

;;; A vector of distance maps, one for each tavern.
(defvar *tavern-distance-maps* nil)

;; The distance map of Sir Stencilbot at the beginning of the turn.
(defvar *origin-map* nil)

(defvar *origin-game* nil)

(defun sir-stencilbot (&key (training t)
                         (server "http://vindinium.walberla.net")
                         (secret-key "hujgkdbt"))
  (let* ((url (concatenate 'string server "/api/" (if training "training" "arena")))
         (game (new-game url secret-key))
         (timer (make-timer))
         (*distance-maps* (make-hash-table
                           :test #'equal
                           :size (array-total-size (game-board game))))
         (*tavern-distance-maps* (compute-tavern-distance-maps game))
         (*origin-map* nil)
         (*origin-game* nil))
    (loop until (game-finished-p game)
          for *origin-map* = (hero-distance-map game (game-active-hero game))
          for *origin-game* = game
          for next-move
            = (mcts-search
               game
               :termination-fn (lambda () (< 0.3 (funcall timer)))
               :active-player-fn (lambda (game) (1- (game-active-id game)))
               :move-fn #'game-simulate
               :playout-fn #'playout
               :untried-moves-fn #'untried-moves)
          do
             (setf game (game-send-turn game secret-key next-move))
             (setf timer (make-timer)))))

;;; The PLAYOUT function is tremendously important. It should accurately
;;; reflect the winning probability at each tile in the game. Factors to
;;; consider are:
;;;
;;; - The current number of gold mines.
;;;
;;; - The chances of capturing new gold mines. This is the ability to reach
;;;   gold mines with a health of more than 20.
;;;
;;; - The chances of being slain by an opponent. This is the case when for
;;;   each tavern, there is an enemy closer to this tavern. Note that being
;;;   slain is only as bad as the number of gold mines lost.
;;;
;;; - The chances of slaying an opponent. These chances are computed in a
;;;   similar manner. The chance arises when we are closer to a tavern than
;;;   a (preferably rich) enemy, when all other taverns are blocked by
;;;   other opponents and when we get closer to this enemy.
;;;
;;; - The proximity to a tavern.

(defun playout (game)
  (flet ((individual-playout (player-number)
           (let ((hero (game-hero game player-number)))
             (tanh
              (+
               (- (count player-number (game-mine-owners game))
                  (count player-number (game-mine-owners *origin-game*)))
               (* (hero-life hero) 0.03))))))
    (make-array 4 :element-type 'single-float
                  :initial-contents (list (individual-playout 1)
                                          (individual-playout 2)
                                          (individual-playout 3)
                                          (individual-playout 4)))))

;;; This function has huge impact on the depth of the search
;;; tree. Consequentially, we should try to keep the number of untried
;;; moves small.
(defun untried-moves (game)
  (if (= (game-active-id game)
         (game-player-id game))
      ;; Sir Stencilbot's moves
      (sir-stencilbot-moves game)
      ;; Other player's moves
      (list (random-elt (game-possible-moves game)))))

(defun sir-stencilbot-moves (game)
  (let* ((player-hero (game-player-hero game))
         (enemy-distance
           (loop for hero in (game-heroes game)
                 unless (eq hero player-hero)
                   minimize (hero-distance game player-hero hero))))
    (case enemy-distance
      ;; potentially hit the enemy by staying
      (1 (game-possible-moves game))
      ;; potentially hit the enemy by moving towards him
      (2 (remove :stay (game-possible-moves game)))
      ;; try to cover a large search space by making only moves that
      ;; increase distance from the origin.
      (otherwise
       (union (progressive-moves game player-hero)
              (list (random-elt (game-possible-moves game))))))))

(defun opponent-moves (game)
  (if (> 3 (hero-distance
            game
            (game-active-hero game)
            (game-player-hero game)))
      (game-possible-moves game)
      (list (random-elt (game-possible-moves game)))))

;;; The set of sane moves that strictly increases the distance from the
;;; origin.
(defun progressive-moves (game hero)
  (let* ((x (hero-x hero))
         (y (hero-y hero))
         (old-distance (aref *origin-map* x y))
         (progressive-moves '()))
    (loop for move in (game-possible-moves game)
          ;; staying is never progressive
          unless (eq move :stay) do
            (multiple-value-bind (new-x new-y)
                (case move
                  (:north (values (1- x) y))
                  (:south (values (1+ x) y))
                  (:east  (values x (1+ y)))
                  (:west  (values x (1- y))))
              (case (game-tile game new-x new-y)
                (:air
                 (when (> (aref *origin-map* new-x new-y) old-distance)
                   (push move progressive-moves)))
                (:tavern
                 (when (< (hero-life hero) 81)
                   (push move progressive-moves)))
                (:mine
                 (push move progressive-moves))
                (:wall
                 (values)))))
    progressive-moves))

(defun hero-distance (game hero-1 hero-2)
  (aref (hero-distance-map game hero-1)
        (hero-x hero-2)
        (hero-y hero-2)))

(defun distance-map (game position)
  (ensure-gethash
   position *distance-maps*
   (compute-distance-map game position)))

(defun hero-distance-map (game hero)
  (let ((position (cons (hero-x hero) (hero-y hero))))
    (distance-map game position)))

(defun compute-tavern-distance-maps (game)
  (compute-distance-maps game (game-tavern-positions game)))

(defun compute-mine-distance-maps (game)
  (compute-distance-maps game (game-tavern-positions game)))

(defun compute-hero-distance-maps (game)
  (compute-distance-maps game (game-hero-positions game)))

(defun compute-distance-maps (game positions)
  (map 'vector
       (lambda (position)
         (distance-map game position))
       positions))
