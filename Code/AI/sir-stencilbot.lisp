(in-package :vindinium/sir-stencilbot)

(defvar *tavern-distance-maps* nil)

(defun sir-stencilbot (&key (training t)
                         (server "http://vindinium.walberla.net")
                         (secret-key "hujgkdbt"))
  (let* ((url (concatenate 'string server "/api/" (if training "training" "arena")))
         (game (new-game url secret-key))
         (timer (make-timer))
         (*tavern-distance-maps* (compute-tavern-distance-maps game)))
    (loop until (game-finished-p game)
          for next-move
            = (mcts-search
               game
               :termination-fn (lambda () (< 0.4 (funcall timer)))
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
(defun playout (game)
  (let ((total-mines (length (game-mine-owners game))))
    (flet
        ((individual-playout (player-number)
           (let ((owned-mines (count player-number (game-mine-owners game)))
                 (hero (game-hero game player-number))
                 (greed 1.0)
                 (caution 1.0)
                 (anger 1.0))
             (+ (* (capture-chance game hero) greed)
                (* (death-chance game hero) owned-mines caution)
                (* (slay-chance game hero) anger)))))
      (make-array 4 :element-type 'single-float
                    :initial-contents (list (individual-playout 1)
                                            (individual-playout 2)
                                            (individual-playout 3)
                                            (individual-playout 4))))))

(defun capture-chance (game hero)
  1.0)

(defun death-chance (game hero)
  (let ((other-heroes (remove hero (game-heroes game))))
    (if (loop for tavern-distance-map across *tavern-distance-maps*
                theris
                (< (aref tavern-distance-map
                         (hero-x hero)
                         (hero-y hero))
                   (loop for other-hero in other-heroes
                         minimize (aref tavern-distance-map
                                        (hero-x other-hero)
                                        (hero-y other-hero)))))
        ;; we have a safe place to withdraw to
        0.0
        ;; we are endangered
        -1.0)))

(defun slay-chance (game hero)
  0.0
  #+nil
  (let ((other-heroes (remove hero (game-heroes game))))
    (loop for other-hero in other-heroes
            thereis
            (loop for tavern-distance-map across *tavern-distance-maps*
                  always
                    (< (aref tavern-distance-map
                             (hero-x hero)
                             (hero-y hero))
                       (loop for other-hero in other-heroes
                             minimize (aref tavern-distance-map
                                            (hero-x other-hero)
                                            (hero-y other-hero)))))
            (if 
                ;; we have a safe place to withdraw to
                0.0
                ;; we are endangered
                -1.0))))

;;; This function has tremendous impact on the depth of the search
;;; tree. Consequentially, we should try to keep the number of untried
;;; moves small.
(defun untried-moves (game)
  (game-possible-moves game))

(defun compute-tavern-distance-maps (game)
  (compute-distance-maps game (game-tavern-positions game)))

(defun compute-mine-distance-maps (game)
  (compute-distance-maps game (game-tavern-positions game)))

(defun compute-hero-distance-maps (game)
  (compute-distance-maps game (game-hero-positions game)))

(defun compute-distance-maps (game positions)
  (map 'vector
       (lambda (position)
         (compute-distance-map game position))
       positions))

;;; Return a path map that contains all shortest paths from the hero to
;;; interesting entities and between these entities.
(defun compute-hero-path-map (game)
  (let ((player-id (game-player-id game))
        (path-map (make-empty-path-map game))
        (interesting-entities '())
        (interesting-distance-maps '()))
    ;; when health is low, taverns are interesting
    (when (< (hero-life (game-player-hero game)) 61)
      (loop for tavern across (game-tavern-positions game)
            for distance-map across *tavern-distance-maps* do
              (push tavern interesting-entities)
              (push distance-map interesting-distance-maps)))
    ;; given enough health, all unoccupied mines are interesting
    (when (> (hero-life (game-player-hero game)) 21)
      (loop for mine across (game-mine-positions game)
            for owner across (game-mine-owners game)
            for distance-map across *mine-distance-maps*
            when (/= owner player-id) do
              (push mine interesting-entities)
              (push distance-map interesting-distance-maps)))
    ;; connect all interesting entities
    (loop for entity in interesting-entities
          for distance-map in interesting-distance-maps do
            (loop for other-entity in interesting-entities
                  when (not (eq other-entity entity)) do
                    (draw-path distance-map path-map other-entity)))
    ;; lead the player to these entities
    (loop for entity in interesting-entities do
      (draw-path (aref *hero-distance-maps* (1- player-id))
                 path-map entity))
    (loop for entity in (game-hero-positions game) do
      (draw-path (aref *hero-distance-maps* (1- player-id))
                 path-map entity))
    path-map))
