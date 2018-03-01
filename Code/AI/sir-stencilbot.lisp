(in-package :vindinium/sir-stencilbot)

(defvar *tavern-distance-maps* nil)

(defvar *mine-distance-maps* nil)

(defvar *hero-distance-maps* nil)

(defvar *hero-path-map* nil)

(defvar *game* nil)

(defun sir-stencilbot (&key (training t)
                         (server "http://vindinium.walberla.net")
                         (secret-key "hujgkdbt"))
  (let* ((url (concatenate 'string server "/api/" (if training "training" "arena")))
         (game (new-game url secret-key))
         (timer (make-timer))
         (*tavern-distance-maps* (compute-tavern-distance-maps game))
         (*mine-distance-maps* (compute-tavern-distance-maps game))
         (*hero-distance-maps* nil)
         (*hero-path-map* nil)
         (*game* nil))
    (loop until (game-finished-p game)
          for *hero-distance-maps* = (compute-hero-distance-maps game)
          for *hero-path-map* = (compute-hero-path-map game)
          for *game* = game
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

(defun playout (game)
  (let ((total-mines (length (game-mine-owners game)))
        (old-mines (make-array 4 :element-type 'non-negative-fixnum
                                 :initial-element 0))
        (new-mines (make-array 4 :element-type 'non-negative-fixnum
                                 :initial-element 0)))
    (declare (dynamic-extent old-mines new-mines))
    (flet ((compute-mines (game mines)
             (loop for mine-owner across (game-mine-owners game) do
               (case mine-owner
                 (1 (incf (aref mines 0)))
                 (2 (incf (aref mines 1)))
                 (3 (incf (aref mines 2)))
                 (4 (incf (aref mines 3)))))))
      (compute-mines *game* old-mines)
      (compute-mines game new-mines))
    (flet
        ((individual-playout (player-number)
           (let ((hero (game-hero game (1+ player-number))))
             (let ( ;; 25% of all gold mines is 0.0
                   ;; 50% of all gold mines is 1.0
                   (gold-score
                     (float (/ (- (aref new-mines player-number)
                                  (aref old-mines player-number))
                               total-mines)))
                   ;; more than 100 health is 1.0
                   ;; 0 health is -1.0
                   (health-score
                     (- (/ (float (hero-life hero)) 50.0)
                        1.0)))
               (tanh (+ ;; caution
                      (* 0.1 health-score)
                      ;; greed
                      (* 1.0 gold-score)))))))
      (make-array  4 :element-type 'single-float
                     :initial-contents (list (individual-playout 0)
                                             (individual-playout 1)
                                             (individual-playout 2)
                                             (individual-playout 3))))))

;;; This function has tremendous impact on the depth of the search
;;; tree. Consequentially, we try hard to keep the number of untried moves
;;; small.
(defun untried-moves (game)
  (if (= (game-active-id game) (game-player-id game))
      (let* ((player-id (game-player-id game))
             (hero (game-hero game player-id))
             (x (hero-x hero))
             (y (hero-y hero))
             (enemy-distance
               (loop for other-hero in (game-heroes game)
                     for distance-map across *hero-distance-maps*
                     unless (eq other-hero hero)
                       minimize (aref distance-map x y))))
        (cond
          ((= 1 enemy-distance)
           (game-possible-moves game))
          ((< enemy-distance 4)
           '(:north :south :west :east))
          (t (or (moveset-to-moves (aref *hero-path-map* x y))
                 ;; the path maps may reach a dead end, in this case, stay put.
                 '(:stay)))))
      (let* ((hero (game-active-hero game))
             (player-distance (aref (aref *hero-distance-maps* (1- (game-player-id game)))
                                    (hero-x hero)
                                    (hero-y hero)))
             (possible-moves (game-possible-moves game)))
        (if (< player-distance 5)
            possible-moves
            (list (random-elt possible-moves))))))

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
