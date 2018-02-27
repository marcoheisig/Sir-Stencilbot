(in-package :vindinium/sir-stencilbot)

;; A path map connecting all taverns and mines with all other taverns and
;; mines.
(defvar *static-path-map* nil)

;; The path map for Sir Stencilbot. This is the only path map that is
;; recomputed each turn.
(defvar *hero-path-map* nil)

(defun sir-stencilbot (&key (training t)
                         (server "http://vindinium.walberla.net")
                         (secret-key "hujgkdbt"))
  (let* ((url (concatenate 'string server "/api/" (if training "training" "arena")))
         (game (new-game url secret-key))
         (timer (make-timer))
         (*static-path-map* (compute-static-path-map game))
         (*hero-path-map* nil))
    (loop until (game-finished-p game)
          for *hero-path-map* = (compute-hero-path-map game)
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
        (mines (make-array 4 :element-type 'non-negative-fixnum
                             :initial-element 0)))
    (declare (dynamic-extent mines))
    (loop for mine-owner across (game-mine-owners game) do
      (case mine-owner
        (1 (incf (aref mines 0)))
        (2 (incf (aref mines 1)))
        (3 (incf (aref mines 2)))
        (4 (incf (aref mines 3)))))
    (flet
        ((individual-playout (player-number)
           (let ((hero (game-hero game (1+ player-number))))
             (let ( ;; 25% of all gold mines is 0.0
                   ;; 50% of all gold mines is 1.0
                   (gold-score
                     (- (/ (float (aref mines player-number))
                           (* 0.25 (float total-mines)))
                        1.0))
                   ;; more than 100 health is 1.0
                   ;; 0 health is -1.0
                   (health-score
                     (- (/ (float (hero-life hero)) 50.0)
                        1.0)))
               (tanh (+ ;; caution
                      (* 0.05 health-score)
                      ;; greed
                      (* 1.0 gold-score)))))))
      (make-array  4 :element-type 'single-float
                     :initial-contents (list (individual-playout 0)
                                             (individual-playout 1)
                                             (individual-playout 2)
                                             (individual-playout 3))))))

(defun untried-moves (game)
  ;; This function has tremendous impact on the depth of the search
  ;; tree. Consequentially, we try hard to keep the number of untried moves
  ;; small.
  (if (= (game-active-id game) (game-player-id game))
      (let* ((player-id (game-player-id game))
             (hero (game-hero game player-id))
             (x (hero-x hero))
             (y (hero-y hero)))
        (or
         (moveset-to-moves
          (moveset-union (aref *hero-path-map* x y)
                         (aref *static-path-map* x y)))
         ;; the path maps may reach a dead end, in this case, stay put.
         '(:stay)))
      (list (random-elt (game-possible-moves game)))))

(defun compute-hero-path-map (game)
  (let* ((player-id (game-player-id game))
         (hero (game-player-hero game))
         (unoccupied-mines
           (loop for mine across (game-mine-positions game)
                 for owner across (game-mine-owners game)
                 when (/= owner player-id)
                   collect mine))
         (taverns (loop for tavern across (game-tavern-positions game)
                        collect tavern)))
    (compute-path-map game (cons (hero-x hero) (hero-y hero))
                      (append unoccupied-mines taverns))))

(defun compute-static-path-map (game)
  (let ((path-map (make-empty-path-map game))
        (entities (concatenate 'list
                               (game-tavern-positions game)
                               (game-mine-positions game))))
    (loop for entity in entities do
      (let ((distance-map (compute-distance-map game entity)))
        (draw-path-map distance-map path-map entities)))
    path-map))
