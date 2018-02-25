(in-package :vindinium/sir-stencilbot)

(defun sir-stencilbot (game)
  (mcts-search
   game
   :time-budget 0.3
   :active-player-fn (lambda (game) (1- (game-active-id game)))
   :move-fn #'game-simulate
   :playout-fn #'playout
   :untried-moves-fn #'untried-moves))

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
               #+nil
               (when (= (hero-id hero) (game-player-id game))
                 (format t "health-score: ~,2F position-score ~,2F gold-score ~,2F~%"
                         health-score position-score gold-score))
               (tanh (+ ;; caution
                      (* 0.3 health-score)
                      ;; greed
                      (* 2.0 gold-score)))))))
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
      (remove :stay (game-possible-moves game))
      (list (random-elt (game-possible-moves game)))))
