(in-package :sir-stencilbot)


(defun |Sir Stencilbot| (game)
  (mcts-search game
               :timeout 0.3
               :metric #'sir-stencilbot-metric))

(defun sir-stencilbot-metric (game)
  (let ((gains (make-array 5 :element-type 'non-negative-single-float
                             :initial-element 0.0))
        (heroes (sort (list (game-hero-1 state)
                            (game-hero-2 state)
                            (game-hero-3 state)
                            (game-hero-4 state))
                      #'> :key #'hero-gold)))
    (let ((top-gold (hero-gold (first players)))
          (second-most-gold (hero-gold (second heroes))))
      (loop for player in players
            maximize (hero-gold player) into max-gold
            minimize (hero-gold player) into min-gold
            finally
               (let ((avg-gold (/ (- max-gold min-gold) 2))
                     (var-gold (- max-gold avg-gold)))
                 (loop for player in players do
                   (setf (aref gains (hero-id player))
                         (/ ()))))))
    (prog1 gains
      (flet ((predicted-gold (hero)
               (+ (hero-gold hero)
                  (* 7 (count (hero-id hero) (game-mine-owners state))))))
        (setf (aref gains (hero-id (first players)))
              (let ((gold-difference
                      (- (predicted-gold (first players))
                         (predicted-gold (second players)))))
                (if (plusp gold-difference)
                    (min (+ 0.5 (/ gold-difference 10)) 1.0)
                    0.4)))
        (loop for player in (rest players) do
          (setf (aref gains (hero-id player))
                0.0))))))
