(in-package :vindinium/sir-stencilbot)

(defun |Sir Stencilbot| (game)
  (mcts-search
   game
   :time-budget 0.4
   :active-player-fn (lambda (game) (1- (game-active-id game)))
   :move-fn #'game-simulate
   :playout-fn #'playout
   :untried-move-fn #'untried-moves))

(defun sir-stencilbot-metric (old-game new-game)
  (let ((gains (make-array 5 :element-type 'single-float
                             :initial-element 0.0)))
    (prog1 gains
      (loop for id from 1 to 4 do
        (let ((old-hero (game-hero old-game id))
              (new-hero (game-hero new-game id)))
          (let ((old-gold-mines (count id (game-mine-owners old-game)))
                (new-gold-mines (count id (game-mine-owners new-game))))
            (setf (aref gains id)
                  (max (+ 0.4
                          ;; winning mines is good
                          (* 0.4 (- new-gold-mines old-gold-mines))
                          ;; health is important
                          (if (< (hero-life old-hero) 72)
                              (min
                               0.3
                               (* 0.01 (- (hero-life new-hero)
                                          (hero-life old-hero))))
                            0.0))
                       0.0))))))))

(defun playout (game)
  )

(defun untried-moves (game)
  )
