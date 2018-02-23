(in-package :sir-stencilbot)

(defun |Sir Stencilbot| (game)
  (let* ((player-id (game-player-id game))
         (other-heroes (remove player-id (list 1 2 3 4)))
         (ignorable-heroes
           (loop for hero-id in other-heroes
                 when (> (distance (game-hero game hero-id)
                                   (game-hero game player-id))
                         5.0)
                 collect hero-id)))
    (mcts-search game :timeout 0.6
                      :metric #'sir-stencilbot-metric
                      :ignorable-heroes ignorable-heroes)))

(defun distance (hero-1 hero-2)
  (sqrt (+ (expt (- (hero-x hero-1) (hero-x hero-2)) 2)
           (expt (- (hero-y hero-1) (hero-y hero-2)) 2))))

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
                          (* 2.0 (max 0 (- new-gold-mines old-gold-mines)))
                          ;; health is important
                          (* 0.05 (- (hero-life old-hero)
                                     (hero-life new-hero))))
                       0.0))))))))
