(in-package :sir-stencilbot)

(defstruct (mcts-node
            (:constructor %make-mcts-node))
  (parent   nil            :read-only t)
  (state    nil :type game :read-only t)
  (move     nil :read-only t)
  (children nil :type list)
  (gains    nil :type (simple-array non-negative-single-float (5)))
  (visits   nil :type non-negative-fixnum)
  (untried-moves nil :type list))

(defun make-mcts-node (parent state-or-move)
  (multiple-value-bind (state move)
      (if (symbolp state-or-move)
          (values (advance-game (mcts-node-state parent) state-or-move) state-or-move)
          (values state-or-move nil))
    (let ((node
            (%make-mcts-node
             :parent parent
             :state state
             :move move
             :children nil
             :visits 0
             :untried-moves (game-possible-moves state)
             :gains (make-array 5 :element-type 'non-negative-single-float
                                  :initial-element 0.0))))
      (prog1 node
        (unless (null parent)
          (pushnew node (mcts-node-children parent))
          (removef (mcts-node-untried-moves parent) move))))))

;;; Return a good move as determined by MCTS.
(defun mcts-search (state &key metric ignorable-heroes)
  (let ((root (make-mcts-node nil state))
        (node-counter 0))
    (loop while (< (- (get-internal-real-time) *time-start*) *time-budget*) do
      (let* ((new-node (mcts-select root))
             (new-game (mcts-node-state new-node))
             (gains (funcall metric state (mcts-simulate new-game))))
        (mcts-backpropagate new-node gains)
        (incf node-counter)
        (loop while (find (game-active-hero new-game) ignorable-heroes) do
          (setf (game-active-hero new-game)
                (next-player (game-active-hero new-game))))))
    (printf "Searched ~D nodes.~%" node-counter)
    (printf "Ignoring Players ~A.~%" ignorable-heroes)
    (printf "Tree Depth ~D.~%" (mcts-tree-depth root))
    (printf "Move Quality: ~{~A: ~A~^ ~}~%"
            (loop for child in (mcts-node-children root)
                  collect (mcts-node-move child)
                  collect (mcts-node-visits child)))
    (mcts-node-move
     (first
      (sort
       (mcts-node-children root)
       #'> :key #'mcts-node-visits)))))

(defun mcts-tree-depth (node)
  (if (null (mcts-node-children node))
      0
      (1+ (apply #'max (mapcar #'mcts-tree-depth (mcts-node-children node))))))

(defun mcts-select (node)
  (declare (optimize (speed 3) (safety 0)))
  (if (not (null (mcts-node-untried-moves node)))
      (let ((move (random-elt (mcts-node-untried-moves node))))
        (make-mcts-node node move))
      ;; Use UCT to choose a child node
      (mcts-select
       (let ((max-gain most-negative-single-float)
             (max-node nil)
             (parent-visits (coerce (mcts-node-visits node) 'single-float)))
         (loop for child-node in (mcts-node-children node) do
           (let* ((child-gain (aref (mcts-node-gains child-node)
                                    (game-active-hero
                                     (mcts-node-state node))))
                  (child-visits (coerce (mcts-node-visits child-node) 'single-float))
                  (gain (+ (/ child-gain child-visits)
                           (* 2.0 ;; C
                              (the single-float
                                   (sqrt (/ (* 2.0 (log parent-visits 2.0))
                                            child-visits)))))))
             (when (> gain max-gain)
               (setf max-gain gain)
               (setf max-node child-node))))
         max-node))))

(defun mcts-simulate (game)
  (loop repeat 10 do
    (setf (game-active-hero game)
          (game-player-id game))
    (setf game
          (advance-game game (random-possible-move game))))
  game)

(defun mcts-backpropagate (node gains)
  (declare (type (or null mcts-node) node)
           (type (simple-array non-negative-single-float (5)) gains)
           (optimize (speed 3) (safety 0)))
  (unless (not node)
    (incf (mcts-node-visits node))
    (loop for index below 5 do
      (incf (aref (mcts-node-gains node) index)
            (aref gains index)))
    (mcts-backpropagate (mcts-node-parent node) gains)))

(defun random-possible-move (state)
  (random-elt (game-possible-moves state)))
