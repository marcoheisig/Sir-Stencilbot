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
             :visits 1
             :untried-moves (game-possible-moves state)
             :gains (mcts-simulate state))))
      (prog1 node
        (unless (null parent)
          (pushnew node (mcts-node-children parent))
          (removef (mcts-node-untried-moves parent) move))))))

;;; Return a good move as determined by MCTS.
(defun mcts-search (state &key timeout metric)
  (let ((root (make-mcts-node nil state))
        (t0 (get-internal-real-time))
        (time-units (* timeout internal-time-units-per-second))
        (node-counter 0))
    (loop while (< (- (get-internal-real-time) t0) time-units) do
      (let* ((new-node (mcts-select root))
             (gains (funcall metric (mcts-simulate (mcts-node-state new-node)))))
        (mcts-backpropagate new-node gains)
        (incf node-counter)))
    (printf "Searched ~D nodes.~%" node-counter)
    (mcts-node-move
     (first
      (sort
       (mcts-node-children root)
       #'> :key #'mcts-node-visits)))))

(defun mcts-select (node)
  (if (not (null (mcts-node-untried-moves node)))
      (let ((move (random-elt (mcts-node-untried-moves node))))
        (make-mcts-node node move))
      ;; Use UCT to choose a child node
      (mcts-select
       (let ((max-gain -1.0)
             (max-node nil))
         (loop for child-node in (mcts-node-children node) do
           (let* ((parent-visits (mcts-node-visits node))
                  (child-gain (aref (mcts-node-gains child-node)
                                    (game-active-hero
                                     (mcts-node-state child-node))))
                  (child-visits (mcts-node-visits child-node))
                  (gain (+ (/ child-gain child-visits)
                           (sqrt (/ (* 2 (log parent-visits))
                                    child-visits)))))
             (when (> gain max-gain)
               (setf gain max-gain)
               (setf max-node child-node))))
         max-node))))

(defun mcts-simulate (state)
  (loop repeat 15 do
    (setf state (advance-game state (random-possible-move state)))
    (setf state (advance-game state :stay))
    (setf state (advance-game state :stay))
    (setf state (advance-game state :stay)))
  state)

(defun mcts-backpropagate (node gains)
  (unless (not node)
    (incf (mcts-node-visits node))
    (loop for index below 5 do
      (incf (aref (mcts-node-gains node) index)
            (aref gains index)))
    (mcts-backpropagate (mcts-node-parent node) gains)))

(defun random-possible-move (state)
  (random-elt (game-possible-moves state)))
