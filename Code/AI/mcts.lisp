(in-package :vindinium)

;;; Given a game, return a list of the untried moves of the current player.
(defvar *mcts-untried-moves-fn* nil)

;;; Given a game and a valid move, execute the move and return the
;;; resulting game
(defvar *mcts-move-fn* nil)

;;; Given a game, return a vector of weights -- one for each player. Each
;;; weight should be a (single-float 0.0 1.0).
(defvar *mcts-playout-fn* nil)

;;; Given a game, return the id of the active player. Used to index the
;;; previously returned weights
(defvar *mcts-active-player-fn* nil)

(defun mcts-search (game &key time-budget
                           playout-fn untried-moves-fn move-fn active-player-fn)
  (let ((*mcts-untried-moves-fn* untried-moves-fn)
        (*mcts-move-fn* move-fn)
        (*mcts-playout-fn* playout-fn)
        (*mcts-active-player-fn* active-player-fn))
    (let ((root (make-mcts-root-node game))
          (node-counter 0)
          (t-max (+ (get-internal-real-time) (* time-budget internal-time-units-per-second))))
      (loop while (< (get-internal-real-time) t-max) do
        (mcts-add-node root)
        (incf node-counter))
      (printf "Searched ~D nodes.~%" node-counter)
      (printf "Tree depth: ~D.~%" (tree-depth root))
      (printf "Heat map: ~A.~%" (heatmap root))
      (printf "Weights: (~{(:~A . ~,5F)~^ ~})~%"
              (loop for child in (mcts-node-children root)
                    collect (mcts-node-move child)
                    collect (mcts-quality child)))
      (mcts-node-move
       (first
        (sort
         (mcts-node-children root)
         #'> :key #'mcts-node-visits))))))

(defstruct (mcts-node
            (:constructor %make-mcts-node))
  (parent   nil                         :read-only t)
  (game     nil :type game              :read-only t)
  (move     nil :type (or null keyword) :read-only t)
  (children nil :type list)
  (weights  nil :type (or null (simple-array single-float (*))))
  (visits   nil :type non-negative-fixnum)
  (untried-moves nil :type list)
  ;; quality caches the most recent result of *MCTS-QUALITY-FN*. It is set
  ;; to NIL during backpropagation.
  (quality  nil :type (or null single-float)))

(defun make-mcts-root-node (game)
  (%make-mcts-node
   :parent nil
   :game game
   :move nil
   :children nil
   :visits 0
   :untried-moves (funcall *mcts-untried-moves-fn* game)
   :weights nil))

(declaim (inline mcts-root-node-p))
(defun mcts-root-node-p (node)
  (not (mcts-node-parent node)))

(defun make-mcts-node (parent move)
  (removef (mcts-node-untried-moves parent) move)
  (let* ((game (funcall *mcts-move-fn* (mcts-node-game parent) move))
         (weights (funcall *mcts-playout-fn* game))
         (untried-moves (funcall *mcts-untried-moves-fn* game))
         (child (%make-mcts-node
                :parent parent
                :game game
                :move move
                :children nil
                :weights weights
                :visits 1
                :untried-moves untried-moves)))
    (prog1 child
      (push child (mcts-node-children parent))
      (mcts-backpropagate parent weights))))

(defun mcts-quality (node)
  (or (mcts-node-quality node)
      (let* ((parent (mcts-node-parent node))
             (child node)
             (id (funcall *mcts-active-player-fn* (mcts-node-game parent))))
        (let ((child-quality (aref (mcts-node-weights child) id))
              (child-visits (coerce (mcts-node-visits child) 'single-float))
              (parent-visits (coerce (mcts-node-visits parent) 'single-float)))
          (setf (mcts-node-quality node)
                (+ (/ child-quality child-visits)
                   (* 0.2
                      (sqrt (/ (* 2.0 (log parent-visits 2))
                               child-visits)))))))))

(defun mcts-add-node (node)
  (let ((untried-moves (mcts-node-untried-moves node)))
    (cond
      ((null untried-moves)
       (mcts-add-node
        (first
         (sort (mcts-node-children node) #'> :key #'mcts-quality))))
      ((= 1 (length untried-moves))
       (make-mcts-node node (first untried-moves)))
      (t
       (make-mcts-node node (random-elt untried-moves))))))

(defun mcts-backpropagate (node weights)
  (declare (type mcts-node node)
           (type (simple-array single-float (*)) weights)
           (optimize (speed 3) (safety 0)))
  (incf (mcts-node-visits node))
  (unless (mcts-root-node-p node)
    (setf (mcts-node-quality node) nil)
    (loop for q across weights
          for index from 0 do
            (incf (aref (mcts-node-weights node) index) q))
    (mcts-backpropagate (mcts-node-parent node) weights)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; debugging utilities

(defun tree-depth (node)
  (if (null (mcts-node-children node))
      1
      (1+ (loop for child in (mcts-node-children node)
                maximize (tree-depth child)))))

(defun map-tree (node function)
  (funcall function node)
  (loop for child in (mcts-node-children node) do
    (map-tree child function)))

(defun heatmap (root)
  (let ((heatmap (make-array (array-dimensions (game-board (mcts-node-game root)))
                             :initial-element 0
                             :element-type '(unsigned-byte 64))))
    (flet ((heat-fn (node)
             (let* ((game (mcts-node-game node))
                    (hero (game-active-hero game)))
               (when (= (hero-id hero) (game-player-id game))
                 (incf (aref heatmap (hero-x hero) (hero-y hero)))))))
      (map-tree root #'heat-fn))
    heatmap))
