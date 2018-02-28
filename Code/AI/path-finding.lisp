(in-package :vindinium)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Distance Maps
;;;
;;; The distance map of a particular POSITION is an array of the same size
;;; and shape as the game board, containing for each tile the distance of
;;; the shortest path from POSITION, or -1 if the tile cannot be reached at
;;; all.

(defun compute-distance-map (game position)
  (let* ((distance-map (make-array (array-dimensions (game-board game))
                                   :element-type 'fixnum
                                   :initial-element -1)))
    (let ((new-worklist '())
          (worklist (list position)))
      (loop until (null worklist)
            for distance from 0 do
              (loop for (x . y) in worklist do
                ;; a value /= -1 means we have already been here
                (when (= -1 (aref distance-map x y))
                  (setf (aref distance-map x y) distance)
                  ;; non-interior neighbors form the new worklist
                  (flet ((probe-neighbor (x y)
                           (unless (eq :wall (game-tile game x y))
                             (push (cons x y) new-worklist))))
                    (probe-neighbor (1+ x) y)
                    (probe-neighbor (1- x) y)
                    (probe-neighbor x (1+ y))
                    (probe-neighbor x (1- y)))))
              (shiftf worklist new-worklist nil))
      distance-map)))

(defun distance (distance-map x y)
  (if (array-in-bounds-p distance-map x y)
      (let ((value (aref distance-map x y)))
        (if (= -1 value)
            most-positive-fixnum
            value))
      most-positive-fixnum))

(defun manhattan-distance (hero-1 hero-2)
  (max (abs (- (hero-x hero-1) (hero-x hero-2)))
       (abs (- (hero-y hero-1) (hero-y hero-2)))))

(defun euclidean-distance (hero-1 hero-2)
  (sqrt (+ (expt (- (hero-x hero-1) (hero-x hero-2)) 2)
           (expt (- (hero-y hero-1) (hero-y hero-2)) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Movesets - a specialized data type for Vindinium moves
;;;
;;; Because storage and manipulations of sets of moves are so frequent, we
;;; provide two representations. The external one is as a list of keywords,
;;; while the internal, fast and efficient one is as a 5-bit integer.

(deftype moveset ()
  '(unsigned-byte 5))

(defconstant +empty-moveset+ 0)

(let ((moveset-alist
        '((:north . #b00001)
          (:east  . #b00010)
          (:south . #b00100)
          (:west  . #b01000)
          (:stay  . #b10000))))

  (defun compute-moveset-moves (moveset)
    (declare (moveset moveset))
    (loop for (move . mask) in moveset-alist
          when (plusp (logand moveset mask))
            collect move))

  (defmacro define-moveset-constructor ()
    `(progn
       (declaim (inline moveset))
       (defun moveset (move)
         (ecase move
           ,@(loop for (key . mask) in moveset-alist
                   collect `(,key ,mask)))))))

(define-moveset-constructor)

(defun moveset-to-moves (moveset)
  (declare (moveset moveset))
  (let ((table
          (load-time-value
           (apply #'vector (mapcar #'compute-moveset-moves (iota 32)))
           t)))
    (aref table moveset)))

(defun moveset-from-moves (moves)
  (let ((moveset +empty-moveset+))
    (declare (moveset moveset))
    (loop for move in moves do
      (setf moveset (moveset-union moveset (moveset move))))))

(declaim (inline moveset-union
                 moveset-intersection
                 moveset-difference))

(defun moveset-union (a b)
  (declare (moveset a b))
  (logior a b))

(defun moveset-difference (a b)
  (declare (moveset a b))
  (logandc2 a b))

(defun moveset-intersection (a b)
  (declare (moveset a b))
  (logand a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Path maps
;;;
;;; A path map is an array of move sets with the same size and shape as the
;;; game board, containing all shortest paths from one position in the game
;;; to a supplied list of other positions.

(defun make-empty-path-map (game)
  (make-array (array-dimensions (game-board game))
              :element-type 'moveset
              :initial-element +empty-moveset+))

(defun compute-path-map (game position other-positions)
  (let ((distance-map (compute-distance-map game position))
        (path-map (make-array (array-dimensions (game-board game))
                              :element-type 'moveset
                              :initial-element +empty-moveset+)))
    (draw-path-map distance-map path-map other-positions)))

(defun draw-path-map (distance-map path-map positions)
  (declare (type (simple-array fixnum (* *)) distance-map)
           (type (simple-array moveset (* *)) path-map)
           (optimize speed))
  (let ((max (1- (array-dimension distance-map 0))))
    (labels ((draw-path (x y)
               ;; Starting from TARGET, descend the distance map until it
               ;; is zero. Since we walk backward, moves must be drawn in
               ;; the opposite direction
               (declare (non-negative-fixnum x y))
               (let ((distance (aref distance-map x y)))
                 ;; If distance is zero, we are done.
                 (unless (zerop distance)
                   (or (and (/= x max)
                            (probe-neighbor (1+ x) y (moveset :north) distance))
                       (and (/= x 0)
                            (probe-neighbor (1- x) y (moveset :south) distance))
                       (and (/= y max)
                            (probe-neighbor x (1+ y) (moveset :west) distance))
                       (and (/= y 0)
                            (probe-neighbor x (1- y) (moveset :east) distance))))))
             (probe-neighbor (x y moveset distance)
               (declare (non-negative-fixnum x y distance)
                        (moveset moveset))
               (let ((new-distance (aref distance-map x y)))
                 (when (and (/= -1 new-distance)
                            (< new-distance distance))
                   (setf (aref path-map x y)
                         (moveset-union (aref path-map x y) moveset))
                   (draw-path x y)))))
      (loop for (x . y) in positions do (draw-path x y))
      path-map)))
