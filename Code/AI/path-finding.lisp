(in-package :vindinium)

(defun compute-distance-map (game pos)
  (let* ((board (game-board game))
         (n (array-dimension board 0))
         (path-map (make-array (list n n) :element-type 'fixnum
                                          :initial-element -1)))
    (let ((new-worklist '())
          (worklist (list pos)))
      (loop until (null worklist)
            for distance from 0 do
              (loop for (x . y) in worklist do
                ;; positive value -> we have already been here
                (when (minusp (aref path-map x y))
                  (setf (aref path-map x y) distance)
                  ;; non-interior neighbors form the new worklist
                  (flet ((probe-neighbor (x y)
                           (when (eq :air (game-tile game x y))
                             (push (cons x y) new-worklist))))
                    (probe-neighbor (1+ x) y)
                    (probe-neighbor (1- x) y)
                    (probe-neighbor x (1+ y))
                    (probe-neighbor x (1- y)))))
              (shiftf worklist new-worklist nil))
      path-map)))

(defun distance (distance-map x y)
  (if (array-in-bounds-p distance-map x y)
      (let ((value (aref distance-map x y)))
        (if (= -1 value)
            most-positive-fixnum
            value))
      most-positive-fixnum))
