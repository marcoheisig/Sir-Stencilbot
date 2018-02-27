(in-package :vindinium)

(defun printf (fmt &rest args)
  (apply #'format t fmt args)
  (finish-output))

(defun make-timer ()
  (let ((t0 (get-internal-real-time)))
    (lambda ()
      (float
       (/ (- (get-internal-real-time) t0)
          internal-time-units-per-second)))))
