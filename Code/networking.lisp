(in-package :sir-stencilbot)

(defparameter *arena-server* "http://vindinium.walberla.net/api/arena")
(defparameter *secret-key* "x5mv46gq")

(defun play-game (bot-function)
  (let ((game (start-arena)))
    (loop until (game-finished-p game)
          do (setf game (advance-game game (funcall bot-function game))))))

(defun start-arena ()
  (let ((json (communicate *arena-server* (cons "key" secret-key))))))

(defun communicate (url &rest params)
  (multiple-value-bind (body status)
      (drakma:http-request url :method :post
                               :parameters params)
    (if (= status 200)
        (jsown:parse body)
        (format t "HTTP ERROR: ~a~%~a" status body))))
