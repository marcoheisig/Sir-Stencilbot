(in-package :vindinium)

(defun new-game (url secret-key)
  (printf "Connecting...~%")
  (let ((game (parse-game
               (communicate url (cons "key" secret-key)))))
    (printf "View URL: ~A~%" (game-view-url game))
    game))

(defun game-send-turn (game secret-key next-move)
  (parse-game
   (communicate (game-play-url game)
                (cons "key" secret-key)
                (cons "dir" (string-capitalize next-move)))))

(defun communicate (url &rest params)
  (multiple-value-bind (body status)
      (let ((drakma:*text-content-types*
              (cons '("application" . "json") drakma:*text-content-types*)))
        (drakma:http-request url :method :post :parameters params))
    (if (= status 200)
        (jsown:parse body)
        (printf "HTTP ERROR: ~a~%~a" status body))))

(defun parse-game (json)
  (let ((game (jsown:val json "game"))
        (my-hero (jsown:val json "hero"))
        (view-url (jsown:val json "viewUrl"))
        (play-url (jsown:val json "playUrl")))
    (let ((game-id (jsown:val game "id"))
          (game-turn (jsown:val game "turn"))
          (game-max-turns (jsown:val game "maxTurns"))
          (game-board (jsown:val game "board"))
          (game-heroes (parse-heroes (jsown:val game "heroes")))
          (player-id (jsown:val my-hero "id")))
      (multiple-value-bind (board mine-owners mine-positions tavern-positions)
          (parse-board game-board)
        (make-game
         :static-state (make-game-static-state
                        :id game-id
                        :player-id player-id
                        :view-url view-url
                        :play-url play-url
                        :board board
                        :mine-positions mine-positions
                        :tavern-positions tavern-positions
                        :max-turns game-max-turns)
         :turn game-turn
         :active-id player-id
         :mine-owners mine-owners
         :hero-1 (elt game-heroes 0)
         :hero-2 (elt game-heroes 1)
         :hero-3 (elt game-heroes 2)
         :hero-4 (elt game-heroes 3))))))

(defun parse-board (board-tuple)
  (ematch board-tuple
    ((list _ (cons "size" n) (cons "tiles" board-string))
     (let ((board (make-array (list n n) :element-type 'game-tile :initial-element :air))
           (mine-owners '())
           (mine-positions '())
           (tavern-positions '()))
       (loop for iy below n do
         (loop for ix below n do
           (let ((string-index (* 2 (+ (* n ix) iy))))
             (multiple-value-bind (tile owner)
                 (parse-tile (aref board-string string-index)
                             (aref board-string (1+ string-index)))
               (case tile
                 (:mine
                  (push owner mine-owners)
                  (push (cons ix iy) mine-positions))
                 (:tavern
                  (push (cons ix iy) tavern-positions)))
               (setf (aref board ix iy) tile)))))
       (values
        board
        (make-array (length mine-owners)
                    :element-type '(unsigned-byte 4)
                    :initial-contents (nreverse mine-owners))
        (make-array (length mine-positions)
                    :element-type '(cons coordinate coordinate)
                    :initial-contents (nreverse mine-positions))
        (make-array (length tavern-positions)
                    :element-type '(cons coordinate coordinate)
                    :initial-contents (nreverse tavern-positions)))))))

(defun parse-tile (first second)
  (ematch (list first second)
    ((list #\  #\ ) (values :air nil))
    ((list #\# #\#) (values :wall nil))
    ((list #\[ #\]) (values :tavern nil))
    ((list #\$ #\-) (values :mine 0))
    ((list #\$ #\1) (values :mine 1))
    ((list #\$ #\2) (values :mine 2))
    ((list #\$ #\3) (values :mine 3))
    ((list #\$ #\4) (values :mine 4))
    ((list #\@ _) (values :air nil))))

(defun parse-heroes (heroes)
  (sort
   (mapcar #'parse-hero heroes) #'<
   :key #'hero-id))

(defun parse-hero (hero-json)
  (flet ((maybe-val (json key default)
           (or (ignore-errors (jsown:val json key))
               default)))
    (let ((id (jsown:val hero-json "id"))
          (name (jsown:val hero-json "name"))
          (user-id (maybe-val hero-json "userId" "none"))
          (elo (maybe-val hero-json "elo" 0))
          (pos (jsown:val hero-json "pos"))
          (spawn-pos (jsown:val hero-json "spawnPos"))
          (life (jsown:val hero-json "life"))
          (gold (jsown:val hero-json "gold")))
      (let ((x (jsown:val pos "x"))
            (y (jsown:val pos "y"))
            (spawn-x (jsown:val spawn-pos "x"))
            (spawn-y (jsown:val spawn-pos "y")))
        (make-hero
         :static-state (make-hero-static-state
                        :id id
                        :name name
                        :user-id user-id
                        :elo elo
                        :spawn-x spawn-x
                        :spawn-y spawn-y)
         :gold gold
         :life life
         :x x
         :y y)))))

;;; Check whether there exists a series of four turns that turn OLD-GAME
;;; into NEW-GAME. Signal an error if this is not possible.
(defun check-simulation (old-game new-game)
  (flet ((run-simulation (turns)
           (let ((result old-game))
             (loop for turn in turns do
               (setf result (game-simulate result turn)))
             result)))
    (block nil
      (map-product
       (lambda (&rest turns)
         (let ((simulation (run-simulation turns)))
           (when (equalp new-game simulation)
             (return))))
       #1='(:stay :north :south :east :west) #1# #1# #1#)
      (error "Cannot advance~%~S~%to~%~S.~%" old-game new-game))))
