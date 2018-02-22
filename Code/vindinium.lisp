(in-package :sir-stencilbot)

(deftype game-move ()
'(member :north :south :east :west :stay))

(deftype game-tile ()
'(member :air :wall :tavern :mine))

(deftype coordinate ()
'(unsigned-byte 8))

(deftype hero-health ()
'(integer 0 100))

(defmacro define-static-reader (struct-name slot-name)
(let ((reader-name (symbolicate struct-name '#:- slot-name)))
`(progn
    (declaim (inline ,reader-name))
    (defun ,reader-name (,struct-name)
        (declare (,struct-name ,struct-name))
        (,(symbolicate struct-name '#:-static-state- slot-name)
        (,(symbolicate struct-name '#:-static-state)
        ,struct-name))))))

(defstruct hero-static-state
(id      nil :type non-negative-fixnum :read-only t)
(token   nil :type string              :read-only t)
(name    nil :type string              :read-only t)
(user-id nil :type string              :read-only t)
(elo     nil :type non-negative-fixnum :read-only t)
(spawn-x nil :type coordinate          :read-only t)
(spawn-y nil :type coordinate          :read-only t))

(defstruct hero
(static-state nil :type hero-static-state)
(last-dir nil :type game-move)
(gold     nil :type non-negative-fixnum)
(health   nil :type hero-health)
(x        nil :type coordinate)
(y        nil :type coordinate))

(define-static-reader hero id)
(define-static-reader hero name)
(define-static-reader hero elo)
(define-static-reader hero spawn-x)
(define-static-reader hero spawn-y)

(defstruct game-static-state
(id           0   :type string                               :read-only t)
(training     nil :type boolean                              :read-only t)
(board        nil :type (simple-array game-tile (* *))       :read-only t)
(spawn-points nil :type (simple-array (unsigned-byte 8) (*)) :read-only t)
(max-turns    nil :type non-negative-fixnum                  :read-only t))

(defstruct game
(static-state nil :type game-static-state)
(turn   0   :type non-negative-fixnum)
(hero-1 nil :type hero)
(hero-2 nil :type hero)
(hero-3 nil :type hero)
(hero-4 nil :type hero))

(define-static-reader game id)
(define-static-reader game training)
(define-static-reader game board)
(define-static-reader game spawn-points)
(define-static-reader game max-turns)

