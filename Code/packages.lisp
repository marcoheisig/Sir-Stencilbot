(defpackage :vindinium
  (:use :cl :alexandria :trivia)
  (:export
   #:hero-id
   #:hero-name
   #:hero-elo
   #:hero-spawn-x
   #:hero-spawn-y
   #:hero-gold
   #:hero-life
   #:hero-x
   #:hero-y

   #:game-id
   #:game-player-id
   #:game-view-url
   #:game-board
   #:game-max-turns
   #:game-mine-positions
   #:game-tavern-positions
   #:game-turn
   #:game-active-id
   #:game-mine-owners
   #:game-hero
   #:game-tile
   #:game-possible-moves
   #:game-simulate

   #:mcts-search

   #:play-game))

(defpackage :vindinium/sir-stencilbot
  (:use :cl :alexandria :trivia :vindinium))

