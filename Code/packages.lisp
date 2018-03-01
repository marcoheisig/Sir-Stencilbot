(defpackage :vindinium
  (:use :cl :alexandria :trivia)
  (:export
   #:make-timer
   #:printf

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
   #:game-play-url
   #:game-board
   #:game-max-turns
   #:game-mine-positions
   #:game-tavern-positions
   #:game-turn
   #:game-active-id
   #:game-mine-owners
   #:game-hero
   #:game-heroes
   #:game-hero-positions
   #:game-active-hero
   #:game-player-hero
   #:game-other-heroes
   #:game-tile
   #:game-possible-moves
   #:game-finished-p
   #:game-simulate

   #:new-game
   #:game-send-turn

   #:mcts-search

   #:compute-distance-map
   #:compute-path-map
   #:make-empty-path-map
   #:draw-path
   #:moveset
   #:moveset-union
   #:moveset-difference
   #:moveset-intersection
   #:moveset-from-moves
   #:moveset-to-moves
   #:distance
   #:manhattan-distance
   #:euclidean-distance))

(defpackage :vindinium/sir-stencilbot
  (:use :cl :alexandria :trivia :vindinium)
  (:export #:sir-stencilbot))

