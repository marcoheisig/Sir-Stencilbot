(defsystem "vindinium"
  :author "Marco Heisig"
  :license "AGPLv3"

  :depends-on ("alexandria"
               "drakma"
               "jsown"
               "clon"
               "trivia")

  :components ((:module "Code"
                :serial t
                :components ((:file "packages")
                             (:file "game")
                             (:file "simulation")
                             (:file "client")
                             (:module "AI"
                              :components ((:file "mcts")
                                           (:file "sir-stencilbot")))))))
