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
                             (:file "utilities")
                             (:file "game")
                             (:file "simulation")
                             (:file "path-finding")
                             (:file "mcts")
                             (:file "client")
                             (:module "AI"
                              :components ((:file "sir-stencilbot")))))))
