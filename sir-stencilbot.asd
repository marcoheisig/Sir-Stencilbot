(defsystem "sir-stencilbot"
  :author "Marco Heisig"
  :license "AGPLv3"

  :depends-on ("alexandria"
               "drakma"
               "jsown"
               "cl-ppcre"
               "trivial-garbage"
               "trivia")

  :components ((:module "Code"
                :serial t
                :components ((:file "package")
                             (:file "vindinium")
                             (:file "simulation")
                             (:file "networking")
                             (:file "mcts")
                             (:module "Bots"
                              :components ((:file "sir-stencilbot")))))))
