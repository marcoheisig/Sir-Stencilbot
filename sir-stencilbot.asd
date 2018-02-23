(defsystem "sir-stencilbot"
  :author "Marco Heisig"
  :license "AGPLv3"

  :depends-on ("alexandria"
               "drakma"
               "jsown"
               "cl-ppcre"
               "trivia")

  :components ((:module "Code"
                :serial t
                :components ((:file "package")
                             (:file "vindinium")
                             (:file "networking")
                             (:file "mcts")))))
