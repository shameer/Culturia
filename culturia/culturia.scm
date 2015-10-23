(define-module (culturia))


(use-modules (cli))
(use-modules (conceptnet))


(define culturia (command "culturia.scm" "Command line to execute primitive tasks"
                          (command "dataset" "Load dataset"
                                   conceptnet-command)))


(program-execute (program-arguments) culturia)
