(module environments (lib "eopl.ss" "eopl") 
  
  (require "data-structures.scm")
  (provide init-nameless-env empty-nameless-env extend-nameless-env
           apply-nameless-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Nameless-env

  (define init-nameless-env
    (lambda ()
      (empty-nameless-env)))
  )