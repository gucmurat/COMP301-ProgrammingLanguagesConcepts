(module translator (lib "eopl.ss" "eopl")
  
  (require "lang.scm")

  (provide translation-of-program)
  ;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

  ;; translation-of-program : Program -> Nameless-program
  ;; Page: 96
  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (a-program                    
            (translation-of exp1 (init-senv)))))))

  ;;counter
  (define counter 0)

  ;; translation-of : Exp * Senv -> Nameless-exp
  ;; Page 97
  (define translation-of
    (lambda (exp senv)
      
      (cases expression exp
        (const-exp (num) (const-exp num))
        (diff-exp (exp1 exp2)
          (diff-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)))
        (zero?-exp (exp1)
          (zero?-exp
            (translation-of exp1 senv)))
        (if-exp (exp1 exp2 exp3)
          (if-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)
            (translation-of exp3 senv)))
        (var-exp (var)
          (nameless-var-exp
            (apply-senv senv var)))
        
        (let-exp (var exp1 body)
        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### Here, you need to check whether var is in the
        ; ###### current environment or not. If it's in the 
        ; ###### environment, you will display a message that 
        ; ###### tells it is being reinitialized. Otherwise,
        ; ###### you will increment the counter and display
        ; ###### the counter at that point.
        ; ###### HINT: You can use the counter global variable
        ; ###### defined above and also you can use 1 additional
        ; ###### helper function.
        ; #####################################################
           
           ;;counter
           (if (includes? senv var)
               ((lambda (x) (display var)(display " has been reinitialized.\n")) 0)
               ((lambda (x) (begin (set! counter (+ counter 1)))
               (display "# of procedure calls: ")(display counter)
               (newline)) 0)
           )
           (nameless-let-exp
            (translation-of exp1 senv)            
            (translation-of body
              (extend-senv var senv)))
           

        ; #####################################################
         )
        (proc-exp (var body)
          (nameless-proc-exp
            (translation-of body
              (extend-senv var senv))))
        (call-exp (rator rand)
          (call-exp
            (translation-of rator senv)
            (translation-of rand senv)))
        (else (report-invalid-source-expression exp))
        )))

  (define includes?
    (lambda (senv var)
      (cond
        ((null? senv) #f)
        ((eqv? var (car senv)) #t)
        (else
          (includes? (cdr senv) var))
        )))

  (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
        "Illegal expression in source code: ~s" exp)))
  
   ;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;
  
  ;;; Senv = Listof(Sym)
  ;;; Lexaddr = N

  ;; empty-senv : () -> Senv
  ;; Page: 95
  (define empty-senv
    (lambda ()
      '()))

  ;; extend-senv : Var * Senv -> Senv
  ;; Page: 95
  (define extend-senv
    (lambda (var senv)
      (cons var senv)))
  
  ;; apply-senv : Senv * Var -> Lexaddr
  ;; Page: 95
  (define apply-senv
    (lambda (senv var)
      (cond
        ((null? senv) (report-unbound-var var))
        ((eqv? var (car senv))
         0)
        (else
          (+ 1 (apply-senv (cdr senv) var))))))

  (define report-unbound-var
    (lambda (var)
      (eopl:error 'translation-of "Unbound variable ~s" var)))

  ;; init-senv : () -> Senv

  (define init-senv
    (lambda ()
      (empty-senv)))
  
  )

  
