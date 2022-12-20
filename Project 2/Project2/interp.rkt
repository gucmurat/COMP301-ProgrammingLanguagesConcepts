#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (zero?-exp (exp1)
        ;; -----------------------
        ;; INSERT YOUR CODE HERE 
        ;; -----------------------
           (let ((val1 (value-of exp1 env)))
                   (cases expval val1
                     (num-val (value1-num)
                        (let ((num1 (expval->num val1)))
                         (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f))))
                     (rational-val (value1-rat)
                         (let ((num1 (expval->rational val1)))
                         (if (zero? (car num1))
                         (bool-val #t)
                         (bool-val #f)) ))
                     (bool-val (value1-bool) (eopl:error 'invalid-type-bool-zero?))
                     (str-val (value1-str) (eopl:error 'invalid-type-str-zero?))))
       )

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      
      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------
      ;;implementation of str-exp
      (str-exp (str) (str-val str))

      ;;implementation of if-exp
      (if-exp (exp1 exp2 conds exps exp3)
            (if (null? conds)
                   (if (expval->bool (value-of exp1 env))
                       (value-of exp2 env)
                       (value-of exp3 env))
                   (if (expval->bool (value-of exp1 env))
                       (value-of exp2 env)
                       (value-of (if-exp (car conds) (car exps) (cdr conds) (cdr exps) exp3) env)))
      )

      ;;implementation of rational-exp
      (rational-exp (num1 num2)
                    (if (zero? num2)
                        (eopl:error 'error)
                        (rational-val (cons  num1  num2)))
       )
      ;;implementation of op-exp
      (op-exp (exp1 exp2 num)
              (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                 (cases expval val1 (num-val (value1-num) (cases expval val2
                  (num-val (value2-num)
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (cond ((equal? num 1) (num-val (+ num1 num2)))
                          ((equal? num 2) (num-val (* num1 num2)))
                          ((equal? num 3) (num-val (/ num1 num2)))
                          (else (num-val (- num1 num2))))))
                  (rational-val (value2-rat) (let ((num1 (expval->num val1))
                                                   (num2 (expval->rational val2)))
                      (cond ((equal? num 1) (rational-val (cons (+ (car num2) (* num1 (cdr num2))) (cdr num2))))
                               ((equal? num 2) (rational-val (cons (* (car num2) num1) (cdr num2))))
                               ((equal? num 3) (rational-val (cons (* num1 (cdr num2)) (car num2))))
                               (else (rational-val (cons (- (* num1 (cdr num2)) num1) (cdr num2)))))))
                   (bool-val (value2-bool) (eopl:error 'invalid-type-bool-exp2))
                   (str-val (value2-str) (eopl:error 'invalid-type-str-exp2))))
                                     (rational-val (value1-rat) (cases expval val2
                    (num-val (value2-num)
                    (let ((num1 (expval->rational val1))
                          (num2 (expval->num val2)))
                      (cond ((equal? num 1) (rational-val (cons (+ (car num1) (* num2 (cdr num1))) (cdr num1))))
                               ((equal? num 2) (rational-val (cons (* (car num1) num2) (cdr num1))))
                               ((equal? num 3) (rational-val (cons (car num1) (* num2 (cdr num1)))))
                               (else (rational-val (cons (- (car num1) (* num2 (cdr num1))) (cdr num1)))))))
                    (rational-val (value2-rat) (let ((num1 (expval->rational val1))
                         (num2 (expval->rational val2)))
                      (cond ((equal? num 1) (rational-val (cons (+ (* (car num1) (cdr num2)) (* (car num2) (cdr num1))) (* (cdr num1) (cdr num2)))))
                               ((equal? num 2) (rational-val (cons (* (car num1) (car num2)) (* (cdr num1)(cdr num2)))))
                               ((equal? num 3) (rational-val (cons (* (car num1) (cdr num2)) (* (cdr num1)(car num2)))))
                               (else (rational-val (cons (- (* (car num1) (cdr num2)) (* (car num2) (cdr num1))) (* (cdr num1) (cdr num2))))))))
                    (bool-val (value2-bool) (eopl:error 'invalid-type-bool-exp2))
                    (str-val (value2-str) (eopl:error 'invalid-type-str-exp2))))
                                       (bool-val (value1-bool) (eopl:error 'invalid-type-bool-exp1))
                                       (str-val (value1-str) (eopl:error 'invalid-type-str-exp1)))))
      
      ;; -----------------------

      )))