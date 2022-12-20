(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################
        (newarray-exp (exp1 exp2)
               (let ((length (expval->num (value-of exp1 env)))) ;length as num to send helper
                 (let ((val (value-of exp2 env)))
                   (begin
                     (newref (value-of exp1 env))    ;add length at index -1 on store
                   (let ((ref (newref val)))         ;initialize first element of array and store its array to return
                     (begin
                       (newarray-helper (- length 1) val)
                       (array-val ref)))))))
        
        (read-array-exp (exp1 exp2)
             (let ((array-ref (expval->array (value-of exp1 env))))
               (let ((index (expval->num (value-of exp2 env))))
                 (let ((return-element (search array-ref index))) ; search is helper procedure
                   return-element))))

        (update-array-exp (exp1 exp2 exp3)
             (let ((array-ref (expval->array (value-of exp1 env))))
               (let ((index (expval->num (value-of exp2 env))))
                 (let ((val (value-of exp3 env)))
                     (setref! (+ array-ref index) val))))) ;to reach index, sum arr-ref and index. Then change it by setref!
                   
        (length-array-exp (exp1)
              (let ((array-ref (expval->array (value-of exp1 env))))
                (search array-ref -1)))   ;length is stored at index -1, just before the array on store

        (swap-array-exp (exp1 exp2 exp3)
              (let ((array-ref (expval->array (value-of exp1 env))))
                (let ((index1 (expval->num (value-of exp2 env))))
                  (let ((element1 (search array-ref index1)))
                    (let ((index2 (expval->num (value-of exp3 env))))
                      (let ((element2 (search array-ref index2)))
                        (begin
                          (setref! (+ array-ref index1) element2)       ;swap them by using setref!
                          (setref! (+ array-ref index2) element1))))))))

        (copy-array-exp (exp1)
              (let ((array-ref (expval->array (value-of exp1 env))))
                (let ((array-length (expval->num (search array-ref -1))))      ;extract array length to copy it
                  (let ((copy-array-ref (+ (newref (search array-ref -1)) 1))) ;send length to store and assign next ref to the copied array reference to return
                    (begin
                      (copy-array-helper array-ref array-length) 
                      (array-val copy-array-ref))))))

        (newqueue-exp ()
           (let ((length 1003)) ;implicit array size is 1000. Plus three locations to keep track of top/end indexes.
                   (begin
                   (let ((ref (newref (num-val 0))))
                     (begin
                       (newarray-helper (- length 1) (num-val 0))
                       (setref! ref (num-val 2))                  ;top index
                       (setref! (+ ref 1) (num-val 2))            ;end index
                       (array-val ref))))))

        (enqueue-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val2))
                  (ref (expval->array val1))) 
              (let ((end-index (expval->num(deref (+ ref 1))))
                    (top-index (expval->num(deref ref))))
                (begin
                  (setref! (+ ref end-index) val2)) ; putting the new value to the end.
                  (setref! (+ ref 1) (num-val (+ end-index 1))) ; updating the end-index
                ))))  

        (dequeue-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((ref (expval->array val1)))
                   (let ((top-index (expval->num(deref ref)))           ; reference to top
                        (end-index (expval->num(deref (+ ref 1)))))     ; reference to end
                     (if (not (= top-index end-index))
                         (begin
                           (setref! ref (num-val (+ top-index 1)))
                           (deref (- (+ ref top-index) 1)))
                         (num-val -1))
                     ))))

        (queue-size-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((ref (expval->array val1))) 
              (let ((end-index (expval->num(deref (+ ref 1))))
                    (top-index (expval->num(deref ref))))
                (num-val (- end-index top-index))
                ))))

        (peek-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((ref (expval->array val1)))
                   (let ((top-index (expval->num(deref ref)))) 
                     (deref (+ ref top-index))
                     ))))
        
        (empty-queue?-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((ref (expval->array val1)))
                   (let ((top-index (expval->num(deref ref)))                    ; reference to top
                        (end-index (expval->num(deref (+ ref 1)))))              ; reference to end
                     (if (= end-index top-index) (bool-val #t) (bool-val #f))
                     ))))

        (print-queue-exp (exp1)
                (let ((val1 (value-of exp1 env)))
                 (let ((ref (expval->array val1)))
                   (let ((top-index (expval->num(deref ref)))                     ; reference to top
                        (end-index (expval->num(deref (+ ref 1)))))               ; reference to end
                     (printqueue-helper ref top-index end-index)))))

        (map-exp (var body arr-exp)
                 (let ((array-ref (expval->array (value-of arr-exp env))))
                   (let ((proc (procedure var body env)))                         ;transform parameters var and body to procedure
                     (let ((array-length (expval->num (search array-ref -1))))    ;extract array-length
                       (begin 
                         (map-helper array-ref array-length proc)                 ;send them to helper procedure
                         (array-val array-ref))))))
                   
        ; #####################################################
        )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE

  (define newarray-helper
    (lambda (length val)
      (if (not (= length 0))
          (begin 
            (newref val)
            (newarray-helper (- length 1) val))  ;recursively creates new array ref by ref
          (num-val 999))))                       ;return dummy value 

  (define search
    (lambda (array-ref index)
      (deref (+ array-ref index))))

  (define copy-array-helper
    (lambda (array-ref array-length)
      (if (not (= array-length 0))
          (begin
            (newref (deref array-ref))                                ;copying process
            (copy-array-helper (+ array-ref 1) (- array-length 1)))
          (num-val 999))))                                            ;return dummy value
  
  (define printqueue-helper
    (lambda (queue-ref top-index end-index)
      (if (= top-index end-index)
          (display "")
          (begin
            (display (expval->num(deref (+ queue-ref top-index))))
            (display " ")
            (printqueue-helper queue-ref (+ top-index 1) end-index)))))

  (define map-helper
    (lambda (array-ref length proc)
      (if (not (= length 0))                                             
          (begin
            (let ((result (apply-procedure proc (deref array-ref))))      ;apply procedure on the value stored in array at that index
              (setref! array-ref result))                                 ;change value at that index(array-ref) with value of result coming from procedure.
            (map-helper (+ array-ref 1) (- length 1) proc))               ;recursively go over on array by incrementing array-ref for keep track of index 
          (num-val 999))))                                                ;and decrement length, think that as counter for recursion. return dummy value.
      
      
      
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
