(module project1 mzscheme
  ;;;;;;;;;;;; Comp 301 Project 1 ;;;;;;;;;;;;;;;;
  ;;; Add group members below
  ;;; Cem Kilinc 64044
  ;;; Sezen Zeynep Sumer 69338
  ;;; Alpay Sabuncuoğlu, asabuncuoglu13, 0011221
  ;;; Gül Sena Altıntaş, galtintas17, 0011222
  ;;; save your file in the format: p1_0011221_asabuncuoglu13_00112222_galtintas17.rkt
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  ;; PROJECT 1 Part A | Write your answer below here as a comment
  ;
  
  ;A)
  ; time-list ::= (seclist minlist hrlist)
  ; 0-to-60-Integer ::= A set of integers from 0 to 60
  ;seclist :: = ('sec 0-to-60-Integer)
  ;minlist :: = ('min 0-to-60-Integer)
  ;hrlist :: = ('hr 0-to-60-Integer)
  
  ;B)
  ;list-sym-list ::= (sym-list) | (sym-list list-sym-list) | ()
  ;sym-list ::= (s) | (s sym-list) | ()
  ;
  ;; PROJECT 1 Part B
  ;; First Representation | We added a suffix so that both first and second representations can be tested at once.

    (define successor-a
      (lambda (x)
        (let* ((hour 0)
          (minute 0)
          (seconds 0))
        (cond ((eq? (caar x) 'sec)
              (set! seconds (cadar x))
              (if (eq? seconds 59)
                  (cons (list 'sec 0) (successor-a (cdr x)))
                  (cons (list 'sec (+ seconds 1)) (cdr x))))
              ((eq? (caar x) 'min)
               (set! minute (cadar x))
                   (if (eq? minute 59)    
                             (cons (list 'min 0) (successor-a (cdr x)))
                             (cons (list 'min (+ minute 1))  (cdr x))))
              ((eq? (caar x) 'hr)
               (set! hour (cadar x))
                   (cons (list 'hr (+ hour 1)) null))))))



  (define create-a
    '((sec 0) (min 0) (hr 0)))

  (define is-zero-a?
    (lambda (x)
      (equal? x create-a)))
  ;; Second Representation | We added a -b suffix so that both Unary and BigNum can be tested at once.
  (define create-b
    
      '()
      )

  (define is-zero-b?
    (lambda (x)
      (null? x)))

  (define successor-b
    (lambda (x)
      (if (null? x) '((s))
          (if (< (length (car x)) 9)
              (cons (cons 's (car x)) (cdr x))
              (cons '() (successor-b (cdr x)))
              )
          )
      ))
  (define test (lambda (x n)
                 (if (> n  0)
                     (let ((mmm (successor-a x)))
                       (display mmm)
                       (print "                ")
                       (test (successor-a x) (- n 1)))
                     (print "success")
                     )

                 ))
  ;(test create-a 7200)
  ;; PROJECT 1 Part C | Write your answer below here as a comment
  ;Constructors are the methods to create objects or data types -create and successor methods are constructors, they are creating a new data.
  ;Observers are the method to manipulate and access data -is-zero? method is an observer but also a predicate since it returns true or false according to input.
  ;Predicates are one type of observers  which takes inputs and return true or false according to inputs.
  ;Extractors are one type of observers  which takes a complex input and return a specific part of the input (helpful in parsing). 
  ;
  
  ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the below function, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
  
  ;; PROJECT 1 Part D | Remove the comments and write test cases.
  (display "First Representation Tests\n")
  (equal?? create-a  '((sec 0) (min 0) (hr 0))) ; should return #t
  (equal?? (is-zero-a? '((sec 1) (min 0) (hr 0))) #f) ; should return #f
  (equal?? (is-zero-a? '((sec 0) (min 0) (hr 0))) #t) ; should return #t
  (equal?? (successor-a '( (sec 59) (min 59) (hr 0))) '((sec 0) (min 0) (hr 1))) ; should return #t
  (newline)

  
  (display "Second Representation Tests\n")
  (equal?? create-b  '()) ; should return ()
  (equal?? (is-zero-b? '('(s s s s s))) #f) ; should return #f
  (equal?? (is-zero-b? '()) #t) ; should return #t
  (equal?? (successor-b '((s s s s s s s s s))) '(() (s))) ; should return (() (s))
  (newline)
)