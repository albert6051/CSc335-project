(define call/cc call-with-current-continuation)

(define first car)

(define rest cdr)

(define th-list '())

(define th-yield
  (lambda ()
    (if(null? th-list)
       #t
       (let (
             (cont (call/cc (lambda (c) c))))
         (if (procedure? cont)
             (let* (
                    (next-th (first th-list))
                    (remaining-th (rest th-list))
                    (new-th-ls
                     (append remaining-th (list cont))))
               (set! th-list new-th-ls)
               (next-th 'next))
             #t)))))

(define th-end
  (lambda ()
    (if (null? th-list)
        '(exit)
        (let* (
               (next-th (first th-list))
               (remaining-th (rest th-list)))
          (set! th-list remaining-th)
          (next-th 'next)))))

(define th-new
  (lambda (th-proc)
    (let (
          (cont (call/cc (lambda (c) c))))
      (if (procedure? cont)
          (begin
            (set! th-list (append th-list (list cont)))
            (th-proc)
            (th-end))
          #t))))

(define proc-1
  (lambda ()
    (display "hello 1\n")
    (th-yield)
    (display "hello 1 again\n")))
(define proc-2
  (lambda ()
    (display "hello 2\n")
    (th-yield)
    (display "hello 2 again\n")
    (th-yield)
    (display "hello 2 again again\n")))
(define proc-3
  (lambda ()
    (display "hello 3\n")
    (th-yield)
    (display "hello 3 again\n")))

(th-new proc-1)
(th-new proc-2)
(th-new proc-3)
(th-end)