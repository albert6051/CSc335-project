(define call/cc call-with-current-continuation)

(define first car)

(define rest cdr)

(define get-signal
  (lambda (entry)
    (car (car entry))))

(define get-wait-list
  (lambda (entry)
    (car (cdr entry))))

(define terminate 'terminate)

; This the processes schedule queue, FIFO base
(define thread-queue '())

; This the table for all the processes that are waiting for a signal to move to schedule queue
(define wait-table '())

; Set terminate point when there is no next thread
(define set-end
  (lambda ()
    (let (
      (cont (call/cc (lambda (c) c))))
      (if (procedure? cont)
          (let* (
                 (next-thread (first thread-queue))
                 (remaining-thread (rest thread-queue)))
            (set! terminate cont)
            (set! thread-queue remaining-thread)
            (next-thread 'next))
          (display 'terminate)))))
   

; This function will save the current stack of the process and put it at the end of the schedule queue
(define thread-yield
  (lambda ()
    (if(null? thread-queue)
       (display '(no thread in the list no need to wait))
       (let (
             (cont (call/cc (lambda (c) c))))
         (if (procedure? cont)
             (let* (
                    (next-thread (first thread-queue))
                    (remaining-thread (rest thread-queue))
                    (new-thread-queue
                     (append remaining-thread (list cont))))
               (set! thread-queue new-thread-queue)
               (next-thread 'next))
             (display '(thread continue)))))))

; This function generate a table that with the new entry of signal and its process
(define new-wait-table
  (lambda (signal cont table)
    (cond ((null? table)
           (list (append table (list (list signal) (list cont)))))
          ((eq? signal (get-signal(first table)))
           (cons (list
                  (list signal)
                  (append (get-wait-list (first table)) (list cont)))
                 (rest table)))
          (else (cons (first table) (new-wait-table signal cont (cdr table)))))))

; This function save the process stack and put it on wait table, then call the next thread
(define wait-signal
  (lambda (signal)
    (if(null? thread-queue)
       (terminate 'end)
       (let (
             (cont (call/cc (lambda (c) c))))
         (if (procedure? cont)
             (let* (
                    (next-thread (first thread-queue))
                    (remaining-thread (rest thread-queue))
                    (new-table
                     (new-wait-table signal cont wait-table)))
               (set! wait-table new-table)
               (set! thread-queue remaining-thread)
               (next-thread 'next))
             (display '(thread continue)))))))

; This function release the process that receive the signal to schedule queue
(define add-proc-list
  (lambda (proc-list)
    (let (
          (new-thread-queue (append thread-queue proc-list)))
      (set! thread-queue new-thread-queue))))

; This function generate a wait table that remove entry from wait table with giving signal
(define remove-signal-table
  (lambda (signal table)
    (cond ((null? table) table)
          ((eq? signal (get-signal(first table)))
           (cdr table))
          (else (cons (first table) (remove-signal-table signal (rest table)))))))

; This function 
(define broadcast
  (lambda (signal table)
    (cond ((null? table) #t)
          ((eq? signal (get-signal(first table)))
           (add-proc-list (get-wait-list(first table)))
           (set! wait-table (remove-signal-table signal wait-table)))
          (else (broadcast signal (rest table))))))

; This function will tell the process is end and call the next process on the schedule queue
; or exit if there is no more process in the queue
(define thread-end
  (lambda ()
    (if (null? thread-queue)
        '(exit)
        (let* (
               (next-thread (first thread-queue))
               (remaining-thread (rest thread-queue)))
          (set! thread-queue remaining-thread)
          (next-thread 'next)))))

; This function will create thread for the process and save stack of the process using continuaton
; which allow the process to yield and call the next thread
(define create-thread
  (lambda (thread-process)
    (let (
          (cont (call/cc (lambda (c) c))))
      (if (procedure? cont)
          (begin
            (set! thread-queue (append thread-queue (list cont)))
            (thread-process)
            (thread-end))
          'end))))


; Testing
;(define proc-1
  ;(lambda ()
    ;(display "hello 1\n")
    ;(thread-yield)
    ;(display "hello 1 again\n")))
;(define proc-2
  ;(lambda ()
    ;(display "hello 2\n")
    ;(thread-yield)
    ;(display "hello 2 again\n")
    ;(thread-yield)
    ;(display "hello 2 again again\n")))
;(define proc-3
  ;(lambda ()
    ;(display "hello 3\n")
    ;(thread-yield)
    ;(display "hello 3 again\n")))

;(define proc-1
;  (lambda ()
;    (display "proc-1 wait for signal first\n")
;    (wait-signal 'first)
;    (display "proc-1 receive signal first\n")
;    (display "proc-1 wait for signal second\n")
;    (wait-signal 'second)
;    (display "proc-1 receive signal second\n")))
;(define proc-2
;  (lambda ()
;    (display "proc-2 wait for signal second\n")
;    (wait-signal 'second)
;    (display "proc-2 receive signal second\n")))
;(define proc-3
;  (lambda ()
;    (display "proc-3 broadcast signal first\n")
;    (broadcast 'first wait-table)))
;(define proc-4
;  (lambda ()
;    (display "proc-4 broadcast signal second\n")
;    (broadcast 'second wait-table)))


;(create-thread proc-1)
;(create-thread proc-2)
;(create-thread proc-3)
;(create-thread proc-4)
;(set-end)

(define consumer
  (lambda ()
    (display "consumer wait for producer to produce\n")
    (wait-signal 'producer)
    (display "consumer receive signal from producer\n")
    (display "consumer broadcast signal\n")
    (broadcast 'consumer wait-table)
    (display "consumer wait for producer to produce\n")
    (wait-signal 'prodcuer)
    (display "comsumer receive signal from producer\n")))

(define producer
  (lambda ()
    (broadcast 'producer wait-table)
    (display "producer broadcast signal\n")
    (wait-signal 'consumer)
    (broadcast 'prodcuer wait-table)
    (display "producer broadcast signal\n")))

    
(create-thread consumer)
(create-thread producer)
(set-end)