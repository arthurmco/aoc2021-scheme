(use-modules (ice-9 rdelim)
             (srfi srfi-1))

(define (read-depth-file-line port list)
  (let ((line (read-line port)))
    (if (eof-object? line)
        list
        (read-depth-file-line port (append list `(,(string->number line)))))))

(define (read-depth-file-data port)
  (read-depth-file-line port '()))


(define (read-depth-file path)
  (let* ((port (open-input-file path))
         (data (read-depth-file-data port)))
    (close-port port)
    data))


(define (has-depth-increased? current prev)
  (> current prev))

(define (has-depth-decreased? current prev)
  (< current prev))

(define (create-depth-change-list list)
  (fold (lambda (current depthlist)
            (if (= (length depthlist) 0)
                `( ,(cons current "n"))
                (let ((prev (caar depthlist)))
                  (cons
                   (cons current
                         (cond
                          ((has-depth-increased? current prev) "+")
                          ((has-depth-decreased? current prev) "-")
                          (else "n")))
                   depthlist))))
        '()
        list))

(define (create-depth-sliding-window list)
  """From a list, return a list of maximum of three elements, corresponding to
   the depth sliding window.

   From a list position n, we will return a list corresponding to (n n-1 n-2) if list[n]
   exists"""

   (let ((l1 list)
         (l2 (cdr list))
         (l3 (cddr list)))
      (zip l1 l2 l3)))


(define (sum-sliding-window slidinglist)
  (map (lambda (item)
         (reduce + 0 item)) slidinglist))


(define (count-increases dlist)
  (count-marks dlist "+"))

(define (count-marks dlist mark)
  (fold (lambda (current count)
          (let ((currentmark (cdr current)))
            (if (equal? mark currentmark)
                (+ count 1)
                count)))
        0
        dlist))

 
(define (run-script file)
  (let* ((heightdata (read-depth-file file))
         (changelist (create-depth-change-list heightdata)))
    (format #t "Increases: ~A" (count-increases changelist))))

 
(define (run-script-2 file)
  (let* ((heightdata (read-depth-file file))
         (slidinglist (create-depth-sliding-window heightdata))
         (slidingsum (sum-sliding-window slidinglist))
         (changelist (create-depth-change-list slidingsum)))
    (format #t "Increases: ~A" (count-increases changelist))))
