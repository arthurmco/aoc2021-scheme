(use-modules (ice-9 rdelim)
             (ice-9 futures)
             (srfi srfi-1)
             (srfi srfi-43))

(define (read-lanternfish-file-line port list)
  (let ((line (read-line port)))
    (if (eof-object? line)
        list
        (read-lanternfish-file-line port (append list `(,line))))))

(define (read-lanternfish-file-data port)
  (read-lanternfish-file-line port '()))


(define (read-lanternfish-file path)
  (let* ((port (open-input-file path))
         (data (map string->number (string-split (car (read-lanternfish-file-data port)) #\, ))))
    (close-port port)
    data))

;; TODO: track per internal timer, not per count!

(define (decrement-lanternfish lanternfish-vec)
  (vector-map (lambda (_ v) (- v 1)) lanternfish-vec))

(define (handle-lanternfish-birth lanternfish-vec)
   (vector-fold
    (lambda (_ fishlist age)
      (if (>= age 0)
          (vector-append fishlist (vector age))
          (vector-append fishlist (vector 6 8))))
    (vector) lanternfish-vec))


(define (lanternfish-iterate-each-day lanternfish-vec days-remaining)
  (format #t "Days remaining for this slot: ~A\n" days-remaining)
  (if (<= days-remaining 0)
      lanternfish-vec
      (lanternfish-iterate-each-day
       (handle-lanternfish-birth (decrement-lanternfish lanternfish-vec))
       (- days-remaining 1))))

(define (lanternfish-iterate-each-day-parallel lanternfish-vec days-remaining)
  (let ((tasks (vector-map
                 (lambda (i v) (future (lanternfish-iterate-each-day v days-remaining)))
                 (vector-map (lambda (_ v) (vector v)) lanternfish-vec))))
    (vector->list
     (vector-map (lambda (_ v) (vector-length v)) (vector-map (lambda (_ v) (touch v)) tasks)))))


(define (run-script file)
  (let* ((initial-state (read-lanternfish-file file))
        (after-80-days (lanternfish-iterate-each-day (list->vector initial-state) 80)))
    (format #t "After 80 days we have ~A fish\n" (vector-length after-80-days))))

(define (run-script-2 file)
  (let* ((initial-state (read-lanternfish-file file))
        (after-256-days (lanternfish-iterate-each-day (list->vector initial-state) 256)))
    (format #t "After 256 days we have ~A fish\n" (vector-length after-256-days))))
