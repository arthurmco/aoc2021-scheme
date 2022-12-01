(use-modules (ice-9 rdelim)
             (ice-9 hash-table)
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
(define (init-lanternfish-lifetime-dictionary lanternfish-list)
  (let ((fish-counter
         (lambda (num)
           (count (lambda (i) (= num i)) lanternfish-list) )))
    (alist->hash-table (map (lambda (num)
                              (cons num (fish-counter num)))
                            (iota 10 -1)))))

(define (sum-lanternfishes lanternfish-dict)
  (hash-fold (lambda (_ num sum) (+ num sum)) 0 lanternfish-dict))

(define (decrement-lanternfish lanternfish-dictionary)
  (alist->hash-table
   (filter (lambda (c) (> (car c) -2))
           (map (lambda (c) (cons (- (car c) 1) (cdr c)))
                (hash-map->list cons lanternfish-dictionary)))))


(define (handle-lanternfish-birth lanternfish-dictionary)
  (let ((pregnant-fishes (hash-ref lanternfish-dictionary -1)))
    (hash-set! lanternfish-dictionary -1 0)
    (hash-set! lanternfish-dictionary 6 (+ (hash-ref lanternfish-dictionary 6 0) pregnant-fishes))
    (hash-set! lanternfish-dictionary 8 (+ (hash-ref lanternfish-dictionary 8 0) pregnant-fishes))
    lanternfish-dictionary))

(define (lanternfish-iterate-each-day lanternfish-dictionary days-remaining)
  (format #t "Days remaining for this slot: ~A\n" days-remaining)
  (if (<= days-remaining 0)
      lanternfish-dictionary
      (lanternfish-iterate-each-day
       (handle-lanternfish-birth (decrement-lanternfish lanternfish-dictionary))
       (- days-remaining 1))))


(define (run-script file)
  (let* ((initial-state (init-lanternfish-lifetime-dictionary (read-lanternfish-file file)))
        (after-80-days (lanternfish-iterate-each-day initial-state 80)))
    (format #t "After 80 days we have ~A fish\n" (sum-lanternfishes after-80-days))))

(define (run-script-2 file)
  (let* ((initial-state (init-lanternfish-lifetime-dictionary (read-lanternfish-file file)))
        (after-256-days (lanternfish-iterate-each-day initial-state 256)))
    (format #t "After 256 days we have ~A fish\n" (sum-lanternfishes after-256-days))))
