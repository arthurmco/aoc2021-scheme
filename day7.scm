(use-modules (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-43))


(define (read-crab-file-line port list)
  (let ((line (read-line port)))
    (if (eof-object? line)
        list
        (read-crab-file-line port (append list `(,line))))))

(define (read-crab-file-data port)
  (read-crab-file-line port '()))


(define (read-crab-file path)
  (let* ((port (open-input-file path))
         (data (map string->number (string-split (car (read-crab-file-data port)) #\, ))))
    (close-port port)
    data))


(define (determine-fuel-consumption-to-position crab-list position)
  (map (lambda (p) (abs (- p position))) crab-list))


(define (fuel-consumption-for-each-position crab-list)
  (let ((positions (delete-duplicates crab-list)))
    (map (lambda (pos) (cons pos (apply + (determine-fuel-consumption-to-position crab-list pos))))
         (iota (apply max crab-list)))))


(define (get-element-with-less-fuel-consumption crab-list)
  (car
   (sort (fuel-consumption-for-each-position crab-list)
         (lambda (a b) (< (cdr a) (cdr b))))))



(define (run-script file)
  (let* ((crabs (read-crab-file file))
         (position (get-element-with-less-fuel-consumption crabs)))
    (format #t "position: ~A, fuel: ~A\n" (car position) (cdr position))))
