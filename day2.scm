(use-modules (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-9))


(define (parse-raw-pilot-line line)
  (let* ((linedata (string-split line #\ ))
         (direction (first linedata))
         (count (second linedata)))

    (cons
     (cond
      ((equal? direction "forward") 'forward)
      ((equal? direction "up") 'up)
      ((equal? direction "down") 'down)
      (else #f))
     (string->number count))))

(define (read-pilot-file-line port list)
  (let ((line (read-line port)))
    (if (eof-object? line)
        list
        (read-pilot-file-line port (append list `(,(parse-raw-pilot-line line)))))))

(define (read-pilot-file-data port)
  (read-pilot-file-line port '()))


(define (read-pilot-file path)
  (let* ((port (open-input-file path))
         (data (read-pilot-file-data port)))
    (close-port port)
    data))


(define-record-type submarine
  (make-submarine horizontal-pos depth aim)
  submarine?
  (horizontal-pos submarine-horizontal-pos)
  (depth submarine-depth)
  (aim submarine-aim))


(define (get-horizontal-offset-from-instruction direction count)
  (if (eq? direction 'forward)
      count
      0))

(define (get-depth-offset-from-instruction submarine direction count)
  (let ((aim (submarine-aim submarine))
        (hoffset (get-horizontal-offset-from-instruction direction count)))
    (* aim hoffset)))

(define (get-aim-offset-from-instruction direction count)
  (cond
   ((eq? direction 'up) (- count))
   ((eq? direction 'down) count)
   (else 0)))

(define (process-instruction instruction submarine)
  (let ((direction (car instruction))
        (count (cdr instruction))
        (hpos (submarine-horizontal-pos submarine))
        (depth (submarine-depth submarine))
        (aim (submarine-aim submarine)))

    (make-submarine
     (+ hpos (get-horizontal-offset-from-instruction direction count))
     (+ depth (get-depth-offset-from-instruction submarine direction count))
     (+ aim (get-aim-offset-from-instruction direction count)))))


(define (walk-submarine initial-submarine pilot-data)
  (fold process-instruction initial-submarine pilot-data))

(define (run-script file)
  (let* ((pilot-data (read-pilot-file file))
         (submarine (walk-submarine (make-submarine 0 0 0) pilot-data)))
    (format #t "Horizontal: ~A | Depth: ~A | Total: ~A \n"
            (submarine-horizontal-pos submarine)
            (submarine-depth submarine)
            (* (submarine-horizontal-pos submarine)
               (submarine-depth submarine)))))

