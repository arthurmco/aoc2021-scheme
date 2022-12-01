(use-modules (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-13))

(define-record-type vent-line
  (make-vent-line start end)
  vent-line?
  (start vent-line-start)
  (end vent-line-end))


(define (parse-vent-coords coord)
  (let ((coord-list (map string->number (string-split coord #\,))))
    (cons (first coord-list) (second coord-list))))

;; 0,9 -> 5,9
(define (parse-vent-line text)
  (apply make-vent-line
         (map (lambda (s) (parse-vent-coords
                           (string-trim-both
                            (string-drop-right s 1))))
              (string-split (string-append text " ") #\>))))

(define (read-vent-file-line port list)
  (let ((line (read-line port)))
    (if (eof-object? line)
        list
        (read-vent-file-line port (append list `(,(parse-vent-line line)))))))

(define (read-vent-file-data port)
  (read-vent-file-line port '()))


(define (read-vent-file path)
  (let* ((port (open-input-file path))
         (data (read-vent-file-data port)))
    (close-port port)
    data))

(define (vent-line-is-horizontal? vent-line)
  (= (cdr (vent-line-start vent-line)) (cdr (vent-line-end vent-line))))

(define (vent-line-is-vertical? vent-line)
  (= (car (vent-line-start vent-line)) (car (vent-line-end vent-line))))


(define (make-trajectory-point start end)
  (cons start end))

(define (vent-line-horizontal-trajectory start end)
  (cond
   ((equal? start end) `(,start))
   ((> (car start) (car end)) (vent-line-horizontal-trajectory end start))
   (else
    (cons
     start
     (vent-line-horizontal-trajectory (cons (+ (car start) 1) (cdr start)) end)))))

(define (vent-line-vertical-trajectory start end)
  (cond
   ((equal? start end) `(,start))
   ((> (cdr start) (cdr end)) (vent-line-vertical-trajectory end start))
   (else
    (cons
     start
     (vent-line-vertical-trajectory (cons (car start) (+ (cdr start) 1)) end)))))

(define (vent-line-trajectory vent-line)
  (cond
   ((vent-line-is-horizontal? vent-line)
    (vent-line-horizontal-trajectory (vent-line-start vent-line)
                                     (vent-line-end vent-line)))
   ((vent-line-is-vertical? vent-line)
    (vent-line-vertical-trajectory (vent-line-start vent-line)
                                   (vent-line-end vent-line)))
   (else '())))


(define (point-to-hash-key point)
  (let ((x (car point))
        (y (cdr point)))
    (format #f "~A-~A" x y)))


;; hash-set uses equal
;; hashq-set uses eq
;; hashx-set uses a custom equality function

(define (generate-overlap-table points)
  (let ((overlap-hash (make-hash-table (length points))))
    (for-each
     (lambda (point)
       (let ((pkey (point-to-hash-key point)))
         (hash-set! overlap-hash pkey
                     (+ (hash-ref overlap-hash pkey 0) 1))))
     points)
    overlap-hash))

(define (count-overlaps overlap-hash)
  (hash-count (lambda (k, v) (> v 1)) overlap-hash))

(define (run-script file)
  (let* ((vents (read-vent-file file))
         (points (apply append (map vent-line-trajectory vents)))
         (overlaps (generate-overlap-table points)))

  (format #t "Overlap count: ~A\n" (count-overlaps overlaps))))
