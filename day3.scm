(use-modules (ice-9 rdelim)
             (srfi srfi-1))

(define (read-diagnostic-file-line port list)
  (let ((line (read-line port)))
    (if (eof-object? line)
        list
        (read-diagnostic-file-line port (append list `(,line))))))

(define (read-diagnostic-file-data port)
  (read-diagnostic-file-line port '()))


(define (read-diagnostic-file path)
  (let* ((port (open-input-file path))
         (data (read-diagnostic-file-data port)))
    (close-port port)
    data))


(define (count-bits position list)
  """Count all 'bits' of the list that lies on the position `position`.
  Returns a cons with the format (0bits . 1bits) """

  (let ((bits (map (lambda (strbyte)
                     (string-ref strbyte position)) list)))

    (fold (lambda (bit count-info)
            (let ((zeroes (car count-info))
                  (ones (cdr count-info)))

              (cons
               (+ zeroes (if (equal? bit #\0) 1 0))
               (+ ones (if (equal? bit #\1) 1 0)))))
          (cons 0 0)
          bits)))


(define (diagnostics-bit-count list)
  (string-length (car list)))

(define (most-common-bit count-info)
  (let ((zeroes (car count-info))
        (ones (cdr count-info)))

    (if (> zeroes ones) 0 1)))

(define (least-common-bit count-info)
  (let ((zeroes (car count-info))
        (ones (cdr count-info)))

    (if (< zeroes ones) 0 1)))



(define (binary-decimal-recur revbinary num)
  (if (null? revbinary) 0
      (+
       (* (car revbinary) (expt 2 num))
       (binary-decimal-recur (cdr revbinary) (+ num 1)))))


(define (binary->decimal binarylist)
  (binary-decimal-recur (reverse binarylist) 0))


(define (get-bit-count list)
  (map (lambda (i) (count-bits i list)) (iota (diagnostics-bit-count list))))


(define (char->bit char)
  (cond
   ((equal? char #\0) 0)
   ((equal? char #\1) 1)
   (else #f)))
  
(define (filter-diagnostic-list-by list index bit)
  (filter (lambda (strbyte)
            (equal? (char->bit (string-ref strbyte index)) bit))
          list))


(define (oxygen-gen-common-bit count-info)
  (let ((zeroes (car count-info))
        (ones (cdr count-info)))
    (cond
     ((> zeroes ones) 0)
     ((> ones zeroes) 1)
     (else 1))))

(define (co2-scrub-common-bit count-info)
  (let ((zeroes (car count-info))
        (ones (cdr count-info)))
    (cond
     ((> zeroes ones) 1)
     ((> ones zeroes) 0)
     (else 0))))


(define (filter-diagnostic-list-recur list bit-count index common-bit-fn)
  (if (= (length list) 1)
      (car list)
      (let ((filtered-list
             (filter-diagnostic-list-by list index
                                        (common-bit-fn (car (drop bit-count index))))))
        (filter-diagnostic-list-recur
         filtered-list
         (get-bit-count filtered-list)
         (+ index 1)
         common-bit-fn))))

(define (get-oxygen-generator-bits list bit-count)
  (map char->bit (string->list (filter-diagnostic-list-recur list bit-count 0 oxygen-gen-common-bit))))

(define (get-co2-scrubber-bits list bit-count)
  (map char->bit (string->list (filter-diagnostic-list-recur list bit-count 0 co2-scrub-common-bit))))


(define (run-script file)
  (let* ((diagnostic (read-diagnostic-file file))
         (bit-count (get-bit-count diagnostic))
         (gamma-rate (binary->decimal (map most-common-bit bit-count)))
         (epsilon-rate (binary->decimal (map least-common-bit bit-count))))
    (format #t "gamma: ~A, epsilon: ~A, power: ~A\n" gamma-rate epsilon-rate
            (* gamma-rate epsilon-rate))))


(define (run-script-2 file)
  (let* ((diagnostic (read-diagnostic-file file))
         (bit-count (get-bit-count diagnostic))
         (oxygen-gen (binary->decimal (get-oxygen-generator-bits diagnostic bit-count)))
         (co2-scrub (binary->decimal (get-co2-scrubber-bits diagnostic bit-count))))
    (format #t "oxygen-gen: ~A, co2-scrub: ~A, life support rating: ~A\n" oxygen-gen co2-scrub
            (* oxygen-gen co2-scrub))))
