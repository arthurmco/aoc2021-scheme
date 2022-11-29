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



(define (run-script file)
  (let* ((diagnostic (read-diagnostic-file file))
         (bit-count (get-bit-count diagnostic))
         (gamma-rate (binary->decimal (map most-common-bit bit-count)))
         (epsilon-rate (binary->decimal (map least-common-bit bit-count))))
    (format #t "gamma: ~A, epsilon: ~A, power: ~A\n" gamma-rate epsilon-rate
            (* gamma-rate epsilon-rate))))
