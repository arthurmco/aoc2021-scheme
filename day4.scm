(use-modules (ice-9 rdelim)
             (srfi srfi-1))

(define (read-bingo-numbers port)
  (let ((line (read-line port)))
    (if (eof-object? line)
        #f
        (map string->number (string-split line #\,)))))

(define (read-bingo-board-lines port remaining-lines)
  (if (= remaining-lines 0)
      '()
      (cons
       (let ((line (read-line port)))
         (filter-map string->number (string-split line #\ )))
       (read-bingo-board-lines port (- remaining-lines 1)))))

(define (read-bingo-board port)
  (read-bingo-board-lines port 5))


(define (read-bingo-boards port)
  (let ((line (read-line port)))
    (if (eof-object? line)
        '()
        (cons (read-bingo-board port)
              (read-bingo-boards port)))))


(define (read-bingo-file-data port)
  (cons
   (read-bingo-numbers port)
   (map (lambda (v) (apply append v)) (read-bingo-boards port))))

(define (read-bingo-file path)
  (let* ((port (open-input-file path))
         (data (read-bingo-file-data port)))
    (close-port port)
    data))


(define (mark-board-as-index-data line number)
  (fold (lambda (value index-data)
          (let ((index (car index-data))
                (index-list (cdr index-data)))
            (cons (+ index 1)
                  (if (= value number)
                      (cons index index-list)
                      index-list))))
        (cons 0 '()) line))

(define (mark-board line number)
  (let ((index-list (cdr (mark-board-as-index-data line number))))
    (if (null? index-list)
        '()
        index-list)))

(define (mark-board-set board-set number)
  (map (lambda (b)
         (mark-board b number))
       board-set))

(define (append-board-set board1 board2)
  (map (lambda (b1 b2)
         (sort (append b1 b2) <))
       board1 board2))


; '(0 1 2 3 4 8 12 16 19 20 21 24)

(define (board-won-horizontally? board-data)
  (if (< (length board-data) 5)
      #f
      (let ((board-split (call-with-values
                             (lambda () (partition (lambda (i) (< i 5)) board-data))
                           (lambda (line after) (cons line after)))))
        (if (equal? (car board-split) '(0 1 2 3 4))
            #t
            (board-won-horizontally? (map (lambda (i) (- i 5)) (cdr board-split)))))))


; '(0 1 2 3 4 8 12 16 19 20 21 24)

(define (board-won-vertically-recur vertical-indices)
  (if (< (length vertical-indices) 5)
      #f
      (let ((board-split (call-with-values
                             (lambda () (span (lambda (i) (= i (car vertical-indices))) vertical-indices))
                           (lambda (line after) (cons line after)))))
        (if (= (length (car board-split)) 5)
            #t
            (board-won-vertically-recur (cdr board-split))))))

  
(define (board-won-vertically? board-data)
  (let ((vertical-indices
         (sort (map (lambda (n) (modulo n 5)) board-data) < )))
    (board-won-vertically-recur vertical-indices)))
  


(define (board-won? board)
  (if (null? board)
      #f
      (or
       (board-won-horizontally? board)
       (board-won-vertically? board))))

(define (what-board-won board-set)
  (list-index
   (lambda (board)
     (board-won? board))
   board-set))


(define (iterate-game board number marked-set)
  (let* ((new-marked-set (append-board-set marked-set (mark-board-set board number)))
         (won-board (what-board-won new-marked-set)))
    (if (equal? won-board #f)
        (cons 'continue new-marked-set)
        (cons 'won (list won-board number new-marked-set)))))

(define (create-empty-marked-set board-set)
  (map (lambda (_) '()) (iota (length board-set))))

(define (fold-step-game board number state)
  (cond
   ((eq? (car state) 'won) state)
   ((eq? (car state) 'continue) (iterate-game board number (cdr state)))))

(define (play-game boards nums)
  (fold (lambda (n state) (fold-step-game boards n state))
        (iterate-game boards (car nums) (create-empty-marked-set boards))
        (cdr nums)))

(define (bingo-board-index boards index)
  (if (= index 0)
      (car boards)
      (bingo-board-index (cdr boards) (- index 1))))

(define (get-unmarked-board board marked-set)
  (let* ((board-vec (list->vector board))
         (index-values (map
                        (lambda (i) (vector-ref board-vec i))
                        marked-set)))

    (filter
     (lambda (v)
       (not (find (lambda (i) (= i v)) index-values)))
     (vector->list board-vec))))


(define (sum-unmarked board marked-set)
  (fold + 0 (get-unmarked-board board marked-set)))


(define (run-script-victory-conditions boards game-end-state)
  (let ((board-won-index (second game-end-state))
        (last-num (third game-end-state))
        (marked-set (fourth game-end-state)))

    (let ((sum-not-marked
           (sum-unmarked
            (bingo-board-index boards board-won-index)
            (bingo-board-index marked-set board-won-index))))
      (format #t "sum: ~A, last number called: ~A, final score: ~A\n"
              sum-not-marked last-num (* sum-not-marked last-num)))))


(define (run-script file)
  (let* ((bingo-file (read-bingo-file file))
         (bingo-nums (car bingo-file))
         (bingo-boards (cdr bingo-file))
         (game-end-state (play-game bingo-boards bingo-nums)))

    (if (eq? (car game-end-state) 'won)
        (run-script-victory-conditions bingo-boards game-end-state)
        (display "Perdeste"))))
