#lang racket


(define (slist->string l)
  (string-join (map symbol->string l)))    
   
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))
 
(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))
 
(define (get-response id)
  (car (assq-ref descriptions id)))
 
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))
 
 

(define (list-of-lengths keylist tokens)
  (map
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       (* (/ (length set) (length x)) (length set))))
   keylist))
 
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))
 
 
(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index
      (cadr (list-ref record index))
      #f)))
 
 ;;Statement used for the main game loop.
(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        
        (printf "~a\n> " (get-response id))
        (printf "> "))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
;;Response actions that can be used within the game.        
        (cond ((number? response)
               (loop response #t))
;;Response to invalid function from player.              
              ((eq? #f response)
               (format #t "No such action. Try again!\n")
               (loop id #f))
;;Response to see help menu              
              ((eq? response 'help)
               (display-help)
               (loop id #f))
;;Response to look around the room              
              ((eq? response 'look)
               (get-directions id)
               (loop id #f))
;Response to quit the game.              
              ((eq? response 'quit)
               (format #t "You have successfully left the game!\n")
               (exit)))))))

(startgame 1)