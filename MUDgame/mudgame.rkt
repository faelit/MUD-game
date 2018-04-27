#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

;;Description of objects
(define object '((1 "a pickaxe")
                  (3 "a watch")
                  (4 "a rock")
                  (2 "a compass")
                  (6 "a finish flag")))
 
;;Description of rooms 
(define descriptions '((1 "You are in the Room Of Beginning!")
                       (2 "You are in the Empty Room. ")
                       (3 "You are in the Frozen Yanderes Chamber.")
                       (4 "You are in the Tsunderes Lounge")
                       (5 "You are in the Room Of Dyling Light")
                       (6 "You have reached Room Of End.\nYou have completed the game!")))

(define objectdb (make-hash))

;;List of actions. These define what a player can do within the game
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define help '(((help) help)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define actions `(,@look ,@quit ,@help))



;Decision table actions for entering rooms, allows user to choose between which direction you can move.
(define decisiontable `((1 ((north) 2) ((west) 3) ,@actions)
                        (2 ((south) 1) ((north) 4) ,@actions)
                        (3 ((south) 1),@actions)
                        (4 ((north) 5) ((west) 3) ,@actions)
                        (5 ((south) 4) ((east) 6),@actions)
                        (6 ((west) 5) ((north) ),@actions)))
                        

;;Statement used for showing possible directions.
(define (get-directions id)  
  (let ((record (assq id decisiontable)))    
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
;;Statement shows that 0 directions were found.      
      (cond ((= 0 n)
             (printf "You have discovered a room with only one way out.\n"))
            ((= 1 n)
;;Statement used for extracting possible directions.             
             (printf "There is a way out at ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
;;Statement used to show player available room exits.               
               (printf "There are exits at ~a.\n" (string-join lostr " and "))))))))

;;Statement used for displaying help menu to the player.
(define (display-help)
  (printf "\nHelp\n
Welcome to the help menu.\n\n
Game Objective\n
The main point of the game is to reach Room of End and take the finish flag.\n\n
GAME COMMANDS\n
- look: to look around the room.\n
- help: to see help menu.\n
- pick: to pick up items.\n
- quit: to exit the game.\n"))


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