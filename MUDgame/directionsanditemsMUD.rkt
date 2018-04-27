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
                        

