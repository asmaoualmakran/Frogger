
#lang racket

;--------------------------------------------
;Programmeerproject 1 academiejaar 2014-20153
;Frogger
;Asma oualmakran (asoualma@vub.ac.be)
;-------------------------------------------

(#%require 2htdp/batch-io)
(#%require "Graphics.rkt")
(#%require "lvl.rkt")
(#%require "drawer.rkt")

(define (vector-for-each v fun)
  (do ((i 0 (+ i 1)))
    ((= i (vector-length v)) v)
    (fun (vector-ref v i))))
;debug code in comment

;--------------------------------------------------------------------------------------
;globale definietie van de tags om magische constanten te vermijden
;het is ook gemakkelijker om van tag te veranderen zonder het hele code aan te passen
;--------------------------------------------------------------------------------------
(define road 'road)
(define grass 'grass)
(define coin 'coin)
(define bush 'bush)
(define finish 'finish)
(define purple 'purple)
(define blue 'blue)
(define green 'green)
(define red 'red)
(define power-up 'power-up)
(define pink 'pink)
(define orange 'organge)



;--------------------------------------------
; ADT-field
; Road en grass zijn functioneel gezien
; bijna indentiek, buiten dat ze een andere
; naam hebben en een andere image
;--------------------------------------------

(define (ADT-field lane section tag)
  (define drawable 0)
  (define layer 0)
  
  (define (set-drawable x)
    (set! drawable x))
  
  (define (set-layer x)
    (set! layer x))
  
  (define (draw draw-adt)
    (cond
      ((eq? tag grass)((draw-adt 'draw-grass)dispatch-field))
      ((eq? tag road)((draw-adt 'draw-road)dispatch-field))
      ((eq? tag finish)((draw-adt 'draw-grass)dispatch-field))))              
  
  
  (define (remove-drawable draw-adt)
    ((draw-adt 'disable-object-draw)dispatch-field))
  
  (define (dispatch-field msg)
    (cond
      ((eq? msg 'drawable)drawable)
      ((eq? msg 'remove-drawable)remove-drawable)
      ((eq? msg 'layer)layer)
      ((eq? msg 'set-drawable)set-drawable)
      ((eq? msg 'set-layer)set-layer)
      ((eq? msg 'draw) draw)
      ((eq? msg 'lane)lane)
      ((eq? msg 'section)section)
      (else (display "Wrong msg ADT-grass")
            (newline)
            (display msg))))
  dispatch-field)


;------------------------------------------------------------------------
; ADT-object zorgt voor alle adt's met gelijkaardig gedrag
; Hierdoor moet je niet ieder soort van object in een appart adt stoppen
;------------------------------------------------------------------------

(define (ADT-object lane section tag)
  
  (define drawable 0)
  (define layer 0)
  
  (define (set-drawable x)
    (set! drawable x))
  
  (define (set-layer x)
    (set! layer x))
  
  (define (draw draw-adt)
    ((draw-adt (determine-object))dispatch-object))
  
  (define (remove-drawable draw-adt)
    ((draw-adt 'disable-object-draw)dispatch-object))
  
  
  (define (determine-object)
    (cond
      ((eq? tag coin) 'draw-coin)
      ((eq? tag blue) 'draw-blue)
      ((eq? tag purple) 'draw-purple)
      ((eq? tag green) 'draw-green)
      ((eq? tag red) 'draw-red)
      ((eq? tag power-up) 'draw-power-up)
      ((eq? tag bush)'draw-bush)
      (else (display "Non-existant object"))))
  
  
  
  (define (dispatch-object msg)
    (cond
      ((eq? msg 'set-drawable)set-drawable)
      ((eq? msg 'set-layer)set-layer)
      ((eq? msg 'layer)layer)
      ((eq? msg 'drawable)drawable)
      ((eq? msg 'remove-drawable)remove-drawable)
      ((eq? msg 'draw)draw)
      ((eq? msg 'lane)lane)
      ((eq? msg 'section)section)
      ((eq? msg 'object-tag)tag)
      (else (display "Wrong msg ADT-object")
            (newline)
            (display msg))))
  dispatch-object)

;-----------------------------------------------
; ADT-score
;-----------------------------------------------

(define (ADT-score)
  
  (let ((score 0)
        (lives 3))
    
    (define highscore (read-file "highscore.txt"))
    
    (define (update-highscore)
      (if (> score (string->number highscore))
          (write-file "highscore.txt" (number->string score))
          #f))
    
    (define coin-val 10)
    (define purple-val 20)
    (define blue-val 50)
    (define red-val 100)
    (define green-val 200)
    
    (define (reset)
      (set! score 0)
      (set! lives 3))
    
    (define (decr-lives)
      (set! lives (- lives 1)))
    
    (define (incr  c)
      (set! score (+ score c)))
    
    (define (draw adt-draw)
      ((adt-draw 'write-text) (string-append "score: "
                                             (number->string score)
                                             "  Lives: "
                                             (number->string lives)) "black"))
    (define (clear-text adt-draw)
      (adt-draw 'delete-text))
    
    (define (incr-score tag)
      (cond
        ((eq? tag coin)(incr coin-val))
        ((eq? tag purple)(incr purple-val))
        ((eq? tag blue )(incr blue-val))
        ((eq? tag green)(incr  green-val))
        (else (display "Non existand value")
              (newline)
              (display tag))))
    
    
    (define (dispatch-score msg)
      (cond
        ((eq? msg 'draw) draw)
        ((eq? msg 'score)score)
        ((eq? msg 'highscore)highscore)
        ((eq? msg 'clear-text)clear-text)
        ((eq? msg 'update-highscore)(update-highscore))
        ((eq? msg 'lives)lives)
        ((eq? msg 'decr-lives)(decr-lives))
        ((eq? msg 'reset)(reset))
        ((eq? msg 'incr-score) incr-score)
        (else (display "Wrong msg ADT-score")
              (newline)
              (display msg))))
    dispatch-score))


;---------------------------------------------
;ADT-menu
;---------------------------------------------

(define (ADT-menu y x)
  
  (define drawable 0)
  (define layer 0)
  
  (define (set-drawable x)
    (set! drawable x))
  
  (define (set-layer x)
    (set! layer x))
  
  
  (define (remove-drawable draw-adt)
    ((draw-adt 'disable-object-draw)dispatch-menu))
  
  (define (draw-start-screen draw-adt)
    ((draw-adt 'draw-start-screen)dispatch-menu)
    ((draw-adt 'write-text)"press space to start" "red"))
  
  (define (draw-game-over draw-adt score-adt)
    ((draw-adt 'draw-game-over)dispatch-menu)
    ((draw-adt 'write-text)
     (string-append
      "press E to restart              HIGHSCORE: "(score-adt 'highscore))"red"))
  
  
  (define (dispatch-menu msg)
    (cond
      ((eq? msg 'lane)y)
      ((eq? msg 'section)x)
      ((eq? msg 'draw-start-screen)draw-start-screen)
      ((eq? msg 'draw-game-over)draw-game-over)
      ((eq? msg 'drawable)drawable)
      ((eq? msg 'layer)layer)
      ((eq? msg 'set-layer)set-layer)
      ((eq? msg 'set-drawable)set-drawable)
      ((eq? msg 'remove-drawable)remove-drawable)
      (else (display "Wrong msg ADT-start-screen")
            (newline)
            (display msg))))
  dispatch-menu)

;-----------------------------------------------------------------------------------------------
; ADT-lane
; dit adt voorziet de abstractie voor de rijstroken te kunnen maken
; ieder rijstrook bestaat uit vakken en een snelweg is een samenstelling van
; rijstroken
; mogelijk om headed vectors voor de rijrichting aan te duiden voor de wagens (idx 0 bevat tag)
; onderstaande ADT is geschreven door Brecht Rooms race-spel-met-gui-verbinding.rkt
;----------------------------------------------------------------------------------------------
(define (ADT-lane vec lane-idx)
  
  (define lane
    (let ((section-idx 0))
      
      (vector-map (lambda (symbol)
                    (define section
                      (cond
                        ((or
                          (eq? symbol road)
                          (eq? symbol finish)
                          (eq? symbol grass))
                         (ADT-field lane-idx section-idx symbol))
                        ((or
                          (eq? symbol coin)
                          (eq? symbol purple)
                          (eq? symbol blue)
                          (eq? symbol green)
                          (eq? symbol red)
                          (eq? symbol power-up)
                          (eq? symbol bush))
                         (ADT-object lane-idx section-idx symbol))))
                    (set! section-idx (+ section-idx 1))
                    section)
                  vec)))
  
  (define (get-lane-section section-numb)
    (vector-ref lane  section-numb))
  
  (define (section-for-each x)
    (vector-for-each lane
                     (lambda (section)(x section))))
  
  (define (dispatch-lane msg)
    (cond
      ((eq? msg 'get-section)get-lane-section)
      ((eq? msg 'section-for-each) section-for-each)
      (else (display "Wrong msg ADT-lane")
            (newline)
            (display msg))))
  dispatch-lane)


;-------------------------------------------------------------------------
; ADT-highway
; deze stelt de snelweg voor die het level is in het spel is
; bestaande uit verschillende rijstroken
; dit zijn geneste vectoren (matrix)  --> O(1) acces
; Dit ADT is geschreven door Brecht Rooms race-spe-met-gui-verbinding.rkt
;-------------------------------------------------------------------------

(define (ADT-highway nested-vec)
  
  (define highway
    (let ((lane-idx 0))
      (vector-map (lambda (matrix)
                    (define lane (ADT-lane matrix lane-idx))
                    (set! lane-idx (+ lane-idx 1))
                    lane)
                  nested-vec)))
  
  (define (get-lane lane-numb)   ; lane-numb is het nummer van de rijstrook
    (vector-ref highway lane-numb))
  
  (define (get-section y x)
    (vector-ref (get-lane y) x))
  
  (define (lane-length)
    (vector-length (vector-ref  nested-vec 0)))
  
  (define (section-for-each x)
    (vector-for-each highway
                     (lambda (lane) ((lane 'section-for-each) x))))
  (define (draw adt-draw)
    (adt-draw 'draw-highway))
  
  (define (clear-highway adt-draw)
    (adt-draw 'clear-highway))
  
  (define (dispatch-highway msg)
    (cond
      ((eq? msg 'get-lane) get-lane)
      ((eq? msg 'get-section) get-section)
      ((eq? msg 'draw)draw)
      ((eq? msg 'clear-highway)clear-highway)
      ((eq? msg 'number-of-lanes)(vector-length nested-vec))
      ((eq? msg 'lane-length)(lane-length))
      ((eq? msg 'section-for-each) section-for-each)
      ((eq? msg 'matrix) nested-vec)
      (else (display "Wrong msg ADT-highway")
            (newline)
            (display msg))))
  dispatch-highway)


;-----------------------------------------------------------
; ADT-car
;-----------------------------------------------------------

(define (ADT-car lane section colour adt-frogger highway)
  
  (let*
      ((car-x 0)
       (car-y 0)
       (next-x (+ car-x 1))
       (end-highway (highway 'lane-length))
       (drawable 0)
       (layer 0)
       (get-colour colour)
       (speed 0)
       (direction 0)
       (x-frogger (adt-frogger 'section))
       (y-frogger (adt-frogger 'lane)))
    
    (define (get-tag vec x y)
      (vector-ref
       (vector-ref vec y)
       x))
    
    
    (define (set-drawable x)
      (set! drawable x))
    
    (define (set-layer x)
      (set! layer x))
    
    (define (set-dir x)
      (set! direction x))
    
    (define (respawn)
      (set! car-x section)
      (set! car-y lane))
    
    (define (advance)
      (set! car-x (+ car-x speed)))

 ;--------------------------------------------------
 ; Deze predicaten testen of er een andere
 ; wagen op de volgende locatie van de
 ; wagen staat, zo ja dan kan de wagen niet bewegen
 ; zo nee dan mag de wagen niet bewegen
 ;--------------------------------------------------
    
    (define (can-advance? car2)
      (not
       (eq? (+ car-x speed) (car2 'section))))
    
    (define (can-go-up?  car2)
      (not
       (eq? (- car-y 1) (car2 'lane))))
    
    (define (can-go-down? car2)
      (not
       (eq? (+ car-y 1)(car2 'lane))))
    
  ;---------------------------------------------  
  ; Deze predicaten testen of de nieuwe locatie
  ; nog steeds op de rijbaan is
  ;---------------------------------------------
    
    (define (check-up)
      (eq? (get-tag (highway 'matrix) car-x (- car-y 1))
           road))
    
    
    (define (check-down)
      (eq? (get-tag (highway 'matrix) car-x (+ car-y 1))
           road))
    
    (define (go-up)
      (set! car-y (- car-y 1)))
    
    (define (go-down)
      (set! car-y (+ car-y 1)))
    
    
    
    (define (determine-colour)
      (cond
        ((eq? get-colour blue)'draw-car-blue)
        ((eq? get-colour red)'draw-car-red )
        ((eq? get-colour orange)'draw-car-orange)
        ((eq? get-colour pink)'draw-car-pink )
        (else
         (display "Non existand colour"))))
    
    
    (define (draw adt-draw)
      ((adt-draw (determine-colour))dispatch-car))
    
    (define (remove-drawable draw-adt)
      ((draw-adt 'disable-object-draw)dispatch-car))
    
    (define (move-car-tile draw-adt)
      ((draw-adt 'move-tile)dispatch-car))
    
    
    (define (random-move adt-car)
      (if (eq? get-colour 'orange)
          (cond
            ((and (= (random 3) 0)
                  (check-up)
                  (can-go-up? adt-car)
                  (go-up)
                  (advance)))
            ((and (= (random 3) 1)
                  (check-down)
                  (can-go-down? adt-car))
             (go-down)
             (advance))
            ((can-advance? adt-car)
             (advance)))
          (advance))
      (adt-car 'section))
    
    
    (define (speed-car adt-car)
      (if (eq? get-colour 'red)
          (cond
            ((can-advance? adt-car)
             (advance)))
          '()))
    
    (define (normal-car adt-car)
      (if (eq? get-colour 'pink)
          (cond
            ((can-advance? adt-car)
             (advance)))
          '()))
    
    (define (follow-frogger adt-car)
      (if (eq? get-colour blue)
          (cond
            ((= car-x (highway 'lane-length))(respawn))   ; probleem respauwn blauwe auto, extra respauwn in dit geval nodig
            ((and (> car-y (adt-frogger 'lane)) 
                  (check-up)
                  (can-go-up? adt-car))
             (go-up)
             (advance))
            ((and
              (check-down)
              (can-go-down? adt-car))
             (go-down)
             (advance)))
          '()))
    
    
    (define (move-car adt-car adt-draw adt-score )
      (cond
        ((eq? car-x end-highway) 
         (respawn))
        ((adt-frogger 'immortal)
         (random-move adt-car)
         (follow-frogger adt-car)
         (speed-car adt-car)
         (normal-car adt-car))
        ((and (or(= (+ car-x 1) (adt-frogger 'section))  ;or test is nodig anders is er geen collision met de achterkant van de wagen
                 (= car-x (adt-frogger 'section)))
              (= car-y (adt-frogger 'lane)))
         ((adt-frogger 'kill-frogger)adt-draw adt-score)
         ((adt-frogger 'move-frogger)adt-draw)) ;je moet advance niet meer oproepen na resetten -> coordinaten zijn verschillend
        (else (random-move adt-car)
              (follow-frogger adt-car)
              (speed-car adt-car)
              (normal-car adt-car)
              )))
    
    (define (init-car)
      (if (eq? get-colour 'red)
          (set! speed 2)
          (set! speed 1))
      (newline)
      (respawn))
    
    (init-car)
    
    ;verschil tussen move-car en move-car-tile
    ;move-car gaat de coordinaten van de auto aanpassen move-car-tile gaat de tile bewegen
    (define (dispatch-car msg)
      (cond
        ((eq? msg 'lane)car-y)
        ((eq? msg 'section)car-x)
        ((eq? msg 'set-drawable)set-drawable)
        ((eq? msg 'drawable)drawable)
        ((eq? msg 'layer)layer)
        ((eq? msg 'set-layer)set-layer)
        ((eq? msg 'remove-drawable)remove-drawable)
        ((eq? msg 'draw)draw)
        ((eq? msg 'init-car)(init-car))
        ((eq? msg 'set-layer)set-layer)
        ((eq? msg 'move-car-tile)move-car-tile)
        ((eq? msg 'move-car)move-car)
        (else
         (newline (display "wrong msg adt-car   ")(display msg)))))
    dispatch-car))


;---------------------------------------------------------
; ADT-frogger
;als je de tile in het object bijhoudt zoals in de andere
; adt's dan wordt de vorige tile niet verwijderd en
; krijg je meerdere van hetzelfde object op het scherm te zien
; de tile wordt in deze voorstelling verplaatst
;---------------------------------------------------------


(define (ADT-frogger game highway)
  
  
  (let*
      ((current-lane (- ( highway 'number-of-lanes)1))
       (current-section (/( highway 'lane-length)2))
       (drawable 0)
       (layer 0)
       (immortal #f)
       ( lane-length  (highway 'lane-length)))
    
    (define (set-drawable x)
      (set! drawable x))
    
    (define (set-layer x)
      (set! layer x))
    
    (define (switch-mortal)
      (set! immortal (not immortal)))
    
    
    (define (draw-frogger draw-adt)
      ((draw-adt 'draw-frogger) dispatch-frogger))
    
    (define (remove-drawable draw-adt)
      ((draw-adt 'disable-object-draw)dispatch-frogger))
    
    (define (draw-immortal draw-adt)
      (remove-drawable draw-adt)
      ((draw-adt 'draw-frogger-immortal)dispatch-frogger))
    
    (define (draw draw-adt)
      (draw-frogger draw-adt))
    
    (define (move-frogger draw-adt)
      ((draw-adt 'move-tile)dispatch-frogger))
    
    (define (mortal-again draw-adt)
      (set! immortal #f)
      (remove-drawable draw-adt)
      (draw-frogger draw-adt))
    
    ;--------------------------------
    ; Update de locatie van frogger
    ;--------------------------------
    
    (define (update-location c)
      (set! current-section (car c))
      (set! current-lane (cdr c)))
    
    ;----------------------------------------------------------------------------
    ; Berekend de volgende locatie afhankelijk van de richting waarin je beweegt
    ;----------------------------------------------------------------------------
    
    (define (next-position dir)
      (cond
        ((eq? dir 'left) (cons (- current-section 1 ) current-lane))
        ((eq? dir 'right) (cons (+ current-section 1) current-lane))
        (else (cons current-section (- current-lane 1)))))
    
    ;-----------------------------
    ; Haalt een tag uit de matrix
    ;-----------------------------
    
    (define (get-tag vec c)
      (vector-ref
       (vector-ref vec (cdr c))
       (car c)))
    
    ;----------------------
    ; ADT uit highway halen
    ;-----------------------
    
    (define (get-adt)
      (define lane ((highway 'get-lane)current-lane))
      (define section ((lane 'get-section) current-section))
      section)
    
    
    ;--------------------------------------------------------------------
    ; Gaat na wat er op een bepaalde locatie staat van de geneste vector
    ;---------------------------------------------------------------------
    
    (define (test-tag tag next draw-adt score-adt)
      (cond
        ((or(eq? tag coin)
            (eq? tag purple)
            (eq? tag blue)
            (eq? tag green))
         (update-location next)
         ((draw-adt 'disable-object-draw)(get-adt))
         ((score-adt 'incr-score)tag))
        ((eq? tag 'power-up)
         (update-location next)
         ((draw-adt 'disable-object-draw)(get-adt))
         (switch-mortal)
         (draw-immortal draw-adt))                            
        ((eq? tag bush))
        ((eq? tag finish)
         (game 'game-over))                  ; procedure won geeft een error, daarom wordt game-over in de 
        ((or (eq? tag road)                  ; plaats gebruikt
             (eq? tag grass))
         (update-location next)
         )))
    
    
    ;-----------------------------------------------
    ; Gaat na of de volgende locatie toegestaan is
    ;-----------------------------------------------
    
    (define (legal? next vec draw-adt score-adt)
      (if
       (and (> (car next) -1)
            (< (car next) lane-length)
            (> (cdr next)  -1))
       (test-tag
        (get-tag vec next)
        next draw-adt score-adt)
       (display "out of range")))
    
    (define (reset draw-adt)
      (set!  current-lane (-(highway 'number-of-lanes)1))
      (set! current-section (/(highway 'lane-length)2))
      (mortal-again draw-adt))
    
    
    ;de bewegingen in 3 richtingen
    (define (up draw-adt score-adt)
      (legal? (next-position 'up) (highway 'matrix) draw-adt score-adt))
    
    (define (left draw-adt score-adt)
      (legal? (next-position 'left) (highway 'matrix) draw-adt score-adt))
    
    (define (right draw-adt score-adt)
      (legal? (next-position 'right) (highway 'matrix) draw-adt score-adt))
    
    
    (define (kill-frogger draw-adt score-adt) ;frogger doden als het een wagen raakt
      (score-adt 'decr-lives)
      (if
       (= (score-adt 'lives) 0)
       (game 'game-over)
       (begin ((score-adt 'draw)draw-adt)                  ; game is een paramater van frogger-adt
              (reset draw-adt))))
    
    
    (define (dispatch-frogger msg)
      (cond 
        ((eq? msg 'up)up)
        ((eq? msg 'left)left) 
        ((eq? msg 'right)right) 
        ((eq? msg 'reset)reset)
        ((eq? msg 'draw)draw)
        ((eq? msg 'immortal)immortal)
        ((eq? msg 'remove-drawable)remove-drawable)
        ((eq? msg 'mortal-again)mortal-again)
        ((eq? msg 'set-drawable)set-drawable)
        ((eq? msg 'set-layer)set-layer)
        ((eq? msg 'drawable)drawable)
        ((eq? msg 'layer)layer)
        ((eq? msg 'lane)current-lane)
        ((eq? msg 'section)current-section)
        ((eq? msg 'move-frogger)move-frogger)
        ((eq? msg 'kill-frogger)kill-frogger)
        (else (display "Wrong msg ADT-frogger")
              (newline)
              (display msg))))
    dispatch-frogger))

;----------------------------------------------------------------------------------
; ADT-game
; Hier gebeuren alle logische tests en wordt er gepast gerageerd op het resultaat
;----------------------------------------------------------------------------------



(define (ADT-game lvl-vec)
  
  (define (dispatch-game msg)
    (cond
      ((eq? msg 'start)(start))
      ((eq? msg 'restart)(restart))
      ;  ((eq? msg 'won)(won))
      ((eq? msg 'current-level)current-level)
      ((eq? msg 'game-over)(game-over))
      (else
       (display "Wrong msg ADT-game  ")
       (display msg)
       (newline))))
  
  
  (define current-level 0)
  (define current-lvl-nr 0)
  
  (define (get-level)
    (vector-ref lvl-vec current-lvl-nr))
  
  (define (set-level!)
    (set! current-level (get-level)))
  
  (define (next-level)
    (set! current-lvl-nr
          (+ current-lvl-nr 1))
    (set-level!))
  
  (set-level!)
  
  (define level (ADT-highway current-level))
  (define frogger (ADT-frogger dispatch-game level))
  
  (define lane1-car (make-vector  3))
  (vector-set! lane1-car 0 (ADT-car 1 0 red frogger level))
  (vector-set! lane1-car 1 (ADT-car 3 0 blue frogger level))
  (vector-set! lane1-car 2 (ADT-car 4 0 orange frogger level))
  
  (define score (ADT-score))
  (define menu (ADT-menu 1 2))
  (define drawer (ADT-draw level frogger 100 70))
  
  (define (on-key-input key)
    (cond
      ((or (eq? key 'up)
           (eq? key 'left)
           (eq? key 'right))
       ((frogger key) drawer score)
       ((frogger 'move-frogger)drawer)
       ((score 'draw)drawer)
       (score 'update-highscore))
      ((eq? key 'down)
       ((frogger 'kill-frogger)drawer score)
       ((frogger 'move-frogger)drawer))
      ((eq? key #\space)
       (start))
      ((eq? key #\e)
       (restart))))
  
  (define (obj-for-all vec x)
    (vector-for-each vec
                     (lambda (object)(x object))))
  
  
  (define (car-action vec tag)
    (obj-for-all vec
                 (lambda (obj)
                   (obj tag))))
  
  (define (reset-car vec)
    (car-action vec 'init-car))
  
  (define (car-actions vec tag adt)
    (obj-for-all vec
                 (lambda (obj)
                   ((obj tag)adt))))
  
  (define (draw-cars vec)
    (car-actions vec 'draw drawer))
  
  (define (car-tile-move vec)
    (car-actions vec 'move-car-tile drawer))
  
  (define (car-test vec)  
    (vector-for-each vec (lambda (car1)
                           (vector-for-each vec (lambda (car2)
                                                  (if (and (not (eq? car1 car2))
                                                           ((car1 'move-car)car2 drawer score))
                                                      ((car1 'move-car-tile)drawer)
                                                      '()
                                                      ))
                                            ))
                     ))
  
  (define timer 0)
  (define cycles 0)
  
  (define (move-all-cars delta-time)
    (set! timer (+ timer delta-time))
    (set! cycles (+ cycles 1))
    (car-tile-move lane1-car)
    (if (> timer 1000)
        (begin
          (car-test lane1-car)
          (set! timer (- timer 1000)))
        '())
    (if (= cycles 210)
        (begin
          ((frogger 'mortal-again)drawer)
          (set! cycles (- cycles 210)))
        '()))
  
  (define (clear-all vec draw-adt)
    (draw-adt 'clear-highway)
    ((frogger 'remove-drawable)draw-adt)
    (car-actions lane1-car 'remove-drawable drawer))
  
  (define (init)
    ((drawer 'set-key!) on-key-input)
    ((menu 'draw-start-screen)drawer)
    (score 'reset)
    (set-level!))
  
  (define (game-over)
    (clear-all lane1-car drawer)
    ((menu 'draw-game-over)drawer score)
    ((frogger 'remove-drawable)drawer)
    ((drawer 'set-game-loop!)(lambda (x)#f)))
  
  
  ;onderstaande procedure werkt niet 
  ;  (define (won)
  ;  ((drawer 'set-game-loop!)(lambda (x)#f))
  ;  (clear-all lane1-car drawer)
  ;  ((frogger 'remove-drawable)drawer)
  ;  (next-level)
  ;  (start))
  
  (define (restart)
    ((menu 'remove-drawable)drawer)
    (init))
  
  (define (start)
    ((menu 'remove-drawable)drawer)
    (frogger 'reset)
    (reset-car lane1-car)
    ((frogger 'draw)drawer)
    ((score 'draw)drawer)
    ((level 'draw)drawer)
    (car-actions lane1-car 'draw drawer)
    ((drawer 'set-key!) on-key-input)
    ((drawer 'set-game-loop!)move-all-cars))
  
  (init)
  
  dispatch-game)


(define game (ADT-game level-vector))

