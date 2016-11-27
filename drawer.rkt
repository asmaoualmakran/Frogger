#lang racket
;--------------------------------------------
;Programmeerproject 1 academiejaar 2014-2015
;Frogger
;Asma oualmakran (asoualma@vub.ac.be)
;-------------------------------------------

(#%require "Graphics.rkt")
(provide ADT-draw)


(define (ADT-draw highway 
                  frogger  hor-size ver-size)
  
  (let* ((text-hight 50)
         (ver-sections (highway 'number-of-lanes))
         (hor-sections (highway 'lane-length))
         (ver-pixels (* ver-size ver-sections))
         (hor-pixels (* hor-size hor-sections)))
    
    (define window (make-window hor-pixels ver-pixels "Frogger"))
    
    (define bg-layer (window 'make-layer))
    (define active-layer (window 'make-layer)) ;hierop kommen alle blokkades en opraapbare componenten
    (define frogger-layer (window 'make-layer))
    (define cars-layer (window 'make-layer))
    (define text-layer (window 'make-layer))
    
    
    ;gaat de snelweg tekenen (alle onderdelen die in de matrix zitten)
    ;------------------------
    (define (getter s)
      ((highway 'section-for-each)
       (lambda (section)
         ((section s) dispatch-draw))))
    
    ;---------------------------
    ; een tile verplaatsten
    ;----------------------------
    (define (move-tile adt)   
      (let
          ((tile (adt 'drawable)))
        ((tile 'set-x!) (* hor-size (adt 'section)))
        ((tile 'set-y!) (* ver-size (adt 'lane)))))
    
    ;---------------------------------------
    ; tile verwijderen
    ;----------------------------------------
    
    (define (disable-object-draw object-adt)
      (let
          ((drawable (object-adt 'drawable))
           (layer (object-adt 'layer)))
        ((layer 'remove-drawable) drawable)))
    
   
    
    (define (draw-highway)
      (getter 'draw))

    
    (define (clear-highway)
      (getter 'remove-drawable))
    
    ;gaat de grasstukken tekenen
    
    (define (draw-grass grass-adt)
      (define grass-tile (make-bitmap-tile "berm.png"))
      ((bg-layer 'add-drawable) grass-tile)
      ((grass-tile 'set-x!)(* hor-size (grass-adt 'section)))
      ((grass-tile 'set-y!)(* ver-size (grass-adt 'lane)))
      ((grass-adt 'set-drawable)grass-tile)
      ((grass-adt 'set-layer)bg-layer)
    )
    
    ;gaat een stuk asfalt tekenen
    (define (draw-road road-adt)
      (define road-tile (make-bitmap-tile "road.png"))
      ((bg-layer 'add-drawable) road-tile)
      ((road-tile 'set-x!)(* hor-size(road-adt 'section)))
      ((road-tile 'set-y!) (* ver-size (road-adt 'lane)))
      ((road-adt 'set-drawable)road-tile)
      ((road-adt 'set-layer)bg-layer))

    ;gaat een munt tekenen
    (define (draw-coin object-adt)
      (define coin-tile (make-bitmap-tile "coin.png" "coin-mask.png"))
      ((active-layer 'add-drawable) coin-tile)
      ((coin-tile 'set-x!) (* hor-size (object-adt 'section)))
      ((coin-tile 'set-y!) (* ver-size (object-adt 'lane)))
      ((object-adt 'set-drawable)coin-tile)
      ((object-adt 'set-layer)active-layer))

    ;gaat een paarse paddenstoel tekenen
    (define (draw-purple object-adt)
      (define purple-tile (make-bitmap-tile "purple-shroom.png" "shroom-mask.png"))
      ((active-layer 'add-drawable) purple-tile)
      ((purple-tile 'set-x!) (* hor-size (object-adt 'section)))
      ((purple-tile 'set-y!) (* ver-size (object-adt 'lane)))
      ((object-adt 'set-drawable)purple-tile)
      ((object-adt 'set-layer)active-layer))

    ;gaat een blauwe paddenstoel tekenen
    (define (draw-blue object-adt)
      (define blue-tile (make-bitmap-tile "blue-shroom.png" "shroom-mask.png"))
      ((active-layer 'add-drawable) blue-tile)
      ((blue-tile 'set-x!) (* hor-size (object-adt 'section)))
      ((blue-tile 'set-y!) (* ver-size (object-adt 'lane)))
      ((object-adt 'set-drawable)blue-tile)
      ((object-adt 'set-layer)active-layer))

    ;gaat een groene paddenstoel tekenen
    (define (draw-green object-adt)
      (define green-tile (make-bitmap-tile "green-shroom.png" "shroom-mask.png"))
      ((active-layer 'add-drawable) green-tile)
      ((green-tile 'set-x!) (* hor-size (object-adt 'section)))
      ((green-tile 'set-y!) (* ver-size (object-adt 'lane)))
      ((object-adt 'set-drawable)green-tile)
      ((object-adt 'set-layer)active-layer))

    ;gaat een power-up ster tekeken 
    (define (draw-power-up object-adt)
      (define power-up-tile (make-bitmap-tile "power-up.png" "power-up-mask.png"))
      ((active-layer 'add-drawable) power-up-tile)
      ((power-up-tile 'set-x!) (* hor-size (object-adt 'section)))
      ((power-up-tile 'set-y!) (* ver-size (object-adt 'lane)))
      ((object-adt 'set-drawable)power-up-tile)
      ((object-adt 'set-layer)active-layer))
    
    ;gaat struiken tekenen
    (define (draw-bush bush-adt)
      (define bush-tile (make-bitmap-tile "bush.png" "bush-mask.png"))
      ((active-layer 'add-drawable)bush-tile)
      ((bush-tile 'set-x!)(* hor-size(bush-adt 'section)))
      ((bush-tile 'set-y!)(* ver-size(bush-adt 'lane)))
     ((bush-adt 'set-drawable)bush-tile)
      ((bush-adt 'set-layer)active-layer))
    
    ;text tile
    
    (define text-tile (make-tile hor-pixels text-hight))
    ((text-tile 'set-y!) (- ver-pixels text-hight))
    ((text-layer 'add-drawable) text-tile)
    
   (define (delete-text)
      (text-tile 'clear))
    
    (define (write text colour)
      (delete-text)
      ((text-tile  'draw-text) text 12 10 10 colour))

    
    ; frogger tile
    
    (define (draw-frogger frogger-adt)
      (define frogger-tile (make-bitmap-tile "mario.png" "mario-mask.png"))
      ((frogger-layer 'add-drawable)frogger-tile)
      ((frogger-tile 'set-x!)(*  hor-size 
                                 (frogger-adt 'section)))
      ((frogger-tile 'set-y!) (*   ver-size 
                                   (frogger-adt 'lane)))
      ((frogger-adt 'set-drawable)frogger-tile)
      ((frogger-adt 'set-layer)frogger-layer))
    
    (define (draw-frogger-immortal frogger-adt)
      (define immortal-tile (make-bitmap-tile "mario2.png" "mario-mask.png"))
      ((frogger-layer 'add-drawable)immortal-tile)
      ((immortal-tile 'set-x!)(*  hor-size 
                                  (frogger-adt 'section)))
      ((immortal-tile 'set-y!) (*   ver-size 
                                    (frogger-adt 'lane)))
      ((frogger-adt 'set-drawable)immortal-tile))

    ;Blauwe auto
    
    (define (draw-car-blue car-adt)
      (define car-tile-blue (make-bitmap-tile "blue-car.png" "car-mask.png"))
      ((cars-layer 'add-drawable)car-tile-blue)
      ((car-tile-blue 'set-x!)(* hor-size (car-adt 'section)))
      ((car-tile-blue 'set-y!)(* ver-size (car-adt 'lane)))
      ((car-adt 'set-drawable) car-tile-blue)
      ((car-adt 'set-layer)cars-layer))
    
    ; Roze auto
    
    (define (draw-car-pink car-adt)
      (define car-tile-pink (make-bitmap-tile "pink-car.png" "car-mask.png"))
      ((cars-layer 'add-drawable)car-tile-pink)
      ((car-tile-pink 'set-x!)(* hor-size (car-adt 'section)))
      ((car-tile-pink 'set-y!)(* ver-size (car-adt 'lane)))
      ((car-adt 'set-drawable) car-tile-pink)
      ((car-adt 'set-layer)cars-layer))
    
    ;Oranje auto
    
    (define (draw-car-orange car-adt)
      (define car-tile-orange (make-bitmap-tile "orange-car.png" "car-mask.png"))
      ((cars-layer 'add-drawable)car-tile-orange)
      ((car-tile-orange 'set-x!)(* hor-size (car-adt 'section)))
      ((car-tile-orange 'set-y!)(* ver-size (car-adt 'lane)))
      ((car-adt 'set-drawable) car-tile-orange)
      ((car-adt 'set-layer)cars-layer))
    
    ;rode auto
    
    (define (draw-car-red car-adt)
      (define car-tile-red (make-bitmap-tile "red-car.png" "car-mask.png"))
      ((cars-layer 'add-drawable)car-tile-red)
      ((car-tile-red 'set-x!) (* hor-size (car-adt 'section)))
      ((car-tile-red 'set-y!) (* ver-size (car-adt 'lane)))
      ((car-adt 'set-drawable) car-tile-red)
      ((car-adt 'set-layer) cars-layer))
    
    ;gaat het start-scherm tekenen
    (define (draw-start-screen menu-adt)
      (define start-screen-tile (make-bitmap-tile "start-screen.png"))
      ((active-layer 'add-drawable)start-screen-tile)
      ((start-screen-tile 'set-x!)(* hor-size (menu-adt 'section)))
      ((start-screen-tile 'set-y!)(* ver-size (menu-adt 'lane)))
      ((menu-adt 'set-drawable)start-screen-tile)
      ((menu-adt 'set-layer)active-layer))

       ;gaat het scherm tekenen wanneer de speler verliest
    (define (draw-game-over menu-adt)
      (define game-over-tile (make-bitmap-tile "game-over.jpg"))
       ((active-layer 'add-drawable)game-over-tile)
      ((game-over-tile 'set-x!)(* hor-size (menu-adt 'section)))
      ((game-over-tile 'set-y!)(* ver-size (menu-adt 'lane)))
      ((menu-adt 'set-drawable)game-over-tile)
      ((menu-adt 'set-layer)active-layer))
    
    
    (define (set-game-loop! x)
      ((window 'set-update-callback!)x))
    
    (define (set-key! x)
      ((window 'set-key-callback!)x))
    
    (define (dispatch-draw msg)
      (cond
        ((eq? msg 'draw-grass)draw-grass)
        ((eq? msg 'draw-road)draw-road)
        ((eq? msg 'draw-coin)draw-coin)
        ((eq? msg 'draw-purple)draw-purple)
        ((eq? msg 'draw-blue)draw-blue)
        ((eq? msg 'draw-green)draw-green)
        ((eq? msg 'draw-power-up)draw-power-up)
        ((eq? msg 'disable-object-draw)disable-object-draw)
        ((eq? msg 'move-tile)move-tile)
        ((eq? msg 'draw-bush)draw-bush) 
        ((eq? msg 'draw-frogger)draw-frogger)
        ((eq? msg 'draw-frogger-immortal)draw-frogger-immortal)
        ((eq? msg 'draw-car-blue)draw-car-blue)
        ((eq? msg 'draw-car-pink)draw-car-pink)
        ((eq? msg 'draw-car-orange)draw-car-orange)
        ((eq? msg 'draw-car-red)draw-car-red)
        ((eq? msg 'draw-start-screen)draw-start-screen)
        ((eq? msg 'draw-game-over)draw-game-over)
        ((eq? msg 'write-text)write)
        ((eq? msg 'delete-text)(delete-text))
        ((eq? msg 'draw-highway)(draw-highway))
        ((eq? msg 'clear-highway)(clear-highway))
        ((eq? msg 'set-key!)set-key!)
        ((eq? msg 'set-game-loop!)set-game-loop!)
        
        (else (display "Wrong msg ADT-draw")
              (newline)
              (display msg))))
    dispatch-draw))