#lang typed/racket/gui
(require typed/mrlib/gif)
(require "qtree.rkt")
  
(: viewport-offset coordinate)
(define viewport-offset (coordinate -392 -381))
  
(: time-scale Nonnegative-Real)
(define time-scale 0.01)

(: cell-scale Nonnegative-Integer)
(define cell-scale 8)
(define canvas-size 800)

(define living-cells null)

(: show-overlay Boolean)
(define show-overlay #f)

(: advance-frame Boolean)
(define advance-frame #f)

(: rewind-frame Boolean)
(define rewind-frame #f)

(: rewind Boolean)
(define rewind #f)

(define error-box (new dialog% [label "Load error!"]))

(define error-msg (new message% [parent error-box] [auto-resize #t] [label ""]))
 
(void (new button% [parent error-box]
           [label "OK"]
           [callback (lambda (button event)
                       (send error-box show #f))]))

; Given a coordinate and a Quadtree, determine if it survives.
(: determine-cell-state (-> Quadtree coordinate Boolean))
(define (determine-cell-state population cell)
  (let ([living-neighbors (quadtree-neighbors population cell)]
        [orig-state (quadtree-member? population cell)])
    (cond
      [(< living-neighbors 2) #f]             ; if the cell has fewer than 2 neighbors, it dies.
      [(equal? living-neighbors 3) #t]        ; if the cell has exactly 3 neighbors, it comes to life.
      [(> living-neighbors 3) #f]             ; if the cell has more than 3 neighbors, it dies.
      [else orig-state]))) 


(: tick (-> Quadtree Quadtree))
(define (tick living-cells)
  (let ([cell-list (descendant-coordinates living-cells)])
    (quadtree-fold null
                   (get-livers cell-list living-cells null null))))

(: get-livers (-> (Listof coordinate) Quadtree Quadtree (Listof coordinate) (Listof coordinate)))
(define (get-livers cell-list living-cells [seen null] [acc null])
  (cond [(null? cell-list) acc]
        [else
         (let* ([cell (first cell-list)]
                [neighbors (neighbor-coords cell)]
                [unseen (filter (lambda ([n : coordinate]) (not (quadtree-member? seen n))) neighbors)]
                [alive? (curry determine-cell-state living-cells)])
           (get-livers (rest cell-list) living-cells (quadtree-fold seen unseen) (append (filter alive? unseen) acc)))]))

(define better-frame% (class frame% (super-new)
                        (define (on-close)
                          (exit))
                        (augment on-close)))

(define frame (new better-frame% [label "Conway's Game of Life"] [width canvas-size] [height canvas-size]))

(: is-panning (Listof coordinate))
(define is-panning null)

(: current-mouse-pos coordinate)
(define current-mouse-pos (coordinate 0 0))
  
(: new-mouse-cells (Listof coordinate))
(define new-mouse-cells null)
  
(: kill-mouse-cells (Listof coordinate))
(define kill-mouse-cells null)
  
(: brush-scale Integer)
(define brush-scale 1)
  
(: make-brush (-> Integer Integer Integer (Listof coordinate)))
(define (make-brush scale event-x event-y)
  (let ([x-offset : Integer (coordinate-x viewport-offset)]
        [y-offset : Integer (coordinate-y viewport-offset)])
    (append-map (lambda ([x : Integer])
                  (map (lambda ([y : Integer])
                         (coordinate (+ x x-offset) (+ y y-offset)))
                       (range (- (floor (/ event-y cell-scale)) scale) (+ (floor (/ event-y cell-scale)) (+ scale 1)))))
                (range (- (floor (/ event-x cell-scale)) scale) (+ (floor (/ event-x cell-scale)) (+ scale 1))))))

(: time-scale-up (-> Positive-Real))
(define (time-scale-up)
  (cond [(>= time-scale 0.8) 0.8]
        [else (+ 0.01 (abs time-scale))]))

(: time-scale-down (-> Nonnegative-Real))
(define (time-scale-down)
  (cond [(<= time-scale 0.01) 0.01]
        [else (abs (- time-scale 0.01))]))
  
(: cell-scale-up (-> Nonnegative-Integer))
(define (cell-scale-up)
  (cond [(>= cell-scale 100) 100]
        [else (+ 1 (abs cell-scale))]))

(: cell-scale-down (-> Nonnegative-Integer))
(define (cell-scale-down)
  (cond [(<= cell-scale 4) 4]
        [else (abs (- cell-scale 1))]))

; ronder
(: render-frame (-> (Listof coordinate) (Instance Bitmap%)))
(define (render-frame cells)
  (let* ([width (send frame get-width)]
         [height (send frame get-height)]
         [target (make-bitmap (if (positive-integer? width) width 1) (if (positive-integer? height) height 1))]
         [dc : (Instance Bitmap-DC%) (new bitmap-dc% [bitmap target])])
    (send dc set-background "black")
    (send dc set-pen "black" 1 'transparent)
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle 0 0 width height)
    (send dc set-brush cell-color 'solid)
    (send dc set-pen cell-color 1 'solid)
    (recursidraw dc cells) target))

(: export-threads (Listof (Pairof Thread Output-Port)))
(define export-threads null) 

(: gif-threads (Listof Thread))
(define gif-threads null) 

(: current-cell-state Quadtree)
(define current-cell-state null)

(: import-cells Quadtree)
(define import-cells null)
  
(: export-cell-state (-> Quadtree Void))
(define (export-cell-state quadtree)
  (let ([export-path (put-file "Export cell state"
                               frame
                               (find-system-path 'desk-dir)
                               "cells.gol"
                               "gol"
                               (list 'common)
                               (list (list "Game of Life state" "*.gol")))])
    (cond [(path? export-path)
           (let ([fh (open-output-file export-path #:exists 'replace)])
             (set! export-threads
                   (list (cons
                          (thread
                           (lambda ()
                             (write
                              ((inst map (Pairof Integer Integer) coordinate)
                               (lambda ([c : coordinate])
                                 (cons (coordinate-x c) (coordinate-y c)))
                               (descendant-coordinates quadtree)) fh))) fh))))]
          [else (void)])))

(: import-cell-state (-> Void))
(define (import-cell-state)
  (let ([import-path (get-file "Import cell state"
                               frame
                               (find-system-path 'desk-dir)
                               #f
                               "gol"
                               (list 'common)
                               (list (list "Game of Life state" "*.gol")))])
    (cond [(path? import-path)
           (let ([file-input (get-cells-from-file import-path)])
             (cond [(list? file-input)
                    (set! import-cells
                          (quadtree-fold null
                                         (filter-map
                                          (Î» (c) (let ([x (if (pair? c) (car c) null)]
                                                       [y (if (pair? c) (cdr c) null)])
                                                   (if (and (exact-integer? x)
                                                            (exact-integer? y))
                                                       (coordinate x y) #f)))
                                          file-input)))]
                   [else (begin (send error-msg set-label "Unknown filetype or corrupt data!") (send error-box show #t))]))]
          [else (void)])))

(: get-cells-from-file (-> Path-String Any))
(define (get-cells-from-file path)
  (let* ([fh (open-input-file path)]
         [cells (read fh)])
    (close-input-port fh)
    cells))

(: write-gif-to-desired-path (-> (Listof Quadtree) Void))
(define (write-gif-to-desired-path history)
  (let ([gif-path (put-file "Save as a gif"
                            frame
                            (find-system-path 'desk-dir)
                            "game of life.gif"
                            "*.gif"
                            (list 'common)
                            (list (list "Animated gif" "*.gif")))])
    (cond [(path? gif-path)
           (set! gif-threads (append gif-threads
                                     (list (thread (lambda ()(write-animated-gif (map (lambda ([qt : Quadtree]) (render-frame (descendant-coordinates qt)))
                                                                                      (reverse history)) 10 gif-path #:one-at-a-time? #t))))))
           (void)]
          [else (void)])))
  
(define better-canvas% (class canvas%
                         (define/override (on-char ev)
                           (let ([kc (send ev get-key-code)])
                             (cond [(eq? kc 'f1)
                                    (let
                                        ([midwidth : Real (+ (coordinate-x viewport-offset) (/ (/ (send frame get-width) cell-scale) 2))]
                                         [midheight : Real (+ (coordinate-y viewport-offset) (/ (/ (send frame get-height) cell-scale) 2))])
                                      (fprintf (current-output-port) "Midwidth: ~s Midheight: ~s\n" (exact-round midwidth) (exact-round midheight))
                                      (fprintf (current-output-port) "offsets: ~s ~s\n" (coordinate-x viewport-offset) (coordinate-y viewport-offset)))]
                                   [(eq? kc 'f11) (send frame fullscreen (not (send frame is-fullscreened?)))]
                                   [(eq? kc 'escape) (send frame fullscreen #f)]
                                   [(eq? kc #\1) (set! cell-color "green")]
                                   [(eq? kc #\2) (set! cell-color "red")]
                                   [(eq? kc #\3) (set! cell-color "orange")]
                                   [(eq? kc #\4) (set! cell-color "yellow")]
                                   [(eq? kc #\5) (set! cell-color "blue")]
                                   [(eq? kc #\6) (set! cell-color "indigo")]
                                   [(eq? kc #\7) (set! cell-color "violet")]
                                   [(eq? kc #\8) (set! brush-scale 0)]
                                   [(eq? kc #\9) (set! brush-scale 1)]
                                   [(eq? kc #\0) (set! brush-scale 3)]
                                   [(eq? kc #\k) (set! genocide #t)(set! game-paused #t)]
                                   [(eq? kc #\w) (set! viewport-offset (coordinate (coordinate-x viewport-offset)
                                                                                   (- (coordinate-y viewport-offset) (exact-round (* 0.1 (/ (send frame get-height) cell-scale))) )))]
                                   [(eq? kc #\a) (set! viewport-offset (coordinate (- (coordinate-x viewport-offset) (exact-round (* 0.1 (/ (send frame get-width)cell-scale))) ) (coordinate-y viewport-offset)))]
                                   [(eq? kc #\d) (set! viewport-offset (coordinate (+ (coordinate-x viewport-offset) (exact-round (* 0.1 (/ (send frame get-width) cell-scale))) ) (coordinate-y viewport-offset)))]

                                     
                                   [(eq? kc #\s) (set! viewport-offset (coordinate (coordinate-x viewport-offset)
                                                                                   (+ (coordinate-y viewport-offset) (exact-round (* 0.1 (/ (send frame get-height) cell-scale))) )))]
                                   [(eq? kc #\g) (write-gif-to-desired-path history)]
                                   [(eq? kc #\e) (export-cell-state current-cell-state)]
                                   [(eq? kc #\i) (import-cell-state)]
                                   [(eq? kc #\o) (set! show-overlay (not show-overlay))]
                                   [(eq? kc 'down) (set! time-scale (time-scale-up))]
                                   [(eq? kc 'up) (set! time-scale (time-scale-down))]
                                   [(eq? kc 'left)  (begin (set! rewind-frame #t))]
                                   [(eq? kc 'right) (begin (set! advance-frame #t))]
                                   [(eq? kc 'wheel-up)
                                    (let* ([oldscale  : Real (/ 1 cell-scale)]
                                           [bs : Void (set! cell-scale (cell-scale-up))] ; it started out so functional and pure.
                                           [scale-diff : Real (- (/ 1 cell-scale) oldscale)]
                                           [current-mouse-x  : Real (coordinate-x current-mouse-pos)]
                                           [current-mouse-y : Real (coordinate-y current-mouse-pos)])
                                      (set! viewport-offset (coordinate
                                                             (exact-round (+ (coordinate-x viewport-offset) (* current-mouse-x scale-diff -1)))
                                                             (exact-round (+ (coordinate-y viewport-offset) (* current-mouse-y scale-diff -1))))))]
                                   [(eq? kc 'wheel-down)
                                    (let* ([oldscale  : Real (/ 1 cell-scale)]
                                           [bs : Void (set! cell-scale (cell-scale-down))] ; it started out so functional and pure.
                                           [scale-diff : Real (- oldscale (/ 1 cell-scale))]
                                           [current-mouse-x  : Real (coordinate-x current-mouse-pos)]
                                           [current-mouse-y : Real (coordinate-y current-mouse-pos)])
                                      (set! viewport-offset (coordinate
                                                             (exact-round (- (coordinate-x viewport-offset) (* current-mouse-x scale-diff -1)))
                                                             (exact-round (- (coordinate-y viewport-offset) (* current-mouse-y scale-diff -1))))))]

                                   [(eq? kc #\r) (begin (set! rewind #t))]
                                   [(eq? kc #\k) (set! genocide #t)(set! game-paused #t)]
                                   [(eq? kc #\space) (set! game-paused (not game-paused))])))
                         (define/override (on-event event)
                           (cond [(or (send event button-down? 'right) (send event get-right-down))
                                  (set! kill-mouse-cells (make-brush brush-scale (send event get-x) (send event get-y)))]
                                 [(or (send event button-down? 'left) (send event get-left-down))
                                  (set! new-mouse-cells (make-brush brush-scale (send event get-x) (send event get-y)))]
                                 [(send event button-down? 'middle) (set! is-panning (list (coordinate (send event get-x) (send event get-y))))]

                                 [(and (send event dragging?) is-panning)
                                  (set! viewport-offset
                                        (coordinate
                                         (+ (exact-round
                                             (/ (- (coordinate-x (first is-panning)) (send event get-x)) cell-scale))
                                            (coordinate-x viewport-offset))
                                         (+ (exact-round
                                             (/ (- (coordinate-y (first is-panning)) (send event get-y)) cell-scale)) (coordinate-y viewport-offset))))
                                  (set! is-panning (list (coordinate (send event get-x) (send event get-y))))]
                                 [(send event moving?) (set! current-mouse-pos (coordinate (send event get-x) (send event get-y)))]
                                 [(send event button-up? 'middle) (set! is-panning null)]))(super-new)))
              
(define canvas (new better-canvas% [parent frame]
                    [paint-callback
                     (lambda (c dc)
                       (send dc set-pen "black" 1 'transparent)
                       (send dc set-brush "black" 'solid)
                       (send dc draw-rectangle 0 0 (send frame get-width) (send frame get-height)))]))

(: cell-color String)
(define cell-color "green")

(: genocide Boolean)
(define genocide #f)

(: game-paused Boolean)
(define game-paused #t)
  
(: draw-cell (-> (Instance DC<%>) Integer Integer Void))
(define (draw-cell dc x y)
  (let ([x-offset : Integer (coordinate-x viewport-offset)]
        [y-offset : Integer (coordinate-y viewport-offset)])
    (send dc set-brush cell-color 'solid)
    (send dc set-pen "black" 1 'solid)
    (send dc draw-rectangle
          (* cell-scale (- x x-offset))
          (* cell-scale (- y y-offset)) cell-scale cell-scale)))
  
(: recursidraw (-> (Instance DC<%>) (Listof coordinate) (Instance DC<%>)))
(define (recursidraw dc cells)
  (cond
    [(empty? cells) dc]
    [else   (let* ([cell : coordinate (first cells)]
                   [x : Integer (coordinate-x cell)]
                   [y : Integer (coordinate-y cell)])
              (draw-cell dc x y)
              (recursidraw dc (rest cells)))]))

(: history (Listof Quadtree))
(define history null)
  
(: evolve (-> Quadtree Any))
(define (evolve quadtree)
  (let* ([coord-list (descendant-coordinates quadtree)]
         [dc (send canvas get-dc)]
         [x null]
         [updated-quadtree (quadtree-nuke (quadtree-fold quadtree new-mouse-cells) kill-mouse-cells)]
         [next (cond [(not (null? import-cells)) (let ([i : Quadtree import-cells]) (set! import-cells null)(set! history null)(set! game-paused #t) i)]
                     [genocide (set! genocide #f) (set! history null) null]
                     [rewind (begin (set! game-paused #t)(set! rewind #f)(let ([h history]) (if (null? history) updated-quadtree (begin (set! history null)(last h)))))]
                     [advance-frame (begin (set! game-paused #t) (set! advance-frame #f)
                                           (set! history (append (list updated-quadtree) history))(tick updated-quadtree))]
                     [rewind-frame  (begin (set! game-paused #t) (set! rewind-frame #f)
                                           (cond [(null? history) updated-quadtree]
                                                 [(eq? (length history) 1) (begin (let ([h history]) (set! history null) (first h)))]
                                                 [else (let ([h history]) (begin (set! history (rest history)) (first h)))]))]
                     [game-paused updated-quadtree]
                     [else (tick updated-quadtree)])])
    (set! current-cell-state next)
    (set! new-mouse-cells null)
    (set! kill-mouse-cells null)
    (send canvas suspend-flush)
    (send canvas on-paint)
    (recursidraw dc coord-list)
    (send dc set-text-foreground "blue")
    (cond [show-overlay (begin
                          (send dc draw-text (string-append "Coordinates:  X " (number->string (+ (coordinate-x viewport-offset) (coordinate-x current-mouse-pos))) "  Y "
                                                            (number->string (* -1 (+ (coordinate-y viewport-offset) (coordinate-y current-mouse-pos) )))) 40 20)
                          (send dc draw-text (string-append "Time scale: " (real->decimal-string (* (/ 0.01 time-scale) 100)) "%") 40 40)
                          (send dc draw-text (string-append "Population: " (number->string (length (descendant-coordinates next)))) 40 60)
                          (send dc draw-text (string-append "Step: " (number->string (length history))) 40 80))])
    (send dc draw-text "rob@robertlavery.com" (- (send frame get-width) 200) (- (send frame get-height) 90))
    (send dc set-text-foreground "green")
    (cond [(not (null? gif-threads)) (send dc draw-text "Saving gif..." 50 (- (send frame get-height) 90))])
    (cond [(not (null? export-threads)) (send dc draw-text "Exporting..." 90 (- (send frame get-height) 90))])
    (set! gif-threads (filter (lambda ([t : Thread]) (not (thread-dead? t))) gif-threads))
    (for ([thread : (Pairof Thread Output-Port) export-threads]) (cond [(thread-dead? (car thread)) (close-output-port (cdr thread))(set! export-threads null)]))
    (send canvas resume-flush)
    (send canvas flush)
    (cond [(and (not game-paused)
                (not (null? updated-quadtree))
                (or (< (length history) 2)
                     (not (member updated-quadtree history))))
           (set! history (append (list updated-quadtree) history))])
    (sleep/yield time-scale)
    (evolve next)))
(void
 (send frame show #t)
 (evolve living-cells))
