;
;    GAMEOFDEATH: A Game of Life Toy
;    Copyright (C) 2019 Robert Lavery rob@robertlavery.com

;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.

;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.

;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <https://www.gnu.org/licenses/>.


#lang typed/racket/gui
(require typed/mrlib/gif)
(require "qtree.rkt")
  
(: viewport-offset coordinate)
(define viewport-offset (coordinate -50 -50))

(define VERSION "1.0.0")

(: time-scale Nonnegative-Real)
(define time-scale 0.01)

(: num->byte (-> Fixnum Byte))
(define (num->byte n)
  (cond [(byte? n) n]
        [else (raise "MAX VALUE EXCEEDED")]))

(: bg-red Byte)
(define bg-red 0)

(: bg-green Byte)
(define bg-green 0)

(: bg-blue Byte)
(define bg-blue 80)

(: background-color (Instance Color%))
(define background-color (make-object color% bg-red bg-green bg-blue))

(: refresh-background-color (-> Void))
(define (refresh-background-color)
  (set! background-color (make-object color% bg-red bg-green bg-blue)))

(define bg-color-picker (new frame% [label "Background Color"] [width 400] [height 200]))

(: bg-red-slider (Instance Slider%))
(define bg-red-slider (new slider% [label "Red"] [min-value 0] [max-value 255] [parent bg-color-picker] [callback (lambda (slider event) (set! bg-red (num->byte(send bg-red-slider get-value)))
                                                                                                                    (refresh-background-color))]))

(: bg-green-slider (Instance Slider%))
(define bg-green-slider (new slider% [label "Green"] [min-value 0] [max-value 255] [parent bg-color-picker] [callback (lambda (slider event) (set! bg-green (num->byte(send bg-green-slider get-value)))
                                                                                                                        (refresh-background-color))]))

(: bg-blue-slider (Instance Slider%))
(define bg-blue-slider (new slider% [label "Blue"] [init-value 80] [min-value 0] [max-value 255] [parent bg-color-picker] [callback (lambda (slider event) (set! bg-blue (num->byte(send bg-blue-slider get-value)))
                                                                                                                      (refresh-background-color))]))

(void (new button% [parent bg-color-picker]
           [label "OK"]
           [callback (lambda (button event)
                       (send bg-color-picker show #f))]))



(: fg-red Byte)
(define fg-red 0)

(: fg-green Byte)
(define fg-green 255)

(: fg-blue Byte)
(define fg-blue 0)

(: cell-color (Instance Color%))
(define cell-color (make-object color% fg-red fg-green fg-blue))

(: refresh-cell-color (-> Void))
(define (refresh-cell-color)
  (set! cell-color (make-object color% fg-red fg-green fg-blue)))

(define fg-color-picker (new frame% [label "Foreground Color"] [width 400] [height 200]))

(: fg-red-slider (Instance Slider%))
(define fg-red-slider (new slider% [label "Red"] [min-value 0] [max-value 255] [parent fg-color-picker] [callback (lambda (slider event) (set! fg-red (num->byte(send fg-red-slider get-value)))
                                                                                                                    (refresh-cell-color))]))

(: fg-green-slider (Instance Slider%))
(define fg-green-slider (new slider% [label "Green"] [min-value 0] [max-value 255] [init-value 255] [parent fg-color-picker] [callback (lambda (slider event) (set! fg-green (num->byte(send fg-green-slider get-value)))
                                                                                                                                         (refresh-cell-color))]))

(: fg-blue-slider (Instance Slider%))
(define fg-blue-slider (new slider% [label "Blue"] [min-value 0] [max-value 255] [parent fg-color-picker] [callback (lambda (slider event) (set! fg-blue (num->byte(send fg-blue-slider get-value)))
                                                                                                                      (refresh-cell-color))]))

(void (new button% [parent fg-color-picker]
           [label "OK"]
           [callback (lambda (button event)
                       (send fg-color-picker show #f))]))



(: cell-scale Nonnegative-Integer)
(define cell-scale 8)
(define canvas-size 800)

(define living-cells null)

(: show-overlay Boolean)
(define show-overlay #t)

(: advance-frame Boolean)
(define advance-frame #f)

(: rewind-frame Boolean)
(define rewind-frame #f)

(: rewind Boolean)
(define rewind #f)

(: genocide Boolean)
(define genocide #f)

(: game-paused Boolean)
(define game-paused #t)

(define error-box (new dialog% [label "Load error!"]))

(define error-msg (new message% [parent error-box] [auto-resize #t] [label ""]))
 
(void (new button% [parent error-box]
           [label "OK"]
           [callback (lambda (button event)
                       (send error-box show #f))]))

(define help-box (new dialog% [label "Help"]))

(define help-messages (map (λ ([text : String]) (new message% [parent help-box] [auto-resize #t] [label text]))
                           '(
                             "Left click to create life. Right click to kill."
                             "Scroll to zoom. Middle-click and drag to pan."
                             "WASD keys may also be used to pan."
                             "Space bar :  Pause/unpause"
                             "Left/right arrows :  Advance or rewind"
                             "Up/down arrows :  Speed (capped at 100%)"
                             "Number keys 1-5 :  Brush sizes"
                             "g :  Export to GIF (Use at your own risk.)"
                             "e :  Export living cells to file"
                             "i :  Import living cells from file"
                             "o :  Disable/enable overlay"
                             "k :  Kill all cells and erase history"
                             "r :  Revert to earliest state"
                             ", :  Export living cells to clipboard"
                             ". :  Import living cells from clipboard"
                             "v :  Import clipboard to mouse position"
                             "c :  Center viewport on origin"
                             "F11 :  Full screen"
                             "Escape :  Exit")))

(void (new button% [parent help-box]
           [label "OK"]
           [callback (lambda (button event)
                       (send help-box show #f))]))

(define about-box (new dialog% [label (string-append "About GAMEOFDEATH version " VERSION)]))

(define about-messages (map (lambda ([text : String]) (new message% [parent about-box] [auto-resize #t] [label text]))
                            (list
                             (string-append "GAMEOFDEATH " VERSION)
                             " is a toy Game of Life simulator written in Typed Racket."
                             "Copyright (C) 2019 Robert Lavery rob@robertlavery.com"
                             "This program is free software: you can redistribute it and/or modify"
                             "it under the terms of the GNU General Public License as published by"
                             "the Free Software Foundation, either version 3 of the License, or"
                             "(at your option) any later version."
                             "This program is distributed in the hope that it will be useful,"
                             "but WITHOUT ANY WARRANTY; without even the implied warranty of"
                             "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
                             "GNU General Public License for more details."
                             "You should have received a copy of the GNU General Public License"
                             "along with this program.  If not, see <https://www.gnu.org/licenses/>.")))

(void (new button% [parent about-box]
           [label "OK"]
           [callback (lambda (button event)
                       (send about-box show #f))]))
                               
(define-type Neighbor-Counts (U 0 1 2 3 4 5 6 7 8))

(: survivable-neighbor-counts (Listof Neighbor-Counts))
(define survivable-neighbor-counts (list 2 3))

(: birth-neighbor-counts (Listof Neighbor-Counts))
(define birth-neighbor-counts (list 3))

(: nnest-in (->* ((-> Integer Integer Boolean) (-> coordinate Integer) (Listof coordinate)) ((U #f Integer)) Integer))
(define (nnest-in comproc getproc coord-list [nsf #f])
  (cond [(and (null? coord-list) nsf) nsf]
        [(not nsf) (nnest-in comproc getproc (rest coord-list) (getproc (first coord-list)))]
        [else (if (comproc (getproc (first coord-list)) nsf)
                  (nnest-in comproc getproc (rest coord-list) (getproc (first coord-list)))
                  (nnest-in comproc getproc (rest coord-list) nsf))]))

(: coord-rows->grid (-> (Listof (Listof Integer)) (Listof coordinate)))
(define (coord-rows->grid coord-rows)
  (tag-rows coord-rows 0))

(: tag-rows (->* ((Listof (Listof Integer)) Integer) ((Listof coordinate)) (Listof coordinate)))
(define (tag-rows rows pos [acc null])
  (cond [(null? rows) acc]
        [else (tag-rows (rest rows) (+ pos 1) (append acc
                                                 ((inst map coordinate Integer) (λ ([c : Integer])
                                                        (coordinate c pos)) (first rows))))]))

(: ncc-c (-> (U Complex False) Nonnegative-Integer))
(define (ncc-c n)
 (cond [(exact-nonnegative-integer? n) n]
       [else (raise "Broken contract-- this function should only be given a [0-9]+!")]))

(: mcc-c (-> (U Complex False) Integer))
(define (mcc-c n)
 (cond [(exact-integer? n) n]
       [else (raise "Broken contract-- this function should only be given a -?[0-9]+!")]))

(: normalize-rle (-> String String))
(define (normalize-rle str)
  (let ([matches (regexp-match* #rx"[0-9]+[bo\\$]|(?<![0-9])[bo\\$]" str)])
    (cond [(null? matches) ""]
          [else ((inst foldr String String) string-append "" (map (lambda ([match : String])
                          (let* ([times (regexp-match* #rx"[0-9]+" match)]
                                 [state (regexp-match* #rx"[ob\\$]" match)])
                            (cond [(null? state) (raise "INCORRECT FORMAT: BAD STATE")]
                                  [(not (null? times)) (string-* (first state) (ncc-c (string->number (first times))))]
                                  [else (first state)]))) matches))])))
  

(: string-* (->* (String Integer) (String) String))
(define (string-* orig-string i [acc-str orig-string])
  (cond [(<= i 0) ""] 
        [(eq? i 1) acc-str]
        [else (string-* orig-string (- i 1) (string-append orig-string acc-str))]))

  
(: string->coord-row (-> String (Listof Integer)))
(define (string->coord-row str)
  (tag-chars (string-split str "") null 0))


(: tag-chars (-> (Listof String) (Listof Integer) Integer (Listof Integer)))
(define (tag-chars char-list coord-list pos)
  (cond [(null? char-list) coord-list]
        [(equal? (first char-list) "o")(tag-chars (rest char-list) (append coord-list (list pos)) (+ pos 1))]
        [(tag-chars (rest char-list) coord-list (+ pos 1))]))

(: rle->quadtree (-> String Quadtree))
(define (rle->quadtree rl)
  (quadtree-fold null(coord-rows->grid (map string->coord-row (string-split (normalize-rle rl) "$")))))


(: denormalize (-> String String))
(define (denormalize str)
  (let ([matches : (Listof String) (regexp-match* #rx"b+|o+|\\$+|!|\n" str)])
    (regexp-replace*
     #px"(.{68}?[^0-9])"
     ((inst foldr String String String String)
      string-append ""
      ((inst map String String)
       (lambda ([char/s : String])
         (cond [(and (regexp-match? #rx"[bo\\$]" char/s)(> (string-length char/s) 1))
                (string-append (number->string (string-length char/s)) (string (string-ref char/s 1)))]
               [else char/s]))
       matches)) "\\1\n")))

(: quadtree->nrle (-> Quadtree String))
(define (quadtree->nrle quadtree)
  (let* ([coord-list (descendant-coordinates quadtree)]
         [min-x (nnest-in < coordinate-x coord-list)]
         [min-y (nnest-in < coordinate-y coord-list)]
         [max-x (nnest-in > coordinate-x coord-list)]
         [max-y (nnest-in > coordinate-y coord-list)])
    ((inst foldr String String String String)
     string-append "!"
     ((inst map String Integer)
      (lambda ([y-coord : Integer])
        ((inst foldr String String String String)
         string-append
         "$"
         ((inst map String Integer)
          (lambda ([x-coord : Integer])
            (if (quadtree-member? quadtree (coordinate x-coord y-coord))
                "o"
                "b"))(range min-x (+ max-x 1 )))))
      (range min-y (+ max-y 1))))))

(: quadtree->rle (-> Quadtree String))
(define (quadtree->rle rlestr)
  (denormalize (for/fold ([accstr : String (quadtree->nrle rlestr)])
            ([reg (list #rx"[0-9b\\$]*!" #rx"[0-9]*b+\\$")]
             [rep (list "!" "$")])
    (regexp-replace* reg accstr rep))))

(: stringificate (->* (Input-Port) (String) String))
(define (stringificate port [accstr ""])
  (let ([next (read-line port 'any)])
    (cond [(eof-object? next) accstr]
          [(string=? next "") (stringificate port accstr)]
          [(stringificate port (string-append accstr "\n" next))])))

(: listificate (-> String (Listof Neighbor-Counts)))
(define (listificate str)
  (remove-duplicates
   ((inst filter-map Neighbor-Counts Number)
    (lambda (nums)
      (if ((make-predicate Neighbor-Counts) nums) nums #f))
    ((inst filter-map Number String)
     (lambda ([n : String])
       (string->number n))(string-split str "")))))

(define rule-picker (new frame% [label "Select Rule"] [width 400] [height 150]))

(define rule-panels (new vertical-panel% [parent rule-picker]))
(define survival-check-boxes (new horizontal-panel% [parent rule-panels]))

(define survival-checkbox-msg (new message% [label "Neighbors to survive:  "] [parent survival-check-boxes]))

(: verify-neighbor-counts (-> Any (Listof Neighbor-Counts)))
(define (verify-neighbor-counts nc)
  (cond [((make-predicate (Listof Neighbor-Counts)) nc) nc]
        [else (raise "NOT A NEIGHBOR COUNT")]))

(: sboxes (Listof (Instance Check-Box%)))
(define sboxes
  (map
   (lambda ([i : Integer])
     (new check-box% [label (number->string i)]
          [value (member i survivable-neighbor-counts)]
          [parent survival-check-boxes]
          [callback
           (lambda (box event)
             (let ([checked? (member i survivable-neighbor-counts)])
               (if checked?
                   (set! survivable-neighbor-counts
                         (filter (lambda (x) (not (eq? x i)))
                                 survivable-neighbor-counts))
                   (set! survivable-neighbor-counts (verify-neighbor-counts (append (list i) survivable-neighbor-counts))))))])) (list 0 1 2 3 4 5 6 7 8)))


(define birth-check-boxes (new horizontal-panel% [parent rule-panels]))

(define birth-checkbox-msg (new message% [label "Neighbors to reproduce:"] [parent birth-check-boxes]))

(: bboxes (Listof (Instance Check-Box%)))
(define bboxes
  (map
   (lambda ([i : Integer])
     (new check-box% [label (number->string i)]
          [value (member i birth-neighbor-counts)]
          [parent birth-check-boxes]
          [style (if (zero? i) '(deleted) null)]
          [callback (lambda (box event)
                      (let ([checked? (member i birth-neighbor-counts)])
                        (if checked?
                            (set! birth-neighbor-counts
                                  (filter (lambda (x) (not (eq? x i))) birth-neighbor-counts))
                            (set! birth-neighbor-counts (verify-neighbor-counts (append (list i) birth-neighbor-counts))))))])) (list 0 1 2 3 4 5 6 7 8)))

(void (new button% [parent rule-picker]
           [label "OK"]
           [callback (lambda (button event)
                       (send rule-picker show #f))]))

(: extract-rle (->* (String) ((U coordinate #f)) Quadtree))
(define (extract-rle str [mouse-coords #f])
  (let* ([width-and-height (regexp-match #rx"x = (-?[0-9]+), y = (-?[0-9]+)" str)]
         [coord-match (regexp-match #rx"# *(?:P|R) *(-?[0-9]+) *(-?[0-9]+)" str)]
         [offset-x (cond [mouse-coords (coordinate-x mouse-coords)]
                         [((make-predicate (Listof String)) coord-match) (mcc-c (string->number ((inst second String) coord-match)))]
                         [else 0])]
         [offset-y (cond [mouse-coords (coordinate-y mouse-coords)]
                         [((make-predicate (Listof String)) coord-match) (mcc-c (string->number ((inst third String) coord-match)))]
                         [else 0])]
         [offset-coordinate (coordinate offset-x offset-y)]
         [rule-match (regexp-match #rx"rule = B?([0-8]+)\\/S?([0-8]+)" str)]
         [rule (if ((make-predicate (Listof String)) rule-match) rule-match #f)]
         [rule-letter-formatted (regexp-match? #rx"rule = B[0-8]+\\/S[0-8]+" str)]
         [first-rule (if rule ((inst second String) rule) "23")]
         [second-rule (if rule ((inst third String) rule) "3")]
         [bnc (if rule-letter-formatted first-rule second-rule)]
         [snc (if rule-letter-formatted second-rule first-rule)]
         [rle (foldr string-append "" (filter (lambda ([line : String])
                                                (regexp-match? #rx"^([0-9]*[ob!\\$])+$" line))
                                              (regexp-split #px"[[:space:]]" str)))])
    (cond [(or
            (string=? rle "")
            (not width-and-height))
           (begin (send error-msg set-label "Unknown filetype or corrupt data!") (send error-box show #t) null)]
          [else
           (begin
               (for ([i : Integer (list 0 1 2 3 4 5 6 7 8)])
                 (begin
                   (send (list-ref sboxes i) set-value (member i (listificate snc)))
                   (send (list-ref bboxes i) set-value (member i (listificate bnc)))))
               (when mouse-coords (set! paste-onto-current #t))
               (set! survivable-neighbor-counts (listificate snc))
               (set! birth-neighbor-counts (listificate bnc))
               (quadtree-translate (rle->quadtree rle) offset-coordinate))])))

(: toggle-fullscreen (-> Void))
(define (toggle-fullscreen)
  (let ([old-midpoint (coordinate
                       (exact-round (/ (send frame get-width) cell-scale 2))
                       (exact-round (/ (send frame get-height) cell-scale 2)))])
    (send frame fullscreen (not (send frame is-fullscreened?)))
    (set! viewport-offset (coordinate
                           (- (coordinate-x viewport-offset)
                              (- (exact-round (/ (send frame get-width) cell-scale 2)) (coordinate-x old-midpoint)))
                           (- (coordinate-y viewport-offset)
                              (- (exact-round (/ (send frame get-height) cell-scale 2)) (coordinate-y old-midpoint)))))))

; Given a coordinate and a Quadtree, determine if it survives.
(: determine-cell-state (-> Quadtree coordinate Boolean))
(define (determine-cell-state population cell)
  (let ([living-neighbors (quadtree-neighbors population cell)]
        [orig-state (quadtree-member? population cell)])
    (cond
      [(and orig-state (not (member living-neighbors survivable-neighbor-counts))) #f]
      [(and orig-state (member living-neighbors survivable-neighbor-counts)) #t]
      [(and (not orig-state) (member living-neighbors birth-neighbor-counts)) #t]
      [else #f]))) 

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

(define master-frame% (class frame% (super-new)
                        (define (on-close)
                          (exit))
                        (augment on-close)))

(define frame (new master-frame% [label "GAMEOFDEATH: A Game of Life Simulator"] [width canvas-size] [height canvas-size]))

(define the-menu-bar (new menu-bar% [parent frame]))

(define file-menu (new menu% [parent the-menu-bar] [label "File"]))
(define edit-menu (new menu% [parent the-menu-bar] [label "Edit"]))
(define view-menu (new menu% [parent the-menu-bar] [label "View"]))
(define help-menu (new menu% [parent the-menu-bar] [label "Help"]))

(define import-button (new menu-item% [parent file-menu] [label "Import from file..."] [callback (lambda (button event) (import-cell-state))]))
(define export-button (new menu-item% [parent file-menu] [label "Export to file..."] [callback (lambda (button event) (export-cell-state current-cell-state))]))
(define gif-button (new menu-item% [parent file-menu] [label "Export as GIF..."] [callback (lambda (button event) (write-gif-to-desired-path history))]))
(define close-button (new menu-item% [parent file-menu] [label "Close"] [callback (lambda (button event) (exit))]))

(define foreground-color-button (new menu-item% [parent edit-menu] [label "Foreground Color"] [callback (lambda (button event) (send fg-color-picker show #t))]))
(define background-color-button (new menu-item% [parent edit-menu] [label "Background Color"] [callback (lambda (button event) (send bg-color-picker show #t))]))
(define rules-button (new menu-item% [parent edit-menu] [label "Change Rule"] [callback (lambda (button event) (send rule-picker show #t))]))
(define import-clipboard-button (new menu-item% [parent edit-menu] [label "Import from clipboard"] [callback (lambda (button event) (import-from-clipboard))]))
(define export-clipboard-button (new menu-item% [parent edit-menu] [label "Export to clipboard"] [callback (lambda (button event) (export-to-clipboard current-cell-state))]))
(define genocide-button (new menu-item% [parent edit-menu] [label "Kill all and clear history"] [callback (lambda (button event) (set! genocide #t))]))
(define revert-button (new menu-item% [parent edit-menu] [label "Revert to earliest state"] [callback (lambda (button event) (set! rewind #t))]))

(define fullscreen-button (new menu-item% [parent view-menu] [label "Fullscreen"] [callback (lambda (button event) (toggle-fullscreen))]))
(define overlay-button (new menu-item% [parent view-menu] [label "Toggle overlay"] [callback (lambda (button event) (set! show-overlay (not show-overlay)))]))

(define controls-button (new menu-item% [parent help-menu] [label "Controls"] [callback (lambda (button event) (send help-box show #t))]))

(define about-button (new menu-item% [parent help-menu] [label "About"] [callback (lambda (button event) (send about-box show #t))]))

(: is-panning (Listof coordinate))
(define is-panning null)

(: current-mouse-pos coordinate)
(define current-mouse-pos (coordinate 0 0))

(: symmetry-mode Boolean)
(define symmetry-mode #f)

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

(: render-frame (-> (Listof coordinate) (Instance Bitmap%)))
(define (render-frame cells)
  (let* ([width (send frame get-width)]
         [height (send frame get-height)]
         [target (make-bitmap (if (positive-integer? width) width 1) (if (positive-integer? height) height 1))]
         [dc : (Instance Bitmap-DC%) (new bitmap-dc% [bitmap target])])
    (send dc set-background background-color)
    (send dc set-pen background-color 1 'transparent)
    (send dc set-brush background-color 'solid)
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

(: paste-onto-current Boolean)
(define paste-onto-current #f)

(: output-cell-list (-> Quadtree (Listof (Pairof Integer Integer))))
(define (output-cell-list quadtree)
  ((inst map (Pairof Integer Integer) coordinate)
   (lambda ([c : coordinate])
     (cons (coordinate-x c) (coordinate-y c)))
   (descendant-coordinates quadtree)))

(: export-rle (-> Quadtree String))
(define (export-rle quadtree)
  (if (null? quadtree) "#C You tried to export an empty screen. x = 0, y = 0\n!"
      (let* ([coord-list (descendant-coordinates quadtree)]
             [smallest-x (nnest-in < coordinate-x coord-list)]
             [smallest-y (nnest-in < coordinate-y coord-list)]
             [x (number->string
                 (-
                  smallest-x
                  (nnest-in > coordinate-x coord-list)))]
             [y (number->string
                 (-
                  smallest-y
                  (nnest-in > coordinate-y coord-list)))]
             [b ((inst foldl Number String) (lambda ([n : Number]
                                                     [a : String])
                                              (string-append a (number->string n))) "" birth-neighbor-counts)]
             [s ((inst foldl Number String) (lambda ([n : Number]
                                                     [a : String])
                                              (string-append a (number->string n))) "" survivable-neighbor-counts)])
        (string-append
         "# Generated by GAMEOFDEATH" "\n"
         "#P " (number->string (- smallest-x 1)) " " (number->string smallest-y) "\n"
         "x = " x ", y = " y ", rule = B" b "/S" s "\n"
         (quadtree->rle quadtree)))))

(: export-to-clipboard (-> Quadtree Void))
(define (export-to-clipboard quadtree)
  (send the-clipboard set-clipboard-string (export-rle quadtree) 0))

(: exporting-to-clipboard Boolean)
(define exporting-to-clipboard #f)

(: import-from-clipboard (->* () ((U coordinate #f)) Void))
(define (import-from-clipboard [coord #f])
  (let* ([clipboard-input (open-input-string (send the-clipboard get-clipboard-string 0))]
         [action (if coord (lambda ([s : String]) (extract-rle s coord)) extract-rle)])
           (set! import-cells (action (stringificate clipboard-input)))))
    
(: export-cell-state (-> Quadtree Void))
(define (export-cell-state quadtree)
  (let ([export-path (put-file "Export cell state"
                               frame
                               (find-system-path 'desk-dir)
                               "cells.rle"
                               "gol"
                               (list 'common)
                               (list (list "Game of Life state" "*.rle")))])
    (cond [(path? export-path)
           (let ([fh (open-output-file export-path #:mode 'text #:exists 'replace)])
             (set! export-threads
                   (list (cons
                          (thread
                           (lambda ()
                             (display
                              (export-rle quadtree) fh))) fh))))]
          [else (void)])))

(: import-cell-state (-> Void))
(define (import-cell-state)
  (let ([import-path (get-file "Import cell state"
                               frame
                               (find-system-path 'desk-dir)
                               #f
                               "gol"
                               (list 'common)
                               (list (list "Game of Life state" "*.rle")))])
    (cond [(path? import-path)
           (let ([file-input (get-cells-from-file import-path)])
             (cond [(string? file-input)
                    (set! import-cells
                          (extract-rle file-input))]
                   [else (begin (send error-msg set-label "Unknown filetype or corrupt data!") (send error-box show #t))]))]
          [else (void)])))

(: get-cells-from-file (-> Path-String Any))
(define (get-cells-from-file path)
  (let* ([fh (open-input-file path)]
         [cells (stringificate fh)])
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
                             (cond [(eq? kc 'f11) (toggle-fullscreen)]
                                   [(eq? kc #\x) (set! symmetry-mode (not symmetry-mode))]
                                   [(eq? kc 'escape) (if (send frame is-fullscreened?)
                                                         (send frame fullscreen #f)
                                                         (exit))]
                                   [(eq? kc #\1) (set! brush-scale 0)]
                                   [(eq? kc #\2) (set! brush-scale 1)]
                                   [(eq? kc #\3) (set! brush-scale 2)]
                                   [(eq? kc #\4) (set! brush-scale 3)]
                                   [(eq? kc #\5) (set! brush-scale 4)]
                                   [(eq? kc #\k) (set! genocide #t)(set! game-paused #t)]
                                   [(eq? kc #\w) (set! viewport-offset (coordinate (coordinate-x viewport-offset)
                                                                                   (- (coordinate-y viewport-offset) (exact-round (* 0.1 (/ (send frame get-height) cell-scale))) )))]
                                   [(eq? kc #\a) (set! viewport-offset (coordinate (- (coordinate-x viewport-offset) (exact-round (* 0.1 (/ (send frame get-width)cell-scale))) ) (coordinate-y viewport-offset)))]
                                   [(eq? kc #\d) (set! viewport-offset (coordinate (+ (coordinate-x viewport-offset) (exact-round (* 0.1 (/ (send frame get-width) cell-scale))) ) (coordinate-y viewport-offset)))]

                                     
                                   [(eq? kc #\s) (set! viewport-offset (coordinate (coordinate-x viewport-offset)
                                                                                   (+ (coordinate-y viewport-offset) (exact-round (* 0.1 (/ (send frame get-height) cell-scale))) )))]
                                   [(eq? kc #\g) (write-gif-to-desired-path history)]
                                   [(or (eq? kc #\h) (eq? kc 'f1)) (send help-box show #t)]
                                   [(eq? kc #\e) (export-cell-state current-cell-state)]
                                   [(eq? kc #\i) (import-cell-state)]
                                   [(eq? kc #\o) (set! show-overlay (not show-overlay))]
                                   [(eq? kc #\,) (set! exporting-to-clipboard #t)]
                                   [(eq? kc #\.) (import-from-clipboard)]
                                   [(eq? kc #\v) (import-from-clipboard (coordinate
                                                                         (exact-round (+ (coordinate-x viewport-offset) (/ (coordinate-x current-mouse-pos) cell-scale)))
                                                                         (exact-round (+ (coordinate-y viewport-offset) (/ (coordinate-y current-mouse-pos) cell-scale)))))]
                                   [(eq? kc #\c) (set! viewport-offset (coordinate
                                                                        (exact-round (/ (send frame get-width) -2 cell-scale))
                                                                        (exact-round (/ (send frame get-height) -2 cell-scale))))]
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
                                   [(eq? kc #\r) (set! rewind #t)]
                                   [(eq? kc #\k) (set! genocide #t)(set! game-paused #t)]
                                   [(eq? kc #\space) (set! game-paused (not game-paused))])))
                         (define/override (on-event event)
                           (cond [(or (send event button-down? 'right) (send event get-right-down))
                                  (set! kill-mouse-cells (make-brush brush-scale (send event get-x) (send event get-y)))]
                                 [(or (send event button-down? 'left) (send event get-left-down))
                                  (set! new-mouse-cells
                                        (if symmetry-mode
                                            (append* (list
                                             (make-brush brush-scale (send event get-x) (send event get-y))
                                             (make-brush brush-scale (* -1 (send event get-x)) (send event get-y))
                                             (make-brush brush-scale (send event get-x) (* -1 (send event get-y)))
                                             (make-brush brush-scale (* -1 (send event get-x)) (* -1 (send event get-y)))))
                                             (make-brush brush-scale (send event get-x) (send event get-y))))]
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
                       (refresh-background-color)
                       (send dc set-pen background-color 1 'transparent)
                       (send dc set-brush background-color 'solid)
                       (send dc draw-rectangle 0 0 (send frame get-width) (send frame get-height)))]))
  
(: draw-cell (-> (Instance DC<%>) Integer Integer Void))
(define (draw-cell dc x y)
  (let ([x-offset : Integer (coordinate-x viewport-offset)]
        [y-offset : Integer (coordinate-y viewport-offset)])
    (send dc set-brush cell-color 'solid)
    (send dc set-pen background-color 1 'solid)
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
         [rule-b ((inst foldl Number String) (lambda ([n : Number]
                                                 [a : String])
                                          (string-append a (number->string n))) "" birth-neighbor-counts)]
         [rule-s ((inst foldl Number String) (lambda ([n : Number]
                                                 [a : String])
                                          (string-append a (number->string n))) "" survivable-neighbor-counts)]
         [dc (send canvas get-dc)]
         [updated-quadtree (quadtree-nuke (quadtree-fold quadtree new-mouse-cells) kill-mouse-cells)]
         [next (cond [(not (null? import-cells)) (let ([i : Quadtree import-cells])
                                                   (set! import-cells null)
                                                   (if paste-onto-current
                                                       (begin
                                                         (set! paste-onto-current #f)
                                                         (quadtree-fold updated-quadtree (descendant-coordinates i)))
                                                       (begin
                                                         (set! history null)
                                                         (set! game-paused #t) i)))]
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
    (send dc set-text-foreground "DimGray")
    (cond [exporting-to-clipboard (export-to-clipboard quadtree) (set! exporting-to-clipboard #f)])
    (cond [show-overlay (begin
                          (send dc set-font (make-object font% 12 'system))
                          (send dc draw-text (string-append "Coordinates:  X "
                                                            (number->string (exact-round (+ (coordinate-x viewport-offset) (/ (coordinate-x current-mouse-pos) cell-scale)))) "  Y "
                                                            (number->string (exact-round (* -1 (+ (coordinate-y viewport-offset) (/ (coordinate-y current-mouse-pos) cell-scale)))))) 25 20)
                          (send dc draw-text (string-append "Time scale: " (real->decimal-string (* (/ 0.01 time-scale) 100)) "%") 25 40)
                          (send dc draw-text (string-append "Population: " (number->string (length (descendant-coordinates next)))) 25 60)
                          (send dc draw-text (string-append "Step: " (number->string (length history))) 25 80)
                          (send dc draw-text (string-append "Zoom: " (real->decimal-string (* (/ cell-scale 8) 100)) "%") 25 100)
                          (send dc draw-text (string-append "Rule: B" rule-b "/S" rule-s) 25 120)
                          (send dc draw-text "rob@robertlavery.com" (- (send frame get-width) 200) (- (send frame get-height) 90)))])
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
