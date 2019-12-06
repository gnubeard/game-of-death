;
;    qtree.rkt : A Typed, Functional Quadtree Implementation
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



; For reference: https://www.researchgate.net/profile/Raphael_Finkel/publication/220197855_Quad_Trees_A_Data_Structure_for_Retrieval_on_Composite_Keys/links/0c9605273bba2ece7b000000/Quad-Trees-A-Data-Structure-for-Retrieval-on-Composite-Keys.pdf

#lang typed/racket

(provide node                          ; Type: A struct, recursive with Quadtree, representing a non-null record with four children.
         node?                         ; Function: Node predicate.
         Quadtree                      ; Type: Union type, either a null record or a node.
         get-node-coords               ; Function: A null-gated function to provide the coordinates associated with a node.
         get-node-child                ; Function: A null-gated function to provide a child under a node in a particular Direction.
         Direction                     ; Type: A union type -- either a function that returns a child node, or the symbol 'self.
         coordinate                    ; Type: A struct of X Y cartesian coordinates.
         coordinate?                   ; Function: Coordinate predicate.
         coordinate-x                  ; Function: Struct accessor for coordinates.
         coordinate-y                  ; Function: Struct accessor for coordinates.
         coordinate-compare            ; Function: Return a Direction when given two points. 
         quadtree-member?              ; Function: A predicate that returns #t iff given Quadtree contains given coordinate.
         in-range?                     ; Function: A predicate that given x y z returns #t iff x <= z <= y. 
         neighbor?                     ; Function: A predicate that returns #t iff two coordinates are neighbors.
         neighbor-coords               ; Function: Returns the coordinates immediately neighboring a cell.
         overlapping-quadrants         ; Function: Returns a list of Directions overlapping neighbor points to a given coordinate.
         quadtree-neighbors            ; Function: Returns a list of coordinates from the Quadtree neighboring a given coordinate.
         descendant-coordinates        ; Function: Return coordinates of a node and all its descendants.
         quadtree-fold                 ; Function: Given a Quadtree and a list of coordinates, recursively insert coordinates and return resultant quadtree.
         quadtree-nuke                 ; Function: quadtree-fold, but for removal.
         quadtree-insert               ; Function: Given a Quadtree and a coordinate, return a Quadtree with the new coordinate inserted.
         quadtree-delete               ; Function: Given a Quadtree and a coordinate, return a Quadtree with the coordinate deleted. Nonrecursive wrapper for cps.
         quadtree-translate            ; Function: Given a Quadtree and a coordinate, return a Quadtree with all coordinates offset by the given coordinate.
         cape                          ; Type: A struct containing a Quadtree and a list of orphaned coordinates.
         cps                           ; Function: Recursively delete a coordinate from a Quadtree, returning a cape containing the new Quadtree and a list of any orphaned coordinates.
         print-all-coords              ; Convenience function to print all the coordinates from a list of coordinates.
         lexicographical-coordinate-sort) ; Uh, this seems like a good idea at the time.

(define-type Quadtree (U Null node))

(struct node ([ NE : Quadtree ]
              [ NW : Quadtree ]
              [ SW : Quadtree ]
              [ SE : Quadtree ]
              [ coords : coordinate]) #:transparent) ; YAY TRANSPARENT

(struct coordinate ([x : Integer] [y : Integer]) #:transparent)

  

; Capes are constructs that let me hand up orphans as needed from the recursive part of the deletion mechanism.
(struct cape ([quadtree : Quadtree]
              [orphans : (Listof coordinate)]))

; A null-checking wrapper for node-coords. 
(define-syntax-rule (get-node-coords quadtree)
  (cond [(null? quadtree) (raise "get-node-coords cannot be called on a null Quadtree.")]
        [else (node-coords quadtree)]))

; A null-checking wrapper for node-<Direction>. 
(: get-node-child (-> Quadtree (-> node Quadtree) Quadtree))
(define (get-node-child quadtree direction)
  (cond [(null? quadtree) (raise "get-node-child cannot be called on a null Quadtree.")]
        [else (direction quadtree)]))

(define-type Direction (U (-> node Quadtree) 'self))

(: lexicographical-coordinate-sort (-> (Listof coordinate) (Listof coordinate)))
(define (lexicographical-coordinate-sort coord-list)
  (sort coord-list
        (lambda ([c1 : coordinate] [c2 : coordinate])
          (let ([c1-x (coordinate-x c1)] [c1-y (coordinate-y c1)] [c2-x (coordinate-x c2)] [c2-y (coordinate-y c2)])
            (cond [(< c1-x c2-x) #t]
                  [(and (= c1-x c2-x) (< c1-y c2-y)) #t]
                  [else #f])))))

(: in-range? (-> Integer Integer Integer Boolean))
(define (in-range? x y z)(and (<= x z) (>= y z)))

; The comparison function discerns which quadrant below a node in which to categorize a set of coordinates.
; The Finkel and Bentley paper uses a convention by which, for those points lying on cardinal directions,
; Quadrants I and III are "open", while II and IV are "closed." It seems to me that this will inevitably
; result in an unbalanced tree, so my implementation gives each quadrant a cardinal direction.

(define-syntax-rule (coordinate-compare a b)
  (let ([a-x : Integer (coordinate-x a)]
        [a-y : Integer (coordinate-y a)]
        [b-x : Integer (coordinate-x b)]
        [b-y : Integer (coordinate-y b)])
    (cond [(and (<= a-x b-x) (<  a-y b-y)) node-NE]
          [(and (>  a-x b-x) (<= a-y b-y)) node-NW]
          [(and (>= a-x b-x) (>  a-y b-y)) node-SW]
          [(and (<  a-x b-x) (>= a-y b-y)) node-SE]
          [(and (= a-x b-x) (= a-y b-y)) 'self]
          ; If you somehow raise this error, I did not account for something.
          ; I am confident the other options are exhaustive.
          [else (raise "coordinate-compare should never return this!")])))

    
(: quadtree-member? (-> Quadtree coordinate Boolean))
(define (quadtree-member? quadtree coords)
  (cond [(null? quadtree) #f]
        [else (let ([quadrant (coordinate-compare (node-coords quadtree) coords)])
                (cond [(equal? quadrant 'self) #t]
                      [else (quadtree-member? (get-node-child quadtree quadrant) coords)]))]))

(define-syntax-rule (neighbor? c1 c2)
  (let ([x1 : Integer (coordinate-x c1)]
        [y1 : Integer (coordinate-y c1)]
        [x2 : Integer (coordinate-x c2)]
        [y2 : Integer (coordinate-y c2)])
    (and
     (and (in-range? (- x1 1) (+ x1 1) x2)
          (in-range? (- y1 1) (+ y1 1) y2))
     (nand (equal? x1 x2) (equal? y1 y2)))))

(: overlapping-quadrants (-> coordinate coordinate (Listof (-> node Quadtree))))
(define (overlapping-quadrants c1 c2)
  (let* ([x1 : Integer (coordinate-x c1)]
         [y1 : Integer (coordinate-y c1)]
         [x2 : Integer (coordinate-x c2)]
         [y2 : Integer (coordinate-y c2)]
         [NE (if (and (>= (+ x2 1) x1) (>  (+ y2 1) y1)) node-NE null)]
         [NW (if (and (<  (- x2 1) x1) (>= (+ y2 1) y1)) node-NW null)]
         [SW (if (and (<= (- x2 1) x1) (<  (- y2 1) y1)) node-SW null)]
         [SE (if (and (>  (+ x2 1) x1) (<= (- y2 1) y1)) node-SE null)])
    ((inst filter (U Null (-> node Quadtree)) (-> node Quadtree)) procedure? (list NE NW SW SE))))
  
; For now I'll just address the case I'll be using in Game of Life, and look for direct neighbors.
(: quadtree-neighbors (-> Quadtree coordinate Integer))
(define (quadtree-neighbors quadtree coords)
  (cond [(null? quadtree) 0]
        [else
         (let ([quadrants (overlapping-quadrants (node-coords quadtree) coords)]
               [am-neighbor : Integer (if (neighbor? (node-coords quadtree) coords) 1 0)])
           (+ (foldl + 0 (map
                          (lambda ([direction : (-> node Quadtree)])
                            (quadtree-neighbors (direction quadtree) coords)) quadrants)) am-neighbor))]))

(: descendant-coordinates (-> Quadtree (Listof coordinate)))
(define (descendant-coordinates quadtree)
  (cond [(null? quadtree) null]
        [else (let ([coords (list (get-node-coords quadtree))])
                (append coords
                        (append-map
                         (lambda ([direction : (-> node Quadtree)])
                           (descendant-coordinates (direction quadtree)))
                         (list node-NE node-NW node-SW node-SE))))]))

; Deletion
; Given a null Quadtree, return it unchanged (it's already gone.)
; At the top of the recursion stack, take any remaining orphans and insert them into the new Quadtree.
; Quadtree-delete is a wrapper function whose job is to keep the orphans an internal matter.
; It calls the recursive cps function, which performs the deletion and hands up a 'cape' struct
; containing a list of orphaned nodes that need to be reinserted to the quadtree.
(: quadtree-delete (-> Quadtree coordinate Quadtree))
(define (quadtree-delete quadtree coords)
  (let* ([crime-scene-and-orphans (cps quadtree coords)]
         [crime-scene (cape-quadtree crime-scene-and-orphans)]
         [orphans (cape-orphans crime-scene-and-orphans)])
    (quadtree-fold crime-scene orphans)))
 

(: cps (-> Quadtree coordinate cape))
(define (cps quadtree coords)
  (cond [(null? quadtree) (cape null null)]
        [(equal? 'self (coordinate-compare (get-node-coords quadtree) coords))
         (cape null (rest (descendant-coordinates quadtree)))]
        [else
         (let* ([quadrant (coordinate-compare (get-node-coords quadtree) coords)]
                [r (lambda ([direction : (-> node Quadtree)]) (if (equal? quadrant direction)
                                                                  (cps (get-node-child quadtree direction) coords)
                                                                  (cape (get-node-child quadtree direction) null)))]
                [NE (r node-NE)] [NW (r node-NW)] [SW (r node-SW)] [SE (r node-SE)]
                [gather-orphans (lambda ([cape-list : (Listof cape)]) (append-map cape-orphans cape-list))]
                [orphans (gather-orphans (list NE NW SW SE))])
           (cape (node
                  (cape-quadtree NE)
                  (cape-quadtree NW)
                  (cape-quadtree SW)
                  (cape-quadtree SE)
                  (get-node-coords quadtree)) orphans))]))

(: quadtree-nuke (-> Quadtree (Listof coordinate) Quadtree))
(define (quadtree-nuke quadtree coord-list)
  (cond [(null? coord-list) quadtree]
        [else (quadtree-nuke (quadtree-delete quadtree (first coord-list)) (rest coord-list))]))

(: quadtree-fold (-> Quadtree (Listof coordinate) Quadtree))
(define (quadtree-fold quadtree coord-list)
  (cond [(null? coord-list) quadtree]
        [else (quadtree-fold (quadtree-insert quadtree (first coord-list)) (rest coord-list))]))
    
(: quadtree-fold-optimized (-> Quadtree (Listof coordinate) Quadtree))
(define (quadtree-fold-optimized quadtree coord-list)
  (quadtree-fold quadtree (lexicographical-coordinate-sort coord-list)))

(: quadtree-insert (-> Quadtree coordinate node))
(define (quadtree-insert quadtree coords)
  (let ([quadrant (if (null? quadtree) null(coordinate-compare (get-node-coords quadtree) coords))])
    (cond [(null? quadtree) (node null null null null coords)]
          [(procedure? quadrant)
           (let ([r (lambda ([direction : (-> node Quadtree)])
                      (if (equal? quadrant direction)
                          (quadtree-insert (get-node-child quadtree direction) coords)
                          (get-node-child quadtree direction)))])
             (node
              (r node-NE)
              (r node-NW)
              (r node-SW)
              (r node-SE)
              (get-node-coords quadtree)))]
          [else quadtree])))

(: quadtree-translate (-> Quadtree coordinate Quadtree))
(define (quadtree-translate quadtree coord)
  (let ([x : Integer (coordinate-x coord)]
        [y : Integer (coordinate-y coord)])
    (quadtree-fold null (map (lambda ([c : coordinate]) (coordinate (+ (coordinate-x c) x) (+ (coordinate-y c) y))) (descendant-coordinates quadtree)))))

; I'd like to find a cleaner way to do this.
(: neighbor-coords (-> coordinate (Listof coordinate)))
(define (neighbor-coords coordinate)
  (let ([x : Integer (coordinate-x coordinate)]
        [y : Integer (coordinate-y coordinate)])
    (append-map (curry coordmap x) (range (- y 1) (+ y 2)))))

; give all the ys for an x
(: coordmap (-> Integer Integer (Listof coordinate)))
(define (coordmap x y)
  (map (lambda ([z : Integer]) (coordinate z y))
       (range (- x 1) (+ x 2))))

(: print-all-coords (-> (Listof coordinate) Void))
(define (print-all-coords coord-list)
  ((inst for-each coordinate)
   (lambda (coord) (fprintf (current-output-port)
                            "X: ~s Y: ~s\n"
                            (coordinate-x coord)
                            (coordinate-y coord))) coord-list))

(: test-insert-performance (-> Integer Void))
(define (test-insert-performance n)
  (time
   (void
    (quadtree-fold null
                   ((inst append-map coordinate Integer)
                    (lambda ([y : Integer])
                      (map (lambda ([x : Integer])
                             (coordinate x y)) (range (* -1 n) n))) (range (* -1 n) n))))))
