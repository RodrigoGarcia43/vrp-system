(in-package :vrp)

(defparameter *search-strategies-for-classical-criteria*
  `((best +best-improvement+)
    (first +first-improvement+)
    (random +random-improvement+))
  "A list with the search strategies that can be used to automatically create classical neighboorhood criteria.  The first element is a symbol to append to the name of the criterion, and the second is a symbol whose symbol-value should be an instance of a select-strategy class.")

(defmacro make-classical-criterion
    (name description
     &key
       (strategies *search-strategies-for-classical-criteria*))
  ;; (declare (ignore strategies))
  (let* ((criterion-description name))
    ;; let's create the description
    `(progn
       (defparameter ,criterion-description ',description)
       ;; now we add one criterion for each strategy
       ,@(loop for (strat instance) in strategies
               collecting `
               (defparameter ,(symb name "-" strat)
                 (make-neighborhood-criterion
                  ,criterion-description
                  +exhaustive-search-strategy+
                  ,instance)))
       )))

(make-classical-criterion
 rab
 ((select-route r1)
  (select-client c1 from r1)
  (insert-client c1 into r1)))

(make-classical-criterion rabs ((select-route r1)
                               (select-client c1 from r1)
                               (insert-client c1 into r1
                                 :ex (list
                                      (list r1 c1.select.position)
                                      (list r1 (1- c1.select.position)))
                                 )))

(make-classical-criterion rabs*
   ((select-route r1)
    (select-client c1 from r1)
    (insert-client c1 into r1
       :ex (list
            (list r1 c1.select.position)
            (list r1 (1- c1.select.position))))))

(make-classical-criterion rad ((select-route r1)
                                (select-client c1 from r1)
                                (add-route c1)))

(make-classical-criterion rads ((select-route r1)
                                (select-client c1 from r1)
                                (add-route c1)))

(make-classical-criterion rads* ((select-route r1)
                                 (select-client c1 from r1)
                                 (add-route c1)))

(make-classical-criterion rarb
  ((select-route r1)
   (select-client c1 from r1)
   (select-route r2)
   (insert-client c1 into r2)))

(make-classical-criterion rarbs
  ((select-route r1)
   (select-client c1 from r1)
   (select-route r2)
   (insert-client c1 into r2
    :ex (list c1.select.coord
              (list c1.route
                    (1- c1.select.position))))))

(make-classical-criterion rarbs*
  ((select-route r1)
   (select-client c1 from r1)
   (select-route r2 :dt r1)
   ;; we don't need to add any constraint
   ;; to the insertion of the client
   ;; because the only constraints were
   ;; in the same route as in the selection
   ;; but that case is excluded now
   (insert-client c1 into r2)))

(make-classical-criterion rarac ((select-route r1)
                                 (select-client c1 from r1)
                                 (select-route r2)
                                 (select-client c2 from r2)
                                 (swap-clients c1 c2)))

(make-classical-criterion raracs ((select-route r1)
                                 (select-client c1 from r1)
                                 (select-route r2 :ge r1)
                                 (select-client
                                  c2 from r2
                                  :ge (list r1 c1.select.position))
                                 (swap-clients c1 c2)))

(make-classical-criterion raracs*
   ((select-route r1)
    (select-client c1 from r1)
    (select-route r2 :ge r1)
    (select-client c2 from r2
      ;; avoid the insertion in the same coord
      ;; where c1 was selected from
      ;; because this also happens in rab
      ;; and as we need to avoid insertions
      ;; before c1.coord, we can translate it
      ;; into:
      :gt c1.select.coord)
    (swap-clients c1 c2)))

(make-classical-criterion ref ((select-route r1)
                               (select-subroute z1 from r1)
                               (insert-subroute z1 into r1)))

(make-classical-criterion refs
  ((select-route r1)
   (select-subroute z1 from r1)
   (insert-subroute z1 into r1
     ;; insert beyond the selection coord
     :gt z1.select.coord)))

(make-classical-criterion refs*
  ((select-route r1)
   (select-subroute z1 from r1
     ;; avoid collisions with rabs*
     :ldt 1)
   (insert-subroute z1 into r1
     ;; insert beyond the selection coord
     :gt z1.select.coord
     ;; don't insert right after the
     ;; select coord
     ;; because this is equivalent to select
     ;; the client at that position and
     ;; insert it right before the subroute
     :dt (list r1 (1+ z1.position)))))

(make-classical-criterion rerf ((select-route r1)
                                (select-subroute c1 from r1)
                                (select-route r2)
                                (insert-subroute c1 into r2)))

(make-classical-criterion rerfs ((select-route r1)
                                 (select-subroute z1 from r1)
                                 (select-route r2)
                                 (insert-subroute z1 into r2
                                    :ex-cond (and
                                              (= r1 r2)
                                              (<= z1.ins.pos
                                                  z1.position)))))

(make-classical-criterion rerfs* 
  ((select-route r1)
   (select-subroute c1 from r1
      ;; avoid collisions with rarbs*
      :ldt 1)
   (select-route r2
     ;; avoid collisions with rerfs
     :dt r1)
   (insert-subroute c1 into r2
      ;; avoid collisions with itself
      :ex-cond (and
                (= r1 r2)
                (<= c1.ins.pos c1.position)))))

(make-classical-criterion rereg ((select-route r1)
                                 (select-subroute c1 from r1)
                                 (select-route r2)
                                 (select-subroute c2 from r2)
                                 (swap-subroutes c1 c2)))

(make-classical-criterion reregs ((select-route r1)
                                  (select-subroute z1 from r1)
                                  (select-route r2)
                                  (select-subroute z2 from r2
                                     :ge z1.select.coord)
                                  (swap-subroutes z1 z2)))

(make-classical-criterion reregs* ((select-route r1)
                                   (select-subroute z1 from r1)
                                   (select-route r2 :ge r1)
                                   (select-subroute z2 from r2
                                      :lex-cond
                                      (= z1.length z2.length 1)
                                      :ge z1.select.coord)
                                   (swap-subroutes z1 z2)))

(make-classical-criterion rehf ((select-route r1)
                                (select-subroute z1 from r1)
                                (reverse-subroute z1)
                                (insert-subroute z1 into r1)))

(make-classical-criterion rehfs ((select-route r1)
                                 (select-subroute z1 from r1)
                                 (reverse-subroute z1)
                                 (insert-subroute z1 into r1
                                   :ex-cond
                                   (or (= z1.ins.pos
                                          z1.select.position)
                                       (= z1.ins.pos
                                          (1- z1.select.position))))))

(make-classical-criterion rehfs*
    ((select-route r1)
     (select-subroute z1 from r1
       ;; avoid collisions with rabs*
       :ldt 1)
     (reverse-subroute z1)
     (insert-subroute z1 into r1
        ;; avoid collisions with itself
        :ex-cond
        (or (= z1.ins.pos
               z1.select.position)
            (= z1.ins.pos
               (1- z1.select.position))))))

(make-classical-criterion rehrf ((select-route r1)
                                 (select-subroute z1 from r1)
                                 (select-route r2)
                                 (reverse-subroute z1)
                                 (insert-subroute z1 into r2)))

(make-classical-criterion rehrfs ((select-route r1)
                                  (select-subroute z1 from r1)
                                  (select-route r2)
                                  (reverse-subroute z1)
                                  (insert-subroute z1 into r2
                                      :ex-cond
                                      (and
                                       (= r1 r2)
                                       (or
                                        (= z1.ins.pos
                                           z1.select.position)
                                        (= z1.ins.pos
                                           (1- z1.select.position)))))))

(make-classical-criterion rehrfs*
  ((select-route r1)
   (select-subroute z1 from r1
      ;; to avoid collisions with rarb
      :ldt 1)
   (select-route r2
      ;; to avoid collisions with rehf
      :dt r1)
   (reverse-subroute z1)
   (insert-subroute z1 into r2
     ;; 
     ;; we don't need to avoid collisions
     ;; with itself because these collisions
     ;; appear when r1 = r2 and we have
     ;; have already forbidden that.
     )))

(make-classical-criterion rehreg ((select-route r1)
                                  (select-subroute c1 from r1)
                                  (select-route r2)
                                  (select-subroute c2 from r2)
                                  (reverse-subroute c1)
                                  (swap-subroutes c1 c2)))

(make-classical-criterion rehregs
   ((select-route r1)
    (select-subroute c1 from r1)
    (select-route r2)
    (select-subroute c2 from r2
      :ex-cond               
      (or
       (and ;; both routes with length 1 and r1 < r2
        (= c1.length c2.length 1)
        (< r1 r2)
        )
       (and ;; |c1| = |c2| = 1, r1 = r2, and c2 < c1
        (= c1.length c2.length 1)
        (= r1 r2)
        (< c2.position c1.position))
       ;; otherwise we'll apply the constraints
       ;; if we are in the same route
       (and
        (= r1 r2)
        (or ;; each case in isolation
         ;; we'll split the analysis in several cases
         ;; that I'll write in the comments
         ;; |c1| = 1 and |c2| = 1
         (and ;; these are the case where both length are 1
          (= c1.length 1)
          (= c2.length 1)
          (or ;; pivoting effect
           ;; the first two remove the "pivoting effect"
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (+ c1.position 1))
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (- c1.position 2))))

         ;; |c1| = 1 and |c2| > 1 
         (and
          (= c1.length 1)
          (> c2.length 1)
          (or
           (and ;; don't go pass me
            (< c2.position c1.position)
            (>= (1-
                 (+ c2.position c2.length))
                c1.position))
           (or ;; pivoting effect
            ;; the first two remove the "pivoting effect"
            ;; this is the case where c2.pos < c1.pos
            (= c2.position
               (- c1.position c2.length 1))
            ;; this is the case where c2.pos > c1.pos
            (= c2.position
               (+ c1.position 1)))))
         ;; TODO |c1| > 1 and |c2| = 1
         (and
          (> c1.length 1)
          (= c2.length 1)
          (or ;; pivot and symmetry
           ;; the first two remove the "pivoting effect"
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (+ c1.position 1))
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (- c1.position 2))
           ;; and this one removes the
           ;; symmetry effect
           (= c2.position
              (- c1.position 1))))

         ;; |c1| > 1 and |c2| > 1
         (and
          (> c1.length 1)
          (> c2.length 1)
          (or
           (and
            (< c2.position c1.position)
            (>= (1- (+ c2.position c2.length))
                c1.position))
           (or ;; pivoting effect  
            ;; this is the case where c2.pos < c1.pos
            (= c2.position
               (- c1.position c2.length 1))
            ;; this is the case where c2.pos > c1.pos
            (= c2.position
               (+ c1.position 1)))))
         ))))
    (reverse-subroute c1)
    (swap-subroutes c1 c2)))

(make-classical-criterion rehregs*
   ((select-route r1)
    (select-subroute c1 from r1
       ;; if c1.length is 1 then we don't
       ;; reverse anything and all those
       ;; neighboors are explored in rereg
       :ldt 1)
    (select-route r2)
    (select-subroute c2 from r2
       ;; to avoid collisions with rarac
       ;; don't select both subroutes with length 1
       :lex-cond (= c1.length c2.length 1)
       ;; ;; but also we should not select c2
       ;; ;; right where c1 ends, if c1.length is 2
       ;; ;; I'm adding this constraint
       ;; ;; to the ex-cond

      :ex-cond               
      (or
       (and ;; avoid other collisions with rarac
         ;; we should avoid the case
         ;; when c1.length is 2, c2.length is 1
         ;; and we are in the same route, and c2 is
         ;; selected from the same place as c1
         ;; this can also be obtained in rarac
         (= r1 r2)
         (= c1.length 2)
         (= c2.length 1)
         (= c2.position c1.position))

       ;; (and ;; |c1| = |c2| = 1 and r1 < r2
       ;;  ;; we don't need this because we have already
       ;;  ;; forbidden r1 = r2.
       ;;  (= c1.length c2.length 1)
       ;;  (< r1 r2))
       ;; (and ;; |c1| = |c2| = 1, r1 = r2, and c2 < c1
       ;;  ;; we don't need this because we have already
       ;;  ;; forbidden r1 = r2.
       ;;  (= r1 r2)
       ;;  (= c1.length c2.length 1)
       ;;  (< c2.position c1.position))
       ;; otherwise we'll apply the constraints
       ;; if we are in the same route
       (and
        (= r1 r2)
        (or ;; each case in isolation
         ;; we'll split the analysis in several cases
         ;; that I'll write in the comments

         ;; |c1| = 1 and |c2| = 1
         ;; we don't need this because we have already
         ;; forbidden r1 = r2
         ;; to avoid collisions with rarac

         ;; |c1| = 1 and |c2| > 1
         ;; no need to analyze this because
         ;; we have already ruled out this case
         ;; to avoid collisions with rereg


         (and ;; |c1| > 1 and |c2| = 1
          (> c1.length 1)
          (= c2.length 1)
          (or ;; pivot and symmetry
           ;; the first two remove the "pivoting effect"
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (+ c1.position 1))
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (- c1.position 2))
           ;; and this one removes the
           ;; symmetry effect
           (= c2.position
              (- c1.position 1))))

         ;; |c1| > 1 and |c2| > 1
         (and
          (> c1.length 1)
          (> c2.length 1)
          (or
           (and
            (< c2.position c1.position)
            (>= (1- (+ c2.position c2.length))
                c1.position))
           (or ;; pivoting effect  
            ;; this is the case where c2.pos < c1.pos
            (= c2.position
               (- c1.position c2.length 1))
            ;; this is the case where c2.pos > c1.pos
            (= c2.position
               (+ c1.position 1)))))))))
    (reverse-subroute c1)
    (swap-subroutes c1 c2)))

(make-classical-criterion rerehg ((select-route r1)
                                  (select-subroute c1 from r1)
                                  (select-route r2)
                                  (select-subroute c2 from r2)
                                  (reverse-subroute c2)
                                  (swap-subroutes c1 c2)))

(make-classical-criterion rerehgs
    ((select-route r1)
     (select-subroute c1 from r1)
     (select-route r2)
     (select-subroute c2 from r2
        :ex-cond               
        (or
         ;; if both subroutes have length 1
         ;; exclude if r2 < r1
         (and
          (= c1.length c2.length 1)
          (< r2 r1))
         ;; let's apply the following rules
         ;; if both subroutes are selected
         ;; from the same route
         (and ;; both subroutes are from the same subroute
          (= r1 r2)
          (or ;; let's analyze all the cases
           (and ;; |c1| = |c2| = 1
            (= c1.length 1)
            (= c2.length 1)
            (or
             ;; c2 is "before" c1
             (< c2.position c1.position)
             (or ;; exclued the pivoting effect
              ;; c1.pos < c2.pos
              (= c2.position
                 (+ c1.position 1)))))
           (and ;; |c1| = 1, |c2| > 1
            (= c1.length 1)
            (> c2.length 1)
            ;; let's remove the pivoting effect
            (or ;; pivoting and symmetric effect
             ;; c1.pos < c2.pos
             (= c2.position
                (+ c1.position 1))
             ;; c1.pos > c2.pos
             (= c2.position
                (- c1.position c2.length 1))
             ;; let's remove the symmetric repetitions
             (= c2.position
                (- c1.position c2.length)))




            )
           (and ;; |c1| > 1, |c2| = 1
            ;; there are not repetitions in this case :-o
            ;; I tested up to 10 clients
            ;; but when we mix it with the other
            ;; cases, some repeated elements appear :-/
            ;; so we should remove the pivoting effect
            (and
             (> c1.length 1)
             (= c2.length 1)
             (or ;; let's remove the pivoting effect
              ;; this is the case where c2.pos > c1.pos
              (= c2.position
                 (+ c1.position 1))         
              ;; this is the case where c2.pos < c1.pos
              (= c2.position
                 (- c1.position c2.length 1))
              ;; and this one is for the symmetric effect
              (= c2.position
                 (- c1.position 1)))))
           (and ;; |c1| > 1, |c2| > 1
            (> c1.length 1)
            (> c2.length 1)
            (or ;; let's analyze the possible problems

             (and ;; don't stop before me
              ;;  ;; It worked!!!
              ;;  ;; Now Iosvanny must find out why :-o
              (< c2.position c1.position)

              (= (+ c2.position c2.length)
                 c1.position))

             (or ;; let's remove the pivoting effect
              ;; this is the case where c2.pos > c1.pos
              (= c2.position
                 (+ c1.position 1))         
              ;; this is the case where c2.pos < c1.pos
              (= c2.position
                 (- c1.position c2.length 1)))
             ))))))
     (reverse-subroute c2)
     (swap-subroutes c1 c2)))

(make-classical-criterion rerehgs*
    ((select-route r1)
     (select-subroute c1 from r1)
     ;; (select-route r2)
     (select-subroute c2 from r1
       ;; to avoid collisions with rarac                         
       ;; avoid |c1| = |c2| = 1
       :lex-cond (= c1.length c2.length 1)
       ;; we also need to avoid c2.pos = c1.pos
       ;; because we can get the same neighbor
       ;; with rarac. This is the same restriction
       ;; as in rehreg. I'll also add it to the
       ;; ex-cond.

       ;; the following line avoids collisions
       ;; with rereg
       :ldt 1

       ;; an from here on is to avoid
       ;; collisions with itself
       :ex-cond               
       (or
        (and ;; avoid other collisions with rarac
         ;; we should avoid the case
         ;; when |c1| = 1, |c2| = 2, r1 = r2,
         ;; and c2 is selected from the same place
         ;; as c1 this can also be obtained in rarac.
         (= c2.length 2)
         (= c1.length 1)
         (= c2.position c1.position))
        ;; |c1| = |c2| = 1 ;; ruled out by rarac
        (and ;; |c1| = 1, |c2| > 1
         (= c1.length 1)
         (> c2.length 1)

         (or ;; all the constraints here
          (or ;; pivoting effect and symmetry
           ;; c1.pos < c2.pos
           (= c2.position
              (+ c1.position 1))
           ;; c1.pos > c2.pos
           (= c2.position
              (- c1.position c2.length 1))
           ;; let's remove the symmetric repetitions
           (= c2.position
              (- c1.position c2.length))
           ;; don't go beyond me!!! to avoid
           ;; collisions with rereg
           (and ;; |c1| = 1, |c2| > 1
            (< c2.position c1.position)
            (= (+ c2.position c2.length -1)
               c1.position))
           )
          ;; avoid |c2| >= 3 and concatenation
          (and ;; avoid |c2| >= 3 and concatenation
           (>= c2.length 3)
           (= c1.position c2.position))
          (or ;; to avoid collisions with rehreg pivots with length
           (or ;; c2.pos > c1.pos
            (> c2.position (+ c1.position 1))
            ;; c2.pos < c1.pos
            (> c1.position (+ c2.position c2.length))))
          ) ;; here ends |c1| = 1, |c2| > 1

         )
        ;; |c1| > 1, |c2| = 1 ;; no need [it is rereg]
        (and ;; |c1| > 1, |c2| > 1
         (> c1.length 1)
         (> c2.length 1)
         (or ;; let's analyze the possible problems
          (and ;; [don't stop before me]
           ;;  ;; It worked!!!
           (< c2.position c1.position)
           (= (+ c2.position c2.length)
              c1.position))

          ;; |c1| > 1, |c2| > 1
          (and ;; don't go beyond me!!! to avoid rereg
           ;; collisions with rereg
           ;; |c1| > 1, |c2| > 1
           (< c2.position c1.position)
           (= (+ c2.position c2.length -1)
              c1.position))
          (and ;; to avoid collisions with rereg
           ;; |c2| = 2 and c2.pos = c1.pos + c1.length - 1
           ;; this is equivalent to a rereg with pivot
           ;; 
           (= c2.length 2)
           (= (+ c2.position c2.position)))
          (or ;; let's remove the pivoting effect
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (+ c1.position 1))         
           ;; this is the case where c2.pos < c1.pos
           (= c2.position
              (- c1.position c2.length 1)))
          ;; avoid |c2| >= 3 and concatenation
          (and ;; avoid |c2| >= 3 and concatenation
           (>= c2.length 3)
           (= c1.position c2.position))
          (or ;; to avoid collisions with rehreg pivots with length
           (or ;; c2.pos > c1.pos
            (> c2.position (+ c1.position 1))
            ;; c2.pos < c1.pos
            (> c1.position (+ c2.position c2.length)))))
         )
        ))
     (reverse-subroute c2)
     (swap-subroutes c1 c2)))

(make-classical-criterion rehrehg ((select-route r1)
                                   (select-subroute c1 from r1)
                                   (select-route r2)
                                   (select-subroute c2 from r2)
                                   (reverse-subroute c1)
                                   (reverse-subroute c2)
                                   (swap-subroutes c1 c2)))

(make-classical-criterion rehrehgs
   ((select-route r1)
    (select-subroute c1 from r1)
    (reverse-subroute c1)
    (select-route r2 :ge r1)
    (select-subroute c2 from r2
       :ex-cond               
       (or
        ;; both subroutes are from the same route
        ;; because we exclude the situation
        ;; r2 < r1 in the selection of r2.
        (and
         (= r1 r2)
         (or ;; the general constraints
          (and ;; |c1| = |c2| = 1
           (= c1.length 1)
           (= c2.length 1)
           (or ;; the constraints here
            ;; c2 should be selected after c1
            (< c2.position c1.position)
            (or ;; pivoting effect
             ;; this is the case where c2.pos > c1.pos
             ;; (the case c2.pos < c1.pos is excluded
             ;; in the previous "or")
             (= c2.position
                (+ c1.position 1)))))
          (and ;; |c1| = 1 and |c2| > 1
           (= c1.length 1)
           (> c2.length 1)
           (or ;; pivoting and symmetric effects
            ;; c1.pos < c2.pos
            (= c2.position
               (+ c1.position 1))
            ;; c1.pos > c2.pos
            (= c2.position
               (- c1.position c2.length 1))
            ;; let's remove the symmetric repetitions
            (= c2.position
               (- c1.position c2.length))
            ))
          (and ;; |c1| > 1 and |c2| = 1
           (> c1.length 1)
           (= c2.length 1)
           (or ;; the constraints here
            (or ;; pivot and symmetry
             ;; the first two remove the "pivoting effect"
             ;; this is the case where c2.pos > c1.pos
             (= c2.position
                (+ c1.position 1))
             ;; this is the case where c2.pos > c1.pos
             (= c2.position
                (- c1.position 2))
             ;; ;; and this one removes the
             ;; ;; symmetry effect
             (= c2.position
                (- c1.position 1)))
            (or ;; don't append to end of a subroute
             (= c2.position c1.position))
            (or ;; the anti pivot??? :-/
             ;; c2 > c1
             (/= c2.position
                 (+ c1.position 1))
             ;; c2 < c1
             (/= c2.position
                 (- c1.position 2)))))
          (and ;; |c1| > 1 and |c2| > 1
           (> c1.length 1)
           (> c2.length 1)
           (or ;; the constraints here
            (or ;; pivoting effect
             ;; this is the case where c2.pos < c1.pos
             (= c2.position
                (- c1.position c2.length 1))
             ;; this is the case where c2.pos > c1.pos
             (= c2.position
                (+ c1.position 1)))
            (or ;; "don't touch me! (if we are both subroutes)"
             (and ;; c1.pos <= c2.pos
              (>= c2.position c1.position)
              (= c2.position c1.position))
             (and ;; c1.pos >= c2.pos
              (<= c2.position c1.position)
              (= (+ c2.position c2.length)
                 c1.position)))
            (or ;; the anti pivot??? :-/
             ;; let's remove the second one when c2 > c1
             (> c2.position
                (+ c1.position 1)))
            ))))))
    (reverse-subroute c2)
    (swap-subroutes c1 c2)))

(make-classical-criterion rehrehgs*
   ((select-route r1)
    (select-subroute c1 from r1)
    (reverse-subroute c1)
    (select-route r2 :ge r1)
    (select-subroute c2 from r2
       ;; to avoid collisions with rarac
       :lex-cond (or
                  (= c1.length 1)
                  (= c2.length 1))
       :ex-cond               
       (or
        (or     ;; the general constraints
         ;; we have ruled out the following cases:
         ;; |c1| = |c2| = 1 because it is rarac
         ;; |c1| = 1 and |c2| > 1 because it is rerehg
         ;; |c1| > 1 and |c2| = 1 because it is rehreg
         (and ;; |c1| > 1 and |c2| > 1
          (> c1.length 1)
          (> c2.length 1)
          (or    ;; the constraints here
           (or   ;; pivoting effect
            ;; this is the case where c2.pos < c1.pos
            (= c2.position
               (- c1.position c2.length 1))
            ;; this is the case where c2.pos > c1.pos
            (= c2.position
               (+ c1.position 1)))
           (or     ;; "don't touch me! (if we are both subroutes)"
            (and   ;; c1.pos <= c2.pos
             (>= c2.position c1.position)
             (= c2.position c1.position))
            (and ;; c1.pos >= c2.pos
             (<= c2.position c1.position)
             (= (+ c2.position c2.length)
                c1.position)))
           (or ;; the anti pivot??? :-/
            ;; let's remove the second one when c2 > c1
            (> c2.position
               (+ c1.position 1)))
           )))))
    (reverse-subroute c2)
    (swap-subroutes c1 c2)))

(make-classical-criterion rereehf
   ((select-route route1)
    (select-subroute subroute1 from route1)
    (select-route route2)
    (select-subroute subroute2 from route2)
    (select-subroute subroute3 from route2)
    (swap-subroutes subroute3 subroute2)
    (insert-subroute subroute1 into route1)
    ))
