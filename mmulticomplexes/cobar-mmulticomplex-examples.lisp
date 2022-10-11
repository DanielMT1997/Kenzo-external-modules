;;Example 1


(cat-init)
(setf A (cyclicGroup 2))
(setf B (cyclicGroup 2))
(setf KA1 (k-g-1 A))
(setf KB1 (k-g-1 B))                    

(setf cocycle #'(lambda (z1 z2)
                  (if (and (= z1 1) (= z2 1))
                      1 0)))

(setf E (gr-cntr-extn a b cocycle))
(change-class E 'AB-group)

(setf KE1 (k-g-1 E))
(setf efhm2 (central-extension-efhm A B cocycle))
(setf (slot-value kE1 'efhm) efhm2)
(setf ka2 (k-g a 2))
(setf tau1 (univ-fbrt-tw A 2))
(setf tw
  (the fibration
    (build-smmr :sorc ka2 :trgt ke1 :degr -1
                :sintr 
                (sintr (cmps (cocycle-fibr-iso2 b a cocycle)
                             (cmps  (twop-incl (cocycle-fibration B A cocycle)) tau1)))
                :orgn `(fibration33 ,tau1))))
                
(setf comp (eilenberg-moore-bicomplex tw))
(setf mmcm-comp (embi-to-multicomplex comp))
(setf ss2 (mmcm-cobar-spectral-system tw))

(PRINT-SPECTRAL-SYSTEM-GROUP ss2 (list '(0 0)) (list '(0 0)) (list '(1 0)) (list '(1 0)) 4)
(setf r 2)
(dotimes (n 5)
  (dotimes (p (1+ n))
    (PRINT-SPECTRAL-SYSTEM-GROUP ss2 (list (list (- p r) 0))  (list (list (- p 1) 0))  (list  (list p 0))  (list (list (- (+ p r) 1) 0)) n)))


#|      			
       
CAT 71 > (setf A (cyclicGroup 2))
[K1 Abelian-Group]

CAT 72 > (setf B (cyclicGroup 2))
[K1 Abelian-Group]

CAT 73 > (setf KA1 (k-g-1 A))
[K2 Abelian-Simplicial-Group]

CAT 74 > (setf KB1 (k-g-1 B))                    
[K2 Abelian-Simplicial-Group]

CAT 75 > 
(setf cocycle #'(lambda (z1 z2)
                  (if (and (= z1 1) (= z2 1))
                      1 0)))
#<anonymous interpreted function 4060005E7C>

CAT 76 > 
(setf E (gr-cntr-extn a b cocycle))
[K14 Group]

CAT 77 > (change-class E 'AB-group)
[K14 Abelian-Group]

CAT 78 > 
(setf KE1 (k-g-1 E))
[K15 Abelian-Simplicial-Group]

CAT 79 > (setf efhm2 (central-extension-efhm A B cocycle))
[K153 Homotopy-Equivalence K15 <= K139 => K135]

CAT 80 > (setf (slot-value kE1 'efhm) efhm2)
[K153 Homotopy-Equivalence K15 <= K139 => K135]

CAT 81 > (setf ka2 (k-g a 2))
[K154 Abelian-Simplicial-Group]

CAT 82 > (setf tau1 (univ-fbrt-tw A 2))
[K166 Fibration K154 -> K2]

CAT 83 > (setf tw
  (the fibration
    (build-smmr :sorc ka2 :trgt ke1 :degr -1
                :sintr 
                (sintr (cmps (cocycle-fibr-iso2 b a cocycle)
                             (cmps  (twop-incl (cocycle-fibration B A cocycle)) tau1)))
                :orgn `(fibration33 ,tau1))))
[K170 Fibration K154 -> K15]

CAT 84 >                 
(setf comp (eilenberg-moore-bicomplex tw))
[K216 Chain-Complex]

CAT 85 > (setf mmcm-comp (embi-to-multicomplex comp))
[K218 Chain-Complex]

CAT 86 > (setf ss2 (mmcm-cobar-spectral-system tw))
[K476 Spectral-System]

CAT 87 > ss2
[K476 Spectral-System]

CAT 88 > (gfltrcm ss2)
[K474 Generalized-Filtered-Chain-Complex]

CAT 89 > (PRINT-SPECTRAL-SYSTEM-GROUP ss2 (list '(0 0)) (list '(0 0)) (list '(1 0)) (list '(1 0)) 4)
Generalized spectral sequence S[((0 0)),((0 0)),((1 0)),((1 0))]_{4}

Component Z/2Z

Component Z/2Z
NIL

CAT 90 > (setf r 2)
2

CAT 91 > (dotimes (n 5)
  (dotimes (p (1+ n))
    (PRINT-SPECTRAL-SYSTEM-GROUP ss2 (list (list (- p r) 0))  (list (list (- p 1) 0))  (list  (list p 0))  (list (list (- (+ p r) 1) 0)) n)))
Generalized spectral sequence S[((-2 0)),((-1 0)),((0 0)),((1 0))]_{0}

Component Z
Generalized spectral sequence S[((-2 0)),((-1 0)),((0 0)),((1 0))]_{1}

Component Z/2Z
Generalized spectral sequence S[((-1 0)),((0 0)),((1 0)),((2 0))]_{1}

Component Z/2Z
Generalized spectral sequence S[((-2 0)),((-1 0)),((0 0)),((1 0))]_{2}
Generalized spectral sequence S[((-1 0)),((0 0)),((1 0)),((2 0))]_{2}

Component Z/2Z
Generalized spectral sequence S[((0 0)),((1 0)),((2 0)),((3 0))]_{2}
Generalized spectral sequence S[((-2 0)),((-1 0)),((0 0)),((1 0))]_{3}

Component Z/2Z
Generalized spectral sequence S[((-1 0)),((0 0)),((1 0)),((2 0))]_{3}
Generalized spectral sequence S[((0 0)),((1 0)),((2 0)),((3 0))]_{3}

Component Z/2Z
Generalized spectral sequence S[((1 0)),((2 0)),((3 0)),((4 0))]_{3}

Component Z/2Z
Generalized spectral sequence S[((-2 0)),((-1 0)),((0 0)),((1 0))]_{4}
Generalized spectral sequence S[((-1 0)),((0 0)),((1 0)),((2 0))]_{4}

Component Z/2Z
Generalized spectral sequence S[((0 0)),((1 0)),((2 0)),((3 0))]_{4}
Generalized spectral sequence S[((1 0)),((2 0)),((3 0)),((4 0))]_{4}

Component Z/2Z
Generalized spectral sequence S[((2 0)),((3 0)),((4 0)),((5 0))]_{4}
NIL

CAT 92 > 

|#




#Example 2

(cat-init)
(setf A (z-group))
(setf B (gr-crts-prdc (z-group) (z-group)))
(setf cocycle #'(lambda (crpr1 crpr2)
                  (with-grcrpr (x1 y1) crpr1
                    (with-grcrpr (x2 y2) crpr2
                      (div (- (* x1 y2) (* y1 x2)) 2)))))
(setf E (gr-cntr-extn A B cocycle))
(change-class E 'AB-group)
(setf KA1 (k-g-1 A))
(efhm KA1)
(setf KB1 (k-g-1 B))
(setf efhm (gr-crts-prdc-efhm A A))
(setf (slot-value kB1 'efhm) efhm)

(setf KE1 (k-g-1 E))
(setf efhm2 (central-extension-efhm A B cocycle))
(setf (slot-value kE1 'efhm) efhm2)

(setf ka2 (k-g a 2))

(setf tau1 (univ-fbrt-tw A 2))

(setf tw
  (the fibration
    (build-smmr :sorc ka2 :trgt ke1 :degr -1
                :sintr 
                (sintr (cmps (cocycle-fibr-iso2 b a cocycle)
                             (cmps  (twop-incl (cocycle-fibration B A cocycle)) tau1)))
                :orgn `(fibration33 ,tau1))))

(setf ss2 (mmcm-cobar-spectral-system tw))

(PRINT-SPECTRAL-SYSTEM-GROUP ss2 (list '(0 0)) (list '(0 0)) (list '(1 0)) (list '(1 0)) 4)

(setf r 2)
(dotimes (n 5)
  (dotimes (p (1+ n))
    (PRINT-SPECTRAL-SYSTEM-GROUP ss2 (list (list (- p r) 0))  (list (list (- p 1) 0))  (list  (list p 0))  (list (list (- (+ p r) 1) 0)) n)))

(cobar-spectral-system-print-spectral-sequences ss2 5 4)


#|
CAT 92 > (cat-init)

---done---

CAT 93 > (setf A (z-group))
[K1 Abelian-Group]

CAT 94 > (setf B (gr-crts-prdc (z-group) (z-group)))
[K2 Abelian-Group]

CAT 95 > (setf cocycle #'(lambda (crpr1 crpr2)
                  (with-grcrpr (x1 y1) crpr1
                    (with-grcrpr (x2 y2) crpr2
                      (div (- (* x1 y2) (* y1 x2)) 2)))))
#<anonymous interpreted function 406000FD9C>

CAT 96 > (setf E (gr-cntr-extn A B cocycle))
[K3 Group]

CAT 97 > (change-class E 'AB-group)
[K3 Abelian-Group]

CAT 98 > (setf KA1 (k-g-1 A))
[K4 Abelian-Simplicial-Group]

CAT 99 > (efhm KA1)
[K25 Homotopy-Equivalence K4 <= K4 => K19]

CAT 100 > (setf KB1 (k-g-1 B))
[K26 Abelian-Simplicial-Group]

CAT 101 > (setf efhm (gr-crts-prdc-efhm A A))
[K87 Homotopy-Equivalence K26 <= K77 => K56]

CAT 102 > (setf (slot-value kB1 'efhm) efhm)
[K87 Homotopy-Equivalence K26 <= K77 => K56]

CAT 103 > 
(setf KE1 (k-g-1 E))
[K88 Abelian-Simplicial-Group]

CAT 104 > (setf efhm2 (central-extension-efhm A B cocycle))
[K193 Homotopy-Equivalence K88 <= K179 => K175]

CAT 105 > (setf (slot-value kE1 'efhm) efhm2)
[K193 Homotopy-Equivalence K88 <= K179 => K175]

CAT 106 > 
(setf ka2 (k-g a 2))
[K194 Abelian-Simplicial-Group]

CAT 107 > 
(setf tau1 (univ-fbrt-tw A 2))
[K206 Fibration K194 -> K4]

CAT 108 > 
(setf tw
  (the fibration
    (build-smmr :sorc ka2 :trgt ke1 :degr -1
                :sintr 
                (sintr (cmps (cocycle-fibr-iso2 b a cocycle)
                             (cmps  (twop-incl (cocycle-fibration B A cocycle)) tau1)))
                :orgn `(fibration33 ,tau1))))
[K210 Fibration K194 -> K88]

CAT 109 > (setf ss2 (mmcm-cobar-spectral-system tw))
[K506 Spectral-System]

CAT 119 > 
(print "EXAMPLE 2")

"EXAMPLE 2" 
"EXAMPLE 2"

CAT 120 > 
(dotimes (n 5)
  (dotimes (p1 (1+ n))
    (dotimes (p2 (1+ (div n 2)))
      (print (list p1 (- p2)))
            (cobar-e2-2gspsq-group (gfltrcm ss2) p1 (- p2) n))))

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2)),((-1 0) (1 -1)),((0 0) (1 -1)),((0 1) (1 0))]_{0}

Component Z

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2)),((-1 0) (1 -1)),((0 0) (1 -1)),((0 1) (1 0))]_{1}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2)),((0 0) (2 -1)),((1 0)),((1 1))]_{1}

Component Z

Component Z

Component Z

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2) (2 -3)),((-1 0) (1 -1) (2 -2)),((0 0) (1 -1) (2 -2)),((0 1) (1 0) (2 -1))]_{2}

(0 -1) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4)),((-2 0) (-1 -1) (1 -2) (2 -3)),((-2 0) (0 -1) (1 -2) (2 -3)),((0 0) (1 -1) (2 -2))]_{2}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2)),((0 0) (2 -1)),((1 0) (2 -1)),((1 1) (2 0))]_{2}

(1 -1) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (2 -3)),((-1 0) (0 -1) (2 -2)),((-1 0) (1 -1) (2 -2)),((1 0) (2 -1))]_{2}

Component Z

Component Z

(2 0) Generalized spectral sequence S[((0 0) (1 -1) (3 -2)),((1 0) (3 -1)),((2 0)),((2 1))]_{2}

Component Z

Component Z

Component Z

(2 -1) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (3 -3)),((0 0) (1 -1) (3 -2)),((0 0) (2 -1)),((2 0))]_{2}

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2) (2 -3)),((-1 0) (1 -1) (2 -2)),((0 0) (1 -1) (2 -2)),((0 1) (1 0) (2 -1))]_{3}

(0 -1) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4)),((-2 0) (-1 -1) (1 -2) (2 -3)),((-2 0) (0 -1) (1 -2) (2 -3)),((0 0) (1 -1) (2 -2))]_{3}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2)),((0 0) (2 -1)),((1 0) (2 -1)),((1 1) (2 0))]_{3}

(1 -1) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (2 -3)),((-1 0) (0 -1) (2 -2)),((-1 0) (1 -1) (2 -2)),((1 0) (2 -1))]_{3}

(2 0) Generalized spectral sequence S[((0 0) (1 -1) (3 -2)),((1 0) (3 -1)),((2 0)),((2 1))]_{3}

(2 -1) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (3 -3)),((0 0) (1 -1) (3 -2)),((0 0) (2 -1)),((2 0))]_{3}

Component Z

Component Z

Component Z

(3 0) Generalized spectral sequence S[((1 0) (2 -1) (4 -2)),((2 0) (4 -1)),((3 0)),((3 1))]_{3}

Component Z

Component Z

(3 -1) Generalized spectral sequence S[((0 0) (1 -1) (2 -2) (4 -3)),((1 0) (2 -1) (4 -2)),((1 0) (3 -1)),((3 0))]_{3}

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2) (2 -3) (3 -4)),((-1 0) (1 -1) (2 -2) (3 -3)),((0 0) (1 -1) (2 -2) (3 -3)),((0 1) (1 0) (2 -1) (3 -2))]_{4}

(0 -1) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4) (3 -5)),((-2 0) (-1 -1) (1 -2) (2 -3) (3 -4)),((-2 0) (0 -1) (1 -2) (2 -3) (3 -4)),((0 0) (1 -1) (2 -2) (3 -3))]_{4}

(0 -2) Generalized spectral sequence S[((-4 0) (-3 -1) (-2 -2) (-1 -3) (1 -4) (2 -5) (3 -6)),((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4) (3 -5)),((-3 0) (-2 -1) (0 -2) (1 -3) (2 -4) (3 -5)),((-2 0) (0 -1) (1 -2) (2 -3) (3 -4))]_{4}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2) (3 -3)),((0 0) (2 -1) (3 -2)),((1 0) (2 -1) (3 -2)),((1 1) (2 0) (3 -1))]_{4}

(1 -1) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (2 -3) (3 -4)),((-1 0) (0 -1) (2 -2) (3 -3)),((-1 0) (1 -1) (2 -2) (3 -3)),((1 0) (2 -1) (3 -2))]_{4}

(1 -2) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (0 -3) (2 -4) (3 -5)),((-2 0) (-1 -1) (0 -2) (2 -3) (3 -4)),((-2 0) (-1 -1) (1 -2) (2 -3) (3 -4)),((-1 0) (1 -1) (2 -2) (3 -3))]_{4}

(2 0) Generalized spectral sequence S[((0 0) (1 -1) (3 -2)),((1 0) (3 -1)),((2 0) (3 -1)),((2 1) (3 0))]_{4}

(2 -1) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (3 -3)),((0 0) (1 -1) (3 -2)),((0 0) (2 -1) (3 -2)),((2 0) (3 -1))]_{4}

(2 -2) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (1 -3) (3 -4)),((-1 0) (0 -1) (1 -2) (3 -3)),((-1 0) (0 -1) (2 -2) (3 -3)),((0 0) (2 -1) (3 -2))]_{4}

(3 0) Generalized spectral sequence S[((1 0) (2 -1) (4 -2)),((2 0) (4 -1)),((3 0)),((3 1))]_{4}

(3 -1) Generalized spectral sequence S[((0 0) (1 -1) (2 -2) (4 -3)),((1 0) (2 -1) (4 -2)),((1 0) (3 -1)),((3 0))]_{4}

Component Z

Component Z

(3 -2) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (2 -3) (4 -4)),((0 0) (1 -1) (2 -2) (4 -3)),((0 0) (1 -1) (3 -2)),((1 0) (3 -1))]_{4}

(4 0) Generalized spectral sequence S[((2 0) (3 -1) (5 -2)),((3 0) (5 -1)),((4 0)),((4 1))]_{4}

Component Z

Component Z

(4 -1) Generalized spectral sequence S[((1 0) (2 -1) (3 -2) (5 -3)),((2 0) (3 -1) (5 -2)),((2 0) (4 -1)),((4 0))]_{4}

(4 -2) Generalized spectral sequence S[((0 0) (1 -1) (2 -2) (3 -3) (5 -4)),((1 0) (2 -1) (3 -2) (5 -3)),((1 0) (2 -1) (4 -2)),((2 0) (4 -1))]_{4}
NIL

CAT 121 > 
(cobar-spectral-system-print-spectral-sequences ss2 5 4)

"SERRE SPECTRAL SEQUENCE" 
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{0}

Component Z
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{1}
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{1}

Component Z

Component Z
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{2}
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{2}

Component Z

Component Z
Generalized spectral sequence S[((-2 0)),((1 0)),((2 0)),((5 0))]_{2}
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{3}
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{3}
Generalized spectral sequence S[((-2 0)),((1 0)),((2 0)),((5 0))]_{3}

Component Z
Generalized spectral sequence S[((-1 0)),((2 0)),((3 0)),((6 0))]_{3}
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{4}
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{4}
Generalized spectral sequence S[((-2 0)),((1 0)),((2 0)),((5 0))]_{4}
Generalized spectral sequence S[((-1 0)),((2 0)),((3 0)),((6 0))]_{4}
Generalized spectral sequence S[((0 0)),((3 0)),((4 0)),((7 0))]_{4}

"EILENBERG-MOORE SPECTRAL SEQUENCE" 
Generalized spectral sequence S[((0 -4)),((0 -1)),((0 0)),((1 3))]_{0}

Component Z
Generalized spectral sequence S[((1 -4)),((1 -1)),((1 0)),((2 3))]_{1}

Component Z

Component Z
Generalized spectral sequence S[((2 -4)),((2 -1)),((2 0)),((3 3))]_{2}
Generalized spectral sequence S[((2 -5)),((2 -2)),((2 -1)),((3 2))]_{2}

Component Z

Component Z
Generalized spectral sequence S[((3 -4)),((3 -1)),((3 0)),((4 3))]_{3}
Generalized spectral sequence S[((3 -5)),((3 -2)),((3 -1)),((4 2))]_{3}

Component Z
Generalized spectral sequence S[((4 -4)),((4 -1)),((4 0)),((5 3))]_{4}
Generalized spectral sequence S[((4 -5)),((4 -2)),((4 -1)),((5 2))]_{4}
Generalized spectral sequence S[((4 -6)),((4 -3)),((4 -2)),((5 1))]_{4}
NIL

CAT 122 > 
|#

;; Example 3: Hopf fibration

(cat-init)
(defun HOPF (n)
  (declare (fixnum n))
  (the simplicial-mrph
    (build-smmr
     :sorc (sphere 2)
     :trgt (k-z-1)
     :degr -1
     :sintr (hopf-sintr n)
     :orgn `(hopf ,n))))

(defun HOPF-SINTR (n)
  (declare (fixnum n))
  (flet ((rslt (dmns gmsm)
               (declare (ignore dmns gmsm))
               (if (zerop n)
                   (absm 1 +empty-list+)
                 (absm 0 (list n)))))
    (the sintr #'rslt)))


(setf h1 (hopf 1) h2 (hopf 2) h3 (hopf 3) h10 (hopf 10))

(setf t1 (fibration-total h1) t2 (fibration-total h2)
  t3 (fibration-total h3) t10 (fibration-total h10))


(print "EXAMPLE 3")

(setf ss3 (mmcm-cobar-spectral-system h3))

(dotimes (n 5)
  (dotimes (p1 (1+ n))
    (dotimes (p2 (1+ (div n 2)))
      (print (list p1 (- p2)))
            (cobar-e2-2gspsq-group (gfltrcm ss3) p1 (- p2) n))))

(cobar-spectral-system-print-spectral-sequences ss3 5 4)

(print "EXAMPLE 4")

(setf ss10 (mmcm-cobar-spectral-system h10))

(dotimes (n 5)
  (dotimes (p1 (1+ n))
    (dotimes (p2 (1+ (div n 2)))
      (print (list p1 (- p2)))
            (cobar-e2-2gspsq-group (gfltrcm ss10) p1 (- p2) n))))

(cobar-spectral-system-print-spectral-sequences ss10 5 4)


#|
CAT 138 > (defun HOPF (n)
  (declare (fixnum n))
  (the simplicial-mrph
    (build-smmr
     :sorc (sphere 2)
     :trgt (k-z-1)
     :degr -1
     :sintr (hopf-sintr n)
     :orgn `(hopf ,n))))
HOPF

CAT 139 > 
(defun HOPF-SINTR (n)
  (declare (fixnum n))
  (flet ((rslt (dmns gmsm)
               (declare (ignore dmns gmsm))
               (if (zerop n)
                   (absm 1 +empty-list+)
                 (absm 0 (list n)))))
    (the sintr #'rslt)))
HOPF-SINTR

CAT 140 > 

(setf h1 (hopf 1) h2 (hopf 2) h3 (hopf 3) h10 (hopf 10))
[K21 Fibration K1 -> K6]

CAT 141 > 
(setf t1 (fibration-total h1) t2 (fibration-total h2)
  t3 (fibration-total h3) t10 (fibration-total h10))
[K42 Simplicial-Set]

CAT 142 > 

(print "EXAMPLE 3")

"EXAMPLE 3" 
"EXAMPLE 3"

CAT 143 > 
(setf ss3 (mmcm-cobar-spectral-system h3))
[K170 Spectral-System]

CAT 144 > 
(dotimes (n 5)
  (dotimes (p1 (1+ n))
    (dotimes (p2 (1+ (div n 2)))
      (print (list p1 (- p2)))
            (cobar-e2-2gspsq-group (gfltrcm ss3) p1 (- p2) n))))

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2)),((-1 0) (1 -1)),((0 0) (1 -1)),((0 1) (1 0))]_{0}

Component Z

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2)),((-1 0) (1 -1)),((0 0) (1 -1)),((0 1) (1 0))]_{1}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2)),((0 0) (2 -1)),((1 0)),((1 1))]_{1}

Component Z

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2) (2 -3)),((-1 0) (1 -1) (2 -2)),((0 0) (1 -1) (2 -2)),((0 1) (1 0) (2 -1))]_{2}

(0 -1) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4)),((-2 0) (-1 -1) (1 -2) (2 -3)),((-2 0) (0 -1) (1 -2) (2 -3)),((0 0) (1 -1) (2 -2))]_{2}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2)),((0 0) (2 -1)),((1 0) (2 -1)),((1 1) (2 0))]_{2}

(1 -1) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (2 -3)),((-1 0) (0 -1) (2 -2)),((-1 0) (1 -1) (2 -2)),((1 0) (2 -1))]_{2}

Component Z/3Z

(2 0) Generalized spectral sequence S[((0 0) (1 -1) (3 -2)),((1 0) (3 -1)),((2 0)),((2 1))]_{2}

(2 -1) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (3 -3)),((0 0) (1 -1) (3 -2)),((0 0) (2 -1)),((2 0))]_{2}

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2) (2 -3)),((-1 0) (1 -1) (2 -2)),((0 0) (1 -1) (2 -2)),((0 1) (1 0) (2 -1))]_{3}

(0 -1) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4)),((-2 0) (-1 -1) (1 -2) (2 -3)),((-2 0) (0 -1) (1 -2) (2 -3)),((0 0) (1 -1) (2 -2))]_{3}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2)),((0 0) (2 -1)),((1 0) (2 -1)),((1 1) (2 0))]_{3}

(1 -1) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (2 -3)),((-1 0) (0 -1) (2 -2)),((-1 0) (1 -1) (2 -2)),((1 0) (2 -1))]_{3}

(2 0) Generalized spectral sequence S[((0 0) (1 -1) (3 -2)),((1 0) (3 -1)),((2 0)),((2 1))]_{3}

(2 -1) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (3 -3)),((0 0) (1 -1) (3 -2)),((0 0) (2 -1)),((2 0))]_{3}

(3 0) Generalized spectral sequence S[((1 0) (2 -1) (4 -2)),((2 0) (4 -1)),((3 0)),((3 1))]_{3}

Component Z

(3 -1) Generalized spectral sequence S[((0 0) (1 -1) (2 -2) (4 -3)),((1 0) (2 -1) (4 -2)),((1 0) (3 -1)),((3 0))]_{3}

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2) (2 -3) (3 -4)),((-1 0) (1 -1) (2 -2) (3 -3)),((0 0) (1 -1) (2 -2) (3 -3)),((0 1) (1 0) (2 -1) (3 -2))]_{4}

(0 -1) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4) (3 -5)),((-2 0) (-1 -1) (1 -2) (2 -3) (3 -4)),((-2 0) (0 -1) (1 -2) (2 -3) (3 -4)),((0 0) (1 -1) (2 -2) (3 -3))]_{4}

(0 -2) Generalized spectral sequence S[((-4 0) (-3 -1) (-2 -2) (-1 -3) (1 -4) (2 -5) (3 -6)),((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4) (3 -5)),((-3 0) (-2 -1) (0 -2) (1 -3) (2 -4) (3 -5)),((-2 0) (0 -1) (1 -2) (2 -3) (3 -4))]_{4}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2) (3 -3)),((0 0) (2 -1) (3 -2)),((1 0) (2 -1) (3 -2)),((1 1) (2 0) (3 -1))]_{4}

(1 -1) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (2 -3) (3 -4)),((-1 0) (0 -1) (2 -2) (3 -3)),((-1 0) (1 -1) (2 -2) (3 -3)),((1 0) (2 -1) (3 -2))]_{4}

(1 -2) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (0 -3) (2 -4) (3 -5)),((-2 0) (-1 -1) (0 -2) (2 -3) (3 -4)),((-2 0) (-1 -1) (1 -2) (2 -3) (3 -4)),((-1 0) (1 -1) (2 -2) (3 -3))]_{4}

(2 0) Generalized spectral sequence S[((0 0) (1 -1) (3 -2)),((1 0) (3 -1)),((2 0) (3 -1)),((2 1) (3 0))]_{4}

(2 -1) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (3 -3)),((0 0) (1 -1) (3 -2)),((0 0) (2 -1) (3 -2)),((2 0) (3 -1))]_{4}

(2 -2) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (1 -3) (3 -4)),((-1 0) (0 -1) (1 -2) (3 -3)),((-1 0) (0 -1) (2 -2) (3 -3)),((0 0) (2 -1) (3 -2))]_{4}

(3 0) Generalized spectral sequence S[((1 0) (2 -1) (4 -2)),((2 0) (4 -1)),((3 0)),((3 1))]_{4}

(3 -1) Generalized spectral sequence S[((0 0) (1 -1) (2 -2) (4 -3)),((1 0) (2 -1) (4 -2)),((1 0) (3 -1)),((3 0))]_{4}

Component Z

(3 -2) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (2 -3) (4 -4)),((0 0) (1 -1) (2 -2) (4 -3)),((0 0) (1 -1) (3 -2)),((1 0) (3 -1))]_{4}

(4 0) Generalized spectral sequence S[((2 0) (3 -1) (5 -2)),((3 0) (5 -1)),((4 0)),((4 1))]_{4}

(4 -1) Generalized spectral sequence S[((1 0) (2 -1) (3 -2) (5 -3)),((2 0) (3 -1) (5 -2)),((2 0) (4 -1)),((4 0))]_{4}

(4 -2) Generalized spectral sequence S[((0 0) (1 -1) (2 -2) (3 -3) (5 -4)),((1 0) (2 -1) (3 -2) (5 -3)),((1 0) (2 -1) (4 -2)),((2 0) (4 -1))]_{4}
NIL

CAT 145 > 
(cobar-spectral-system-print-spectral-sequences ss3 5 4)

"SERRE SPECTRAL SEQUENCE" 
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{0}

Component Z
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{1}

Component Z
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{1}

Component Z/3Z
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{2}
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{2}
Generalized spectral sequence S[((-2 0)),((1 0)),((2 0)),((5 0))]_{2}
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{3}
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{3}
Generalized spectral sequence S[((-2 0)),((1 0)),((2 0)),((5 0))]_{3}
Generalized spectral sequence S[((-1 0)),((2 0)),((3 0)),((6 0))]_{3}
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{4}
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{4}
Generalized spectral sequence S[((-2 0)),((1 0)),((2 0)),((5 0))]_{4}
Generalized spectral sequence S[((-1 0)),((2 0)),((3 0)),((6 0))]_{4}
Generalized spectral sequence S[((0 0)),((3 0)),((4 0)),((7 0))]_{4}

"EILENBERG-MOORE SPECTRAL SEQUENCE" 
Generalized spectral sequence S[((0 -4)),((0 -1)),((0 0)),((1 3))]_{0}

Component Z
Generalized spectral sequence S[((1 -4)),((1 -1)),((1 0)),((2 3))]_{1}

Component Z/3Z
Generalized spectral sequence S[((2 -4)),((2 -1)),((2 0)),((3 3))]_{2}
Generalized spectral sequence S[((2 -5)),((2 -2)),((2 -1)),((3 2))]_{2}
Generalized spectral sequence S[((3 -4)),((3 -1)),((3 0)),((4 3))]_{3}
Generalized spectral sequence S[((3 -5)),((3 -2)),((3 -1)),((4 2))]_{3}
Generalized spectral sequence S[((4 -4)),((4 -1)),((4 0)),((5 3))]_{4}
Generalized spectral sequence S[((4 -5)),((4 -2)),((4 -1)),((5 2))]_{4}
Generalized spectral sequence S[((4 -6)),((4 -3)),((4 -2)),((5 1))]_{4}
NIL

CAT 146 > 
(print "EXAMPLE 4")

"EXAMPLE 4" 
"EXAMPLE 4"

CAT 147 > 
(setf ss10 (mmcm-cobar-spectral-system h10))
[K253 Spectral-System]

CAT 148 > 
(dotimes (n 5)
  (dotimes (p1 (1+ n))
    (dotimes (p2 (1+ (div n 2)))
      (print (list p1 (- p2)))
            (cobar-e2-2gspsq-group (gfltrcm ss10) p1 (- p2) n))))

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2)),((-1 0) (1 -1)),((0 0) (1 -1)),((0 1) (1 0))]_{0}

Component Z

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2)),((-1 0) (1 -1)),((0 0) (1 -1)),((0 1) (1 0))]_{1}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2)),((0 0) (2 -1)),((1 0)),((1 1))]_{1}

Component Z

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2) (2 -3)),((-1 0) (1 -1) (2 -2)),((0 0) (1 -1) (2 -2)),((0 1) (1 0) (2 -1))]_{2}

(0 -1) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4)),((-2 0) (-1 -1) (1 -2) (2 -3)),((-2 0) (0 -1) (1 -2) (2 -3)),((0 0) (1 -1) (2 -2))]_{2}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2)),((0 0) (2 -1)),((1 0) (2 -1)),((1 1) (2 0))]_{2}

(1 -1) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (2 -3)),((-1 0) (0 -1) (2 -2)),((-1 0) (1 -1) (2 -2)),((1 0) (2 -1))]_{2}

Component Z/10Z

(2 0) Generalized spectral sequence S[((0 0) (1 -1) (3 -2)),((1 0) (3 -1)),((2 0)),((2 1))]_{2}

(2 -1) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (3 -3)),((0 0) (1 -1) (3 -2)),((0 0) (2 -1)),((2 0))]_{2}

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2) (2 -3)),((-1 0) (1 -1) (2 -2)),((0 0) (1 -1) (2 -2)),((0 1) (1 0) (2 -1))]_{3}

(0 -1) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4)),((-2 0) (-1 -1) (1 -2) (2 -3)),((-2 0) (0 -1) (1 -2) (2 -3)),((0 0) (1 -1) (2 -2))]_{3}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2)),((0 0) (2 -1)),((1 0) (2 -1)),((1 1) (2 0))]_{3}

(1 -1) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (2 -3)),((-1 0) (0 -1) (2 -2)),((-1 0) (1 -1) (2 -2)),((1 0) (2 -1))]_{3}

(2 0) Generalized spectral sequence S[((0 0) (1 -1) (3 -2)),((1 0) (3 -1)),((2 0)),((2 1))]_{3}

(2 -1) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (3 -3)),((0 0) (1 -1) (3 -2)),((0 0) (2 -1)),((2 0))]_{3}

(3 0) Generalized spectral sequence S[((1 0) (2 -1) (4 -2)),((2 0) (4 -1)),((3 0)),((3 1))]_{3}

Component Z

(3 -1) Generalized spectral sequence S[((0 0) (1 -1) (2 -2) (4 -3)),((1 0) (2 -1) (4 -2)),((1 0) (3 -1)),((3 0))]_{3}

(0 0) Generalized spectral sequence S[((-2 0) (-1 -1) (1 -2) (2 -3) (3 -4)),((-1 0) (1 -1) (2 -2) (3 -3)),((0 0) (1 -1) (2 -2) (3 -3)),((0 1) (1 0) (2 -1) (3 -2))]_{4}

(0 -1) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4) (3 -5)),((-2 0) (-1 -1) (1 -2) (2 -3) (3 -4)),((-2 0) (0 -1) (1 -2) (2 -3) (3 -4)),((0 0) (1 -1) (2 -2) (3 -3))]_{4}

(0 -2) Generalized spectral sequence S[((-4 0) (-3 -1) (-2 -2) (-1 -3) (1 -4) (2 -5) (3 -6)),((-3 0) (-2 -1) (-1 -2) (1 -3) (2 -4) (3 -5)),((-3 0) (-2 -1) (0 -2) (1 -3) (2 -4) (3 -5)),((-2 0) (0 -1) (1 -2) (2 -3) (3 -4))]_{4}

(1 0) Generalized spectral sequence S[((-1 0) (0 -1) (2 -2) (3 -3)),((0 0) (2 -1) (3 -2)),((1 0) (2 -1) (3 -2)),((1 1) (2 0) (3 -1))]_{4}

(1 -1) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (2 -3) (3 -4)),((-1 0) (0 -1) (2 -2) (3 -3)),((-1 0) (1 -1) (2 -2) (3 -3)),((1 0) (2 -1) (3 -2))]_{4}

(1 -2) Generalized spectral sequence S[((-3 0) (-2 -1) (-1 -2) (0 -3) (2 -4) (3 -5)),((-2 0) (-1 -1) (0 -2) (2 -3) (3 -4)),((-2 0) (-1 -1) (1 -2) (2 -3) (3 -4)),((-1 0) (1 -1) (2 -2) (3 -3))]_{4}

(2 0) Generalized spectral sequence S[((0 0) (1 -1) (3 -2)),((1 0) (3 -1)),((2 0) (3 -1)),((2 1) (3 0))]_{4}

(2 -1) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (3 -3)),((0 0) (1 -1) (3 -2)),((0 0) (2 -1) (3 -2)),((2 0) (3 -1))]_{4}

(2 -2) Generalized spectral sequence S[((-2 0) (-1 -1) (0 -2) (1 -3) (3 -4)),((-1 0) (0 -1) (1 -2) (3 -3)),((-1 0) (0 -1) (2 -2) (3 -3)),((0 0) (2 -1) (3 -2))]_{4}

(3 0) Generalized spectral sequence S[((1 0) (2 -1) (4 -2)),((2 0) (4 -1)),((3 0)),((3 1))]_{4}

(3 -1) Generalized spectral sequence S[((0 0) (1 -1) (2 -2) (4 -3)),((1 0) (2 -1) (4 -2)),((1 0) (3 -1)),((3 0))]_{4}

Component Z

(3 -2) Generalized spectral sequence S[((-1 0) (0 -1) (1 -2) (2 -3) (4 -4)),((0 0) (1 -1) (2 -2) (4 -3)),((0 0) (1 -1) (3 -2)),((1 0) (3 -1))]_{4}

(4 0) Generalized spectral sequence S[((2 0) (3 -1) (5 -2)),((3 0) (5 -1)),((4 0)),((4 1))]_{4}

(4 -1) Generalized spectral sequence S[((1 0) (2 -1) (3 -2) (5 -3)),((2 0) (3 -1) (5 -2)),((2 0) (4 -1)),((4 0))]_{4}

(4 -2) Generalized spectral sequence S[((0 0) (1 -1) (2 -2) (3 -3) (5 -4)),((1 0) (2 -1) (3 -2) (5 -3)),((1 0) (2 -1) (4 -2)),((2 0) (4 -1))]_{4}
NIL

CAT 149 > 
(cobar-spectral-system-print-spectral-sequences ss10 5 4)

"SERRE SPECTRAL SEQUENCE" 
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{0}

Component Z
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{1}

Component Z
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{1}

Component Z/10Z
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{2}
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{2}
Generalized spectral sequence S[((-2 0)),((1 0)),((2 0)),((5 0))]_{2}
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{3}
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{3}
Generalized spectral sequence S[((-2 0)),((1 0)),((2 0)),((5 0))]_{3}
Generalized spectral sequence S[((-1 0)),((2 0)),((3 0)),((6 0))]_{3}
Generalized spectral sequence S[((-4 0)),((-1 0)),((0 0)),((3 0))]_{4}
Generalized spectral sequence S[((-3 0)),((0 0)),((1 0)),((4 0))]_{4}
Generalized spectral sequence S[((-2 0)),((1 0)),((2 0)),((5 0))]_{4}
Generalized spectral sequence S[((-1 0)),((2 0)),((3 0)),((6 0))]_{4}
Generalized spectral sequence S[((0 0)),((3 0)),((4 0)),((7 0))]_{4}

"EILENBERG-MOORE SPECTRAL SEQUENCE" 
Generalized spectral sequence S[((0 -4)),((0 -1)),((0 0)),((1 3))]_{0}

Component Z
Generalized spectral sequence S[((1 -4)),((1 -1)),((1 0)),((2 3))]_{1}

Component Z/10Z
Generalized spectral sequence S[((2 -4)),((2 -1)),((2 0)),((3 3))]_{2}
Generalized spectral sequence S[((2 -5)),((2 -2)),((2 -1)),((3 2))]_{2}
Generalized spectral sequence S[((3 -4)),((3 -1)),((3 0)),((4 3))]_{3}
Generalized spectral sequence S[((3 -5)),((3 -2)),((3 -1)),((4 2))]_{3}
Generalized spectral sequence S[((4 -4)),((4 -1)),((4 0)),((5 3))]_{4}
Generalized spectral sequence S[((4 -5)),((4 -2)),((4 -1)),((5 2))]_{4}
Generalized spectral sequence S[((4 -6)),((4 -3)),((4 -2)),((5 1))]_{4}
NIL

CAT 150 > 
|#

