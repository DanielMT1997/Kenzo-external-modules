;;Example 1

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

(setf ss1 (mmcm-bar-spectral-system tw))
 (PRINT-SPECTRAL-SYSTEM-GROUP ss1 (list '(0 0)) (list '(0 0)) (list '(1 0)) (list '(1 0)) 4)

(setf r 2)
(dotimes (n 5)
  (dotimes (p (1+ n))
    (PRINT-SPECTRAL-SYSTEM-GROUP ss1 (list (list (- p r) 0))  (list (list (- p 1) 0))  (list  (list p 0))  (list (list (- (+ p r) 1) 0)) n)))


#|
Output:

CAT 49 > (PRINT-SPECTRAL-SYSTEM-GROUP ss1 (list '(0 0)) (list '(0 0)) (list '(1 0)) (list '(1 0)) 4)
Generalized spectral sequence S[((0 0)),((0 0)),((1 0)),((1 0))]_{4}
NIL

CAT 50 > (setf r 2)
2

CAT 51 > (dotimes (n 5)
  (dotimes (p (1+ n))
    (PRINT-SPECTRAL-SYSTEM-GROUP ss1 (list (list (- p r) 0))  (list (list (- p 1) 0))  (list  (list p 0))  (list (list (- (+ p r) 1) 0)) n)))
Generalized spectral sequence S[((-2 0)),((-1 0)),((0 0)),((1 0))]_{0}

Component Z
Generalized spectral sequence S[((-2 0)),((-1 0)),((0 0)),((1 0))]_{1}

Component Z/2Z
Generalized spectral sequence S[((-1 0)),((0 0)),((1 0)),((2 0))]_{1}
Generalized spectral sequence S[((-2 0)),((-1 0)),((0 0)),((1 0))]_{2}
Generalized spectral sequence S[((-1 0)),((0 0)),((1 0)),((2 0))]_{2}
Generalized spectral sequence S[((0 0)),((1 0)),((2 0)),((3 0))]_{2}
Generalized spectral sequence S[((-2 0)),((-1 0)),((0 0)),((1 0))]_{3}

Component Z/2Z
Generalized spectral sequence S[((-1 0)),((0 0)),((1 0)),((2 0))]_{3}
Generalized spectral sequence S[((0 0)),((1 0)),((2 0)),((3 0))]_{3}
Generalized spectral sequence S[((1 0)),((2 0)),((3 0)),((4 0))]_{3}
Generalized spectral sequence S[((-2 0)),((-1 0)),((0 0)),((1 0))]_{4}
Generalized spectral sequence S[((-1 0)),((0 0)),((1 0)),((2 0))]_{4}
Generalized spectral sequence S[((0 0)),((1 0)),((2 0)),((3 0))]_{4}
Generalized spectral sequence S[((1 0)),((2 0)),((3 0)),((4 0))]_{4}
Generalized spectral sequence S[((2 0)),((3 0)),((4 0)),((5 0))]_{4}
NIL



|#
