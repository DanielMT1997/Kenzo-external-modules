"-----------------
This is a file containing several examples for multicomplexes using the program Kenzo
------------------"
;;(in-package :cat)

"First example: POSTNIKOV TOWER OF THE SPHERE S^3 WITH TWO FIBRATIONS"
;;(CAT-INIT)

(progn
  (setf B (sphere 3))
  (setf k1 (chml-clss B 3))
  (setf t1 (z-whitehead B k1))
  (setf N (fibration-total t1))
  (setf k0 (chml-clss N 4))
  (setf t0 (z2-whitehead N k0))
  (setf E (fibration-total t0)))
;; [K298 Simplicial-Set]

;;Effective homology for E. 
(setf D (rbcc (efhm e)))

;;The right bottom chain complex is a tensor product. We can see it as an m-multicomplex.
(setf Df (tnpr-to-multicomplex D))


;;We define over it the associated generalized filtration.
(setf Dfltr (change-mmcm-to-gflcc Df 2))

;;; COMPUTATION OF THE LEVEL 2 OF THE SERRE SPECTRAL SYSTEM

(e2-gspsq-group Dfltr '(0 0) 5)
;; Component Z/2Z

(e2-gspsq-group Dfltr '(2 0) 5)
;; Component Z/2Z

(e2-gspsq-group Dfltr '(2 3) 5)
;; Component Z


;;; LEXICOGRAPHICAL CONNECTIONS

(lexcon-gspsq-group Dfltr '(0 0) '(1 1) 5)
;; Component Z/2Z

(lexcon-gspsq-group Dfltr '(2 0) '(1 1) 5)
;; NIL

(lexcon-gspsq-group Dfltr '(2 3) '(1 1) 5)
;; NIL



;;; FINAL GROUP OF THE SPECTRAL SYSTEM FOR DEGREE 5

(final-gspsq-group Dfltr 5)
;; Component Z/2Z


"Second example: POSTNIKOV TOWER OF THE SPHERE S^3 WITH THREE FIBRATIONS"



(cat-init)
(progn
  (setf s3 (sphere 3))
  (setf k3 (chml-clss s3 3))
  (setf F3 (z-whitehead s3 k3))
  (setf X4 (fibration-total F3))
  (setf k4 (chml-clss x4 4))
  (setf F4 (z2-whitehead X4 k4))
  (setf X5 (fibration-total F4))
  (setf k5 (chml-clss x5 5))
  (setf F5 (z2-whitehead X5 k5))
  (setf X6 (fibration-total F5)))


;;Steps similar to those of the previous example
(setf Y (rbcc (efhm X6)))
(setf Yf (tnpr-to-multicomplex Y))
(setf Yfltr (change-mmcm-to-gflcc Yf 3))

;;; LEVEL 2 OF THE SPECTRAL SYSTEM

(e2-gspsq-group Yfltr '(0 6 0) 6)
;; Component Z

;;; LEXICOGRAPHICAL CONNECTION

(lexcon-gspsq-group Yfltr '(0 6 0) '(1 1 1) 6)
;; Component Z/3Z

;;; FINAL GROUP OF THE SPECTRAL SYSTEM FOR DEGREE 6

(final-gspsq-group Yfltr 6)
;;Component Z/12Z



"Third example. Effective example"

(cat-init)
(progn
  (setf kz22 (k-z2 2))
  (setf k2 (chml-clss kz22 4))
  (setf F2 (z2-whitehead kz22 k2))
  (setf X3 (fibration-total f2))
  (setf k3 (chml-clss x3 5))
  (setf F3 (z2-whitehead X3 k3))
  (setf X4 (fibration-total F3))
  (setf k4 (chml-clss x4 6))
  (setf F4 (z2-whitehead X4 k4))
  (setf X5 (fibration-total F4)))
;; [K612 Kan-Simplicial-Set]

(setf Z (rbcc (efhm x5)))
(setf Zf (tnpr-to-multicomplex Z))
(setf Zfltr (change-mmcm-to-gflcc Zf 3))

(e1-eff-gspsq-group Zfltr '(0 0 3) 3)
;; Component Z
;; Sale bien. Es diferente al habitual porque: En el último ejemplo se calcula la primera página de uno de los complejos de cadenas. Si luego se hace con el efectivo directamente (el rbcc con la filtración del producto tensorial) entonces sale lo mismo que me sale a mi con los multicomplejos. No entiendo entonces cómo es capaz el programa de calcular la primera página del sistema espectral. El complejo de cadenas es ya de por sí efectivo, así que los programas funcionan. Lo que pasa es que luego la cadena de reducciones de la homología efectiva es a otro complejo de cadenas diferente, de tipo producto tensorial, y aquí ya no se puede garantizar que sea igual en los dos casos.



"
CAT 7 > (progn
  (setf kz22 (k-z2 2))
  (setf k2 (chml-clss kz22 4))
  (setf F2 (z2-whitehead kz22 k2))
  (setf X3 (fibration-total f2))
  (setf k3 (chml-clss x3 5))
  (setf F3 (z2-whitehead X3 k3))
  (setf X4 (fibration-total F3))
  (setf k4 (chml-clss x4 6))
  (setf F4 (z2-whitehead X4 k4))
  (setf X5 (fibration-total F4)))
[K612 Kan-Simplicial-Set]

CAT 14 > (setf Z (rbcc (efhm x5)))
[K808 Chain-Complex]

CAT 15 > (setf Zf (tnpr-to-multicomplex Z))
[K823 Chain-Complex]

CAT 16 > (setf Zfltr (change-mmcm-to-gflcc Zf 3))
[K827 Generalized-Filtered-Chain-Complex]

CAT 20 > (setf ex5 (rbcc (efhm x5)))
[K808 Chain-Complex]

CAT 21 > ;; [K808 Chain-Complex]

(setf effX5 (change-chcm-to-gflcc ex5 (dzm 3) tnpr3-gflin 'tnpr3-gflin))
[K829 Generalized-Filtered-Chain-Complex]

CAT 22 > ;; [K825 Generalized-Filtered-Chain-Complex]

(setf x5f (change-chcm-to-gflcc x5 (dzm 3) crpr3-gflin 'crpr3-gflin))
[K831 Generalized-Filtered-Chain-Complex]

CAT 23 > ;; [K827 Generalized-Filtered-Chain-Complex]

(setf x5 x5f)
[K831 Generalized-Filtered-Chain-Complex]

CAT 24 > ;;[K827 Generalized-Filtered-Chain-Complex]

(e1-eff-gspsq-group X5 '(0 0 3) 3)
Generalized spectral sequence S[((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0))]_{3}

Component Z

Component Z

Component Z

Component Z

Component Z

Component Z

Component Z

Component Z
NIL


CAT 46 > (gen-eff-spsq-group effx5 z q p b degr)
Generalized spectral sequence S[((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0))]_{3}

Component Z
NIL

CAT 25 > (e1-eff-gspsq-group Zfltr '(0 0 3) 3)
Generalized spectral sequence S[((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0))]_{3}

Component Z
NIL

CAT 26 > (setf point '(0 0 3))
(0 0 3)

CAT 27 > (setf p (t3-downset-list point))
((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0))

CAT 28 > (setf q (t3-downset-list (2-npoints-add point (list 0 1 -1))))
((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0))

CAT 29 > (setf z q)
((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0))

CAT 30 > (setf b p)
((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0))

CAT 31 > (setf degr 3)
3


CAT 37 > (setf basis-dfvs1 (gen-eff-spsq-basis-dvs Zfltr z q p b degr))
((
----------------------------------------------------------------------{CMBN 3}
<1 * (MMCGNRT (0 3 0 0) (# # # #))>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * (MMCGNRT (3 0 0 0) (# # # #))>
------------------------------------------------------------------------------
) (1 0))

CAT 38 > (setf basis-dfvs1 (gen-eff-spsq-basis-dvs x5 z q p b degr))
((
----------------------------------------------------------------------{CMBN 3}
<1 * <CrPr - <CrPr - # 2-1-0 #> 2-1-0 <<GBar>>>>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * <CrPr - <CrPr - # 2-1-0 #> 2-1-0 <<GBar>>>>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * <CrPr - <CrPr - # 2-1-0 #> 2-1-0 <<GBar>>>>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * <CrPr - <CrPr - # 2-1-0 #> 2-1-0 <<GBar>>>>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * <CrPr - <CrPr - # 2-1-0 #> 2-1-0 <<GBar>>>>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * <CrPr - <CrPr - # 2-1-0 #> 2-1-0 <<GBar>>>>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * <CrPr - <CrPr - # 2-1-0 #> 2-1-0 <<GBar>>>>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * <CrPr - <CrPr - # 2-1-0 #> 2-1-0 <<GBar>>>>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * <CrPr - <CrPr - # 2-1-0 #> 2-1-0 <<GBar>>>>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * <CrPr - <CrPr - # 2-1-0 #> 2-1-0 <<GBar>>>>
------------------------------------------------------------------------------
 ...) (1 1 1 1 0 0 0 0 0 0 ...))



CAT 45 > (setf basis-dfvs1 (gen-eff-spsq-basis-dvs effx5 z q p b degr))
((
----------------------------------------------------------------------{CMBN 3}
<1 * <TnPr <TnPr # #> <<Abar>>>>
------------------------------------------------------------------------------
 
----------------------------------------------------------------------{CMBN 3}
<1 * <TnPr <TnPr # #> <<Abar>>>>
------------------------------------------------------------------------------
) (1 0))
"

(cat-init)
(progn
  (setf kz22 (k-z2 2))
  (setf k2 (chml-clss kz22 4))
  (setf F2 (z2-whitehead kz22 k2))
  (setf X3 (fibration-total f2))
  (setf k3 (chml-clss x3 5))
  (setf F3 (z2-whitehead X3 k3))
  (setf X4 (fibration-total F3))
  (setf k4 (chml-clss x4 6))
  (setf F4 (z2-whitehead X4 k4))
  (setf X5 (fibration-total F4)))

;;Test with the intermediate chain complex. Does not work yet.

(setf intm-chcm (SERRE-INTM-CHCM (second (orgn x5))))
(setf intm-chcm-f (change-chcm-to-gflcc intm-chcm (dzm 3) tnpr3-gflin 'tnpr3-gflin))
(e1-eff-gspsq-group intm-chcm-f '(0 0 3) 3)

;;Now as 3-multicomplex.

(setf mmcm-intm-chcm (tnpr-to-multicomplex intm-chcm))
(setf mmcm-intm-chcm-f (change-mmcm-to-gflcc mmcm-intm-chcm 3))
(e1-eff-gspsq-group mmcm-intm-chcm-f '(0 0 3) 3) 



"
These are different to the ones of X5, but it is ok since it is the first page.

CAT 10 > (setf intm-chcm (SERRE-INTM-CHCM (second (orgn x5))))
[K692 Chain-Complex]

CAT 11 > (setf intm-chcm-f (change-chcm-to-gflcc intm-chcm (dzm 3) tnpr3-gflin 'tnpr3-gflin))
[K703 Generalized-Filtered-Chain-Complex]

CAT 12 > (e1-eff-gspsq-group intm-chcm-f '(0 0 3) 3)
Generalized spectral sequence S[((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0))]_{3}

Component Z

Component Z

Component Z

Component Z
NIL

CAT 13 > (setf mmcm-intm-chcm (tnpr-to-multicomplex intm-chcm))
[K705 Chain-Complex]

CAT 14 > (setf mmcm-intm-chcm-f (change-mmcm-to-gflcc mmcm-intm-chcm 3))
[K707 Generalized-Filtered-Chain-Complex]

CAT 15 > (e1-eff-gspsq-group mmcm-intm-chcm-f '(0 0 3) 3) 
Generalized spectral sequence S[((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 0 3) (0 1 2) (0 2 1) (0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0))]_{3}

Component Z

Component Z

Component Z

Component Z
NIL

CAT 16 > 
"
            
 (setf x5f (change-chcm-to-gflcc x5 (dzn 3) crpr3-gflin 'crpr3-gflin))
(dotimes (p1 5)
  (dotimes (p2 5)
    (dotimes (p3 5)
      (print (list p1 p2 p3))
            (e2-eff-3gspsq-group x5f p1 p2 p3 3))))
            
"
(e2-eff-3gspsq-group x5f 0 3 0 3)
Generalized spectral sequence S[((1 0 1) (1 1 0) (2 0 0)),((1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 3 1) (0 4 0) (1 0 3) (1 1 2) (1 2 1) (1 3 0) (2 0 2) (2 1 1) (2 2 0) (3 0 1) ...)]_{3}

Component Z/2Z"
(dotimes (p1 5)
  (dotimes (p2 5)
    (dotimes (p3 5)
      (print (list p1 p2 p3))
            (e2-eff-3gspsq-group intm-chcm-f p1 p2 p3 3))))

"
These are all null except for the '(0 3 0), which is Z/2Z. One may change the 3 on the right to compute for different degrees of the complexes.
"
            
(dotimes (p1 5)
  (dotimes (p2 5)
    (dotimes (p3 5)
      (print (list p1 p2 p3))
            (e2-eff-3gspsq-group mmcm-intm-chcm-f p1 p2 p3 3))))
            
" 
(e2-eff-3gspsq-group mmcm-intm-chcm-f 0 3 0 3)
Generalized spectral sequence S[((1 0 1) (1 1 0) (2 0 0)),((1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 3 0) (1 0 2) (1 1 1) (1 2 0) (2 0 1) (2 1 0) (3 0 0)),((0 3 1) (0 4 0) (1 0 3) (1 1 2) (1 2 1) (1 3 0) (2 0 2) (2 1 1) (2 2 0) (3 0 1) ...)]_{3}

Component Z/2Z
"



;; Tests for the serre intermediate chain complex with the first one of the previous examples. Using the effective homology for chain complexes coming from add. We obtain the same results as in that example.

(progn
  (setf s3 (sphere 3))
  (setf k3 (chml-clss s3 3))
  (setf F3 (z-whitehead s3 k3))
  (setf X4 (fibration-total F3))
  (setf k4 (chml-clss x4 4))
  (setf F4 (z2-whitehead X4 k4))
  (setf X5 (fibration-total F4))
  (setf k5 (chml-clss x5 5))
  (setf F5 (z2-whitehead X5 k5))
  (setf X6 (fibration-total F5)))
  
(efhm (serre-intm-chcm f5))
(setf inteff (rbcc (efhm (serre-intm-chcm f5))))
(setf inteff-mmcm (tnpr-to-multicomplex inteff))
(setf inteff-mmcm-f (change-mmcm-to-gflcc inteff-mmcm 3))
(e2-gspsq-group inteff-mmcm-f '(0 6 0) 6)
(lexcon-gspsq-group inteff-mmcm-f '(0 6 0) '(1 1 1) 6)
(final-gspsq-group inteff-mmcm-f 6)


  
"
CAT 47 > 
(progn
  (setf s3 (sphere 3))
  (setf k3 (chml-clss s3 3))
  (setf F3 (z-whitehead s3 k3))
  (setf X4 (fibration-total F3))
  (setf k4 (chml-clss x4 4))
  (setf F4 (z2-whitehead X4 k4))
  (setf X5 (fibration-total F4))
  (setf k5 (chml-clss x5 5))
  (setf F5 (z2-whitehead X5 k5))
  (setf X6 (fibration-total F5)))
[K630 Simplicial-Set]

CAT 48 > (efhm (serre-intm-chcm f5))
[K922 Homotopy-Equivalence K710 <= K905 => K919]

CAT 49 > (setf inteff (rbcc (efhm (serre-intm-chcm f5))))
[K919 Chain-Complex]

CAT 50 > (setf inteff-mmcm (tnpr-to-multicomplex inteff))
[K923 Chain-Complex]



CAT 55 > (setf inteff-mmcm-f (change-mmcm-to-gflcc inteff-mmcm 3))
[K927 Generalized-Filtered-Chain-Complex]

CAT 56 > (e2-gspsq-group inteff-mmcm-f '(0 6 0) 6)
Generalized spectral sequence S[((1 0 4) (1 1 3) (1 2 2) (1 3 1) (1 4 0) (2 0 3) (2 1 2) (2 2 1) (2 3 0) (3 0 2) ...),((1 0 5) (1 1 4) (1 2 3) (1 3 2) (1 4 1) (1 5 0) (2 0 4) (2 1 3) (2 2 2) (2 3 1) ...),((0 6 0) (1 0 5) (1 1 4) (1 2 3) (1 3 2) (1 4 1) (1 5 0) (2 0 4) (2 1 3) (2 2 2) ...),((0 6 1) (0 7 0) (1 0 6) (1 1 5) (1 2 4) (1 3 3) (1 4 2) (1 5 1) (1 6 0) (2 0 5) ...)]_{6}

Component Z
NIL

CAT 57 > 
(lexcon-gspsq-group inteff-mmcm-f '(0 6 0) '(1 1 1) 6)
Generalized spectral sequence S[((0 0 2) (0 1 1) (0 2 0) (1 0 1) (1 1 0) (2 0 0)),((1 0 5) (1 1 4) (1 2 3) (1 3 2) (1 4 1) (1 5 0) (2 0 4) (2 1 3) (2 2 2) (2 3 1) ...),((0 6 0) (1 0 5) (1 1 4) (1 2 3) (1 3 2) (1 4 1) (1 5 0) (2 0 4) (2 1 3) (2 2 2) ...),((0 0 9) (0 1 8) (0 2 7) (0 3 6) (0 4 5) (0 5 4) (0 6 3) (1 7 2) (1 8 1) (1 9 0) ...)]_{6}

Component Z/3Z
NIL

CAT 58 > 
(final-gspsq-group inteff-mmcm-f 6)
Generalized spectral sequence S[NIL,NIL,((0 0 17) (0 1 16) (0 2 15) (0 3 14) (0 4 13) (0 5 12) (0 6 11) (0 7 10) (0 8 9) (0 9 8) ...),((0 0 17) (0 1 16) (0 2 15) (0 3 14) (0 4 13) (0 5 12) (0 6 11) (0 7 10) (0 8 9) (0 9 8) ...)]_{6}

Component Z/12Z
NIL

"


;Test for the effective homology for multicomplexes with the same example. We use the method search-efhm for tnpr-to-mmcm.

(cat-init)
(progn
  (setf s3 (sphere 3))
  (setf k3 (chml-clss s3 3))
  (setf F3 (z-whitehead s3 k3))
  (setf X4 (fibration-total F3))
  (setf k4 (chml-clss x4 4))
  (setf F4 (z2-whitehead X4 k4))
  (setf X5 (fibration-total F4))
  (setf k5 (chml-clss x5 5))
  (setf F5 (z2-whitehead X5 k5))
  (setf X6 (fibration-total F5)))
  
(setf serre-intm-mmcm (tnpr-to-multicomplex (serre-intm-chcm f5)))
(setf serre-intm-eff-mmcm (rbcc (efhm serre-intm-mmcm)))
(setf inteff-mmcm-eff-f (change-mmcm-to-gflcc serre-intm-eff-mmcm  3))
(e2-gspsq-group inteff-mmcm-eff-f '(0 6 0) 6)
(lexcon-gspsq-group inteff-mmcm-eff-f '(0 6 0) '(1 1 1) 6)
(final-gspsq-group inteff-mmcm-eff-f 6)
"
CAT 54 > (progn
  (setf s3 (sphere 3))
  (setf k3 (chml-clss s3 3))
  (setf F3 (z-whitehead s3 k3))
  (setf X4 (fibration-total F3))
  (setf k4 (chml-clss x4 4))
  (setf F4 (z2-whitehead X4 k4))
  (setf X5 (fibration-total F4))
  (setf k5 (chml-clss x5 5))
  (setf F5 (z2-whitehead X5 k5))
  (setf X6 (fibration-total F5)))
[K630 Simplicial-Set]

CAT 55 > (setf serre-intm-mmcm (tnpr-to-multicomplex (serre-intm-chcm f5)))
[K719 Chain-Complex]

CAT 58 > (setf serre-intm-eff-mmcm (rbcc (efhm serre-intm-mmcm)))
[K930 Chain-Complex]

CAT 59 > (setf inteff-mmcm-eff-f (change-mmcm-to-gflcc serre-intm-eff-mmcm  3))
[K940 Generalized-Filtered-Chain-Complex]

CAT 60 > (e2-gspsq-group inteff-mmcm-eff-f '(0 6 0) 6)
Generalized spectral sequence S[((1 0 4) (1 1 3) (1 2 2) (1 3 1) (1 4 0) (2 0 3) (2 1 2) (2 2 1) (2 3 0) (3 0 2) ...),((1 0 5) (1 1 4) (1 2 3) (1 3 2) (1 4 1) (1 5 0) (2 0 4) (2 1 3) (2 2 2) (2 3 1) ...),((0 6 0) (1 0 5) (1 1 4) (1 2 3) (1 3 2) (1 4 1) (1 5 0) (2 0 4) (2 1 3) (2 2 2) ...),((0 6 1) (0 7 0) (1 0 6) (1 1 5) (1 2 4) (1 3 3) (1 4 2) (1 5 1) (1 6 0) (2 0 5) ...)]_{6}

Component Z
NIL

CAT 61 > (lexcon-gspsq-group inteff-mmcm-eff-f '(0 6 0) '(1 1 1) 6)
Generalized spectral sequence S[((0 0 2) (0 1 1) (0 2 0) (1 0 1) (1 1 0) (2 0 0)),((1 0 5) (1 1 4) (1 2 3) (1 3 2) (1 4 1) (1 5 0) (2 0 4) (2 1 3) (2 2 2) (2 3 1) ...),((0 6 0) (1 0 5) (1 1 4) (1 2 3) (1 3 2) (1 4 1) (1 5 0) (2 0 4) (2 1 3) (2 2 2) ...),((0 0 9) (0 1 8) (0 2 7) (0 3 6) (0 4 5) (0 5 4) (0 6 3) (1 7 2) (1 8 1) (1 9 0) ...)]_{6}

Component Z/3Z
NIL

CAT 62 > (final-gspsq-group inteff-mmcm-eff-f 6)
Generalized spectral sequence S[NIL,NIL,((0 0 17) (0 1 16) (0 2 15) (0 3 14) (0 4 13) (0 5 12) (0 6 11) (0 7 10) (0 8 9) (0 9 8) ...),((0 0 17) (0 1 16) (0 2 15) (0 3 14) (0 4 13) (0 5 12) (0 6 11) (0 7 10) (0 8 9) (0 9 8) ...)]_{6}

Component Z/12Z
NIL

"
