;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COBAR COMPLEXES AS M-MULTICOMPLEXES AS COBAR COMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(IN-PACKAGE #:cat)

(provide "mmulticomplexes.lisp")

(DEFUN GNRT-EMBI-TO-MMULTICOMPLEX (gnrt)
  "--------------------------------------------------------------[function-doc]
Input: a generator over a complex of type Cobar tensored with a tensor product (from now on, EMBI). This corresponds to the Eilenberg-Moore-bicomplex-I
Output: the same generator seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type tnpr gnrt))
	(the MmcGnrt (build-mmcgnrt (embi-multidegree gnrt) (embi-mmcmgnrt gnrt))))   

#|		
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
(setf gnrt1 (first (basis comp 1)))
(gnrt-embi-to-mmulticomplex gnrt1)

CAT 73 > (setf comp (eilenberg-moore-bicomplex tw))
[K216 Chain-Complex]

CAT 74 > (setf gnrt1 (first (basis comp 1)))
<TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1))>>

CAT 75 > (gnrt-embi-to-mmulticomplex gnrt1) 
(:MMCGNRT (1 0 0) (<<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1))>))                
|#

			
(DEFUN EMBI-MULTIDEGREE (gnrt)
  "--------------------------------------------------------------[function-doc]
Input: a generator of type EMBI.
Output: returns the multidegree associated to the generator.
------------------------------------------------------------------------------"
	(declare (type tnpr gnrt))
	(let* ((degr1 (degr1 gnrt))
        	(allp (gnrt1 gnrt))
        	(degr2 (degr2 gnrt))
        	(leng (with-allp (l) allp (the fixnum (- (length l))))))
		(declare (type fixnum degr1 degr2 leng)
			(type list l))
		(the list (list degr2 leng (- degr1 leng)))))


(DEFUN EMBI-MMCMGNRT (gnrt)
  "--------------------------------------------------------------[function-doc]
Input: a generator of type EMBI.
Output: the generator associated to it.
------------------------------------------------------------------------------"
	(declare (type tnpr gnrt))
	(let* ((allp (gnrt1 gnrt))
        	(tnpr2 (gnrt2 gnrt)))
        	(declare (type allp allp)
        		(type tnpr gnrt))
        	(the list (list allp tnpr2))))



(DEFUN GNRT-MMULTICOMPLEX-TO-EMBI (mmcgnrt)
  "--------------------------------------------------------------[function-doc]
Input: a generator of type m-multicomplex that came from an EMBI complex.
Output: the same generator as an EMBI. 
------------------------------------------------------------------------------"
	(declare (type mmcgnrt mmcgnrt))
	(let* ((degr2 (first (mmcgnrt-mdegr mmcgnrt)))
		(leng (second (mmcgnrt-mdegr mmcgnrt)))
		(degr1 (+ (third (mmcgnrt-mdegr mmcgnrt)) leng))
		(allp (first (mmcgnrt-gnrt mmcgnrt)))
		(tnpr2 (second (mmcgnrt-gnrt mmcgnrt))))
		(declare (type fixnum degr2 degr1 leng)
			(type allp allp)
			(type tnpr tnpr2))
		(the tnpr (tnpr degr1 allp degr2 tnpr2))))

#|
CAT 78 > (gnrt-mmulticomplex-to-embi (gnrt-embi-to-mmulticomplex gnrt1))
<TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1))>>
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS TO CHANGE A COMPLEX OF TYPE EMBI TO M-MULTICOMPLEX ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN MMCM-CMPR-OF-EMBI (chcm)
  "--------------------------------------------------------------[function-doc]
Input: recieves a chain complex with generators of the form embi.
Output: returns the same comparison function of that chain complex but such that it applies on generators of type mmcgnrt.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(let ((cmpr-embi (cmpr chcm)))
		(flet ((comp (mmc1 mmc2)
			(declare (type MmcGnrt mmc1 mmc2))
			(funcall cmpr-embi (gnrt-mmulticomplex-to-embi mmc1) (gnrt-mmulticomplex-to-embi mmc2))
			)) 
			(the cmprf #'comp))))

#|




;;;
CAT 85 > (setf gnrt2 (second (basis comp 1)))
<TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 0))>>
CAT 87 > (funcall (cmpr comp) gnrt1 gnrt2)
:LESS
CAT 92 > (funcall (mmcm-cmpr-of-embi comp) (gnrt-embi-to-mmulticomplex gnrt1) (gnrt-embi-to-mmulticomplex gnrt2))
:LESS


CAT 103 > (dotimes (degr 3)
  (dotimes (i 2)
    (dotimes (j 2)
      (print  (funcall (cmpr comp) (nth (+ 1 i) (basis comp (+ 1 degr))) (nth (+ 1 j) (basis comp (+ 1 degr)))))
             (print (funcall (mmcm-cmpr-of-embi comp) (gnrt-embi-to-mmulticomplex (nth (+ 1 i) (basis comp (+ 1 degr)))) (gnrt-embi-to-mmulticomplex (nth (+ 1 j) (basis comp (+ 1 degr)))))))))

:EQUAL 
:EQUAL 
:LESS 
:LESS 
:GREATER 
:GREATER 
:EQUAL 
:EQUAL 
:EQUAL 
:EQUAL 
:LESS 
:LESS 
:GREATER 
:GREATER 
:EQUAL 
:EQUAL 
:EQUAL 
:EQUAL 
:LESS 
:LESS 
:GREATER 
:GREATER 
:EQUAL 
:EQUAL 
NIL

|#			
			
(DEFUN MMCM-BASIS-OF-EMBI (chcm)
  "--------------------------------------------------------------[function-doc]
Input: chain complex of the form embi.
Output: basis of the same chain complex seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(let ((embibasis (basis chcm)))
		(declare (type basis embibasis))
		(when (eq embibasis :locally-effective)
			(return-from mmcm-basis-of-embi :locally-effective))
		
		(flet ((bas (degr)
			(declare (type fixnum degr))
			(the list 
				(mapcar
					#'(lambda (gnrt)
						(gnrt-embi-to-mmulticomplex gnrt)
					)
				(funcall embibasis degr)))))
		(the basis #'bas))))	

#|
CAT 104 > (basis comp 1)
(<TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 0))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 1))>> <TnPr <<AlLp[1 <<GBar<- (1)><- NIL>>>]>> <TnPr <<GBar>> NIL>>)

CAT 105 > (funcall (mmcm-basis-of-embi comp) 1)
((:MMCGNRT (1 0 0) (<<AlLp>> <TnPr <<GBar>> (#)>)) (:MMCGNRT (1 0 0) (<<AlLp>> <TnPr <<GBar>> (#)>)) (:MMCGNRT (1 0 0) (<<AlLp>> <TnPr <<GBar>> (#)>)) (:MMCGNRT (0 -1 2) (<<AlLp[1 <<GBar<- #><- NIL>>>]>> <TnPr <<GBar>> NIL>)))

CAT 106 > (basis comp 2)
(<TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1) (GRCRPR 0 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1) (GRCRPR 1 . 0))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1) (GRCRPR 1 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 0) (GRCRPR 0 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 0) (GRCRPR 1 . 0))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 0) (GRCRPR 1 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 1) (GRCRPR 0 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 1) (GRCRPR 1 . 0))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 1) (GRCRPR 1 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar<- (1)><- NIL>>> NIL>> ...)

CAT 107 > (funcall (mmcm-basis-of-embi comp) 2)
((:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar<- #><- NIL>>> NIL>)) ...)
|#
		
(DEFUN MMCM-DFFR-OF-EMBI (chcm)
  "--------------------------------------------------------------[function-doc]
Input: chain complex of the form embi.
Output: its internal differential seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(flet ((dif (degr mmc)
		(declare (type fixnum degr)
			(type MmcGnrt mmc))
		(the cmbn 
			(make-cmbn
				:degr (- degr 1)
				:list (mapcar #'(lambda (term)
						(declare (type term term))
						(let ((cffc (cffc term))
							(gnrt (gnrt term)))
							(declare (type fixnum cffc)
								(type gnrt gnrt))
							(term cffc (gnrt-embi-to-mmulticomplex gnrt))))
				(cmbn-list (dffr chcm degr (gnrt-mmulticomplex-to-embi mmc)))))
		)))
	(the intr-mrph #'dif)))
	
#|
CAT 108 > (dffr comp 2 (first (basis comp 2)))

----------------------------------------------------------------------{CMBN 1}
<2 * <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1))>>>
<-1 * <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 0))>>>
------------------------------------------------------------------------------


CAT 109 > (funcall (mmcm-dffr-of-embi comp) 2 (gnrt-embi-to-mmulticomplex (first (basis comp 2))))

----------------------------------------------------------------------{CMBN 1}
<2 * (MMCGNRT (1 0 0) (<<AlLp>> <TnPr <<GBar>> (#)>))>
<-1 * (MMCGNRT (1 0 0) (<<AlLp>> <TnPr <<GBar>> (#)>))>
------------------------------------------------------------------------------

CAT 110 > 
|#	
	
	
(DEFUN EMBI-TO-MULTICOMPLEX (chcm)
  "--------------------------------------------------------------[function-doc]
Input: a chain complex of the form embi.
Output: the same chain complex seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(the chain-complex
		(let ((chcm
			(build-chcm
				:cmpr (mmcm-cmpr-of-embi chcm)
				:basis (mmcm-basis-of-embi chcm)
				:intr-dffr (mmcm-dffr-of-embi chcm)
				:strt :gnrt
				:bsgn (if (eq (bsgn chcm) nil)
						nil
						(gnrt-embi-to-mmulticomplex (bsgn chcm))
					) 
				:orgn (append (list 'embi-TO-MMC) (orgn chcm)))))
			(declare (type chain-complex chcm))
		chcm)))

#|
CAT 110 > (setf mmcm-comp (embi-to-multicomplex comp))
[K218 Chain-Complex]

CAT 111 > comp
[K216 Chain-Complex]

CAT 112 > (basis comp 1)
(<TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 0))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 1))>> <TnPr <<AlLp[1 <<GBar<- (1)><- NIL>>>]>> <TnPr <<GBar>> NIL>>)

CAT 113 > (basis comp 2)
(<TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1) (GRCRPR 0 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1) (GRCRPR 1 . 0))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 0 . 1) (GRCRPR 1 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 0) (GRCRPR 0 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 0) (GRCRPR 1 . 0))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 0) (GRCRPR 1 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 1) (GRCRPR 0 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 1) (GRCRPR 1 . 0))>> <TnPr <<AlLp>> <TnPr <<GBar>> ((GRCRPR 1 . 1) (GRCRPR 1 . 1))>> <TnPr <<AlLp>> <TnPr <<GBar<- (1)><- NIL>>> NIL>> ...)

CAT 114 > (basis mmcm-comp 1)
((:MMCGNRT (1 0 0) (<<AlLp>> <TnPr <<GBar>> (#)>)) (:MMCGNRT (1 0 0) (<<AlLp>> <TnPr <<GBar>> (#)>)) (:MMCGNRT (1 0 0) (<<AlLp>> <TnPr <<GBar>> (#)>)) (:MMCGNRT (0 -1 2) (<<AlLp[1 <<GBar<- #><- NIL>>>]>> <TnPr <<GBar>> NIL>)))


CAT 117 > (basis mmcm-comp 2)
((:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>)) (:MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar<- #><- NIL>>> NIL>)) ...)


CAT 120 > (funcall (cmpr mmcm-comp) (first (basis mmcm-comp 2)) (second (basis mmcm-comp 2)))
:LESS

CAT 121 > (bsgn mmcm-comp)
(:MMCGNRT (0 0 0) (<<AlLp>> <TnPr <<GBar>> NIL>))

CAT 122 > (dffr mmcm-comp 1 (first (basis mmcm-comp 1)))

----------------------------------------------------------------------{CMBN 0}
------------------------------------------------------------------------------

CAT 123 > (dffr mmcm-comp 3 (first (basis mmcm-comp 3)))

----------------------------------------------------------------------{CMBN 2}
<1 * (MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>))>
<-1 * (MMCGNRT (2 0 0) (<<AlLp>> <TnPr <<GBar>> (# #)>))>
------------------------------------------------------------------------------

CAT 124 > 
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COBAR SPECTRAL SYSTEM WITH MMULTICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFVAR MMCM-COBAR-GFLIN)
  "--------------------------------------------------------------[variable-doc]
Definition of the filtration for the generators of the cobar. 
------------------------------------------------------------------------------"
(SETF MMCM-COBAR-GFLIN
	#'(lambda (degr gnrt)
		(declare (ignore degr)
			(type mmcgnrt gnrt))
		(let ((mdegr (MmcGnrt-Mdegr gnrt)))
			(the list (list (list (list (first Mdegr) (second Mdegr))))))))

(DEFUN CHECK-MMCM-COBAR-GFLIN (fltrcm n1 &rest rest)
  "--------------------------------------------------------------[function-doc]
Function that checks whether the previous filtration is well defined.

Input:a filtered chain complex and an integer or two.
Output: t if the differential is well defined as a morphism of generalized filtered chain complex, that is, it preserves the index of filtration.
------------------------------------------------------------------------------"
	(let ((gf (change-chcm-to-gflcc fltrcm (dz2) mmcm-cobar-gflin '(mmcm-cobar-gflin))))
		(let ((n2 (if (> (length rest) 0) (first rest) (1+ n1))))
			(dotimes (n0 (- n2 n1))
				(let ((n (+ n0 n1)))
					(dotimes (i (length (basis gf n)))
						(if (not (gfltrd-mrph-order-p (dffr gf) n (nth i (basis gf n)) '(0 0)))
                					(return-from check-mmcm-cobar-gflin nil)))))
      			t)))
      			
#|      			
CAT 23 > (check-mmcm-cobar-gflin mmcm-comp 0 5)
T
|#



(DEFUN MMCM-COBAR-SPECTRAL-SYSTEM (fibration)
  "--------------------------------------------------------------[function-doc]
Function that defines the cobar spectral system associated to a fibration, but seen as a 3-multicomplex. We take the effective homology of the eilenberg-moore bicomplex of a fibration. Then we filter it with the cobar filtration and we build the associated spectral system.

Input:a fibration given by a twisted cartesian product.
Output: the associated spectral system.
------------------------------------------------------------------------------"
  (let* ((k (eilenberg-moore-bicomplex fibration))
         (ecc (rbcc (efhm k)))
         (ecc-mmcm (embi-to-multicomplex ecc))
         (k-mmcm (embi-to-multicomplex k)))
    (declare 
     (type chain-complex k)
     (type chain-complex ecc)
     (type chain-complex ecc-mmcm)
     (type chain-complex k-mmcm))
    (progn
      (setf k-mmcm (change-chcm-to-gflcc k-mmcm (dz2) mmcm-cobar-gflin '(mmcm-cobar-gflin)))
      (setf ecc-mmcm (change-chcm-to-gflcc ecc-mmcm (dz2) mmcm-cobar-gflin '(mmcm-cobar-gflin)))
      (the spectral-system
        (build-spectral-system ecc-mmcm `(cobar-spectral-system-mmcm ,fibration))))))  
        

#|     
For the examples see the file "cobar-mmulticomplex-examples.lisp".
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BAR COMPLEXES AS M-MULTICOMPLEXES AS COBAR COMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN GNRT-EMBII-TO-MMULTICOMPLEX (gnrt)
  "--------------------------------------------------------------[function-doc]
Input: a generator over a complex of type bar tensored with a tensor product (from now on, EMBII). This corresponds to the Eilenberg-Moore-Bicomplex-II
Output: the same generator seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type tnpr gnrt))
	(the MmcGnrt (build-MmcGnrt (embii-multidegree gnrt) (embii-mmcmgnrt gnrt))))


#|
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
                
                
(setf comp (eilenberg-moore-bicomplex-ii tw))
(setf gnrt1 (first (basis comp 1)))
(gnrt-embii-to-mmulticomplex gnrt1)
		
CAT 8 > (setf A (cyclicGroup 2))
[K1 Abelian-Group]

CAT 9 > (setf B (cyclicGroup 2))
[K1 Abelian-Group]

CAT 10 > (setf KA1 (k-g-1 A))
[K2 Abelian-Simplicial-Group]

CAT 11 > (setf KB1 (k-g-1 B))                    
[K2 Abelian-Simplicial-Group]

CAT 12 > 
(setf cocycle #'(lambda (z1 z2)
                  (if (and (= z1 1) (= z2 1))
                      1 0)))
#<anonymous interpreted function 406000C91C>

CAT 13 > 
(setf E (gr-cntr-extn a b cocycle))
[K14 Group]

CAT 14 > (change-class E 'AB-group)
[K14 Abelian-Group]

CAT 15 > 
(setf KE1 (k-g-1 E))
[K15 Abelian-Simplicial-Group]

CAT 16 > (setf efhm2 (central-extension-efhm A B cocycle))
[K153 Homotopy-Equivalence K15 <= K139 => K135]

CAT 17 > (setf (slot-value kE1 'efhm) efhm2)
[K153 Homotopy-Equivalence K15 <= K139 => K135]

CAT 18 > (setf ka2 (k-g a 2))
[K154 Abelian-Simplicial-Group]

CAT 19 > (setf tau1 (univ-fbrt-tw A 2))
[K166 Fibration K154 -> K2]

CAT 20 > (setf tw
  (the fibration
    (build-smmr :sorc ka2 :trgt ke1 :degr -1
                :sintr 
                (sintr (cmps (cocycle-fibr-iso2 b a cocycle)
                             (cmps  (twop-incl (cocycle-fibration B A cocycle)) tau1)))
                :orgn `(fibration33 ,tau1))))
[K170 Fibration K154 -> K15]

CAT 21 > (setf comp (eilenberg-moore-bicomplex-ii tw))
[K218 Chain-Complex]

CAT 22 > (setf gnrt1 (first (basis comp 1)))
<TnPr <TnPr <<GBar>> ((GRCRPR 0 . 1))> <<Abar>>>

CAT 23 > (gnrt-embii-to-mmulticomplex gnrt1)
(:MMCGNRT (1 0 0) (<TnPr <<GBar>> ((GRCRPR 0 . 1))> <<Abar>>))

CAT 24 > 
|#

			
(DEFUN EMBII-MULTIDEGREE (gnrt)
  "--------------------------------------------------------------[function-doc]
Input: a generator of type EMBII.
Output: returns the multidegree associated to the generator.
------------------------------------------------------------------------------"
	(declare (type tnpr gnrt))
	(let* ((degr1 (degr1 gnrt))
        	(degr2 (degr2 gnrt))
        	(abar (gnrt2 gnrt))
        	(leng (with-abar (l) abar (the fixnum (length l)))))
		(declare (type fixnum degr1 degr2 leng)
			(type list l))
		(the list (list degr1 leng (- degr2 leng)))))

#|
CAT 24 > (embii-multidegree gnrt1)
(1 0 0)
|#


(DEFUN EMBII-MMCMGNRT (gnrt)
  "--------------------------------------------------------------[function-doc]
Input: a generator of type EMBII.
Output: the generator associated to it.
------------------------------------------------------------------------------"
	(declare (type tnpr gnrt))
	(let* ((tnpr1 (gnrt1 gnrt))
        	(abar (gnrt2 gnrt)))
        	(declare (type abar allp)
        		(type tnpr gnrt))
        	(the list (list tnpr1 abar))))

#|
CAT 25 > (embii-mmcmgnrt gnrt1)
(<TnPr <<GBar>> ((GRCRPR 0 . 1))> <<Abar>>)
|#


(DEFUN GNRT-MMULTICOMPLEX-TO-EMBII (mmcgnrt)
  "--------------------------------------------------------------[function-doc]
Input: a generator of type m-multicomplex that came from an EMBII complex.
Output: the same generator as an EMBII. 
------------------------------------------------------------------------------"
	(declare (type mmcgnrt mmcgnrt))
	(let* ((degr1 (first (mmcgnrt-mdegr mmcgnrt)))
		(leng (second (mmcgnrt-mdegr mmcgnrt)))
		(degr2 (+ (third (mmcgnrt-mdegr mmcgnrt)) leng))
		(tnpr1 (first (mmcgnrt-gnrt mmcgnrt)))
		(abar (second (mmcgnrt-gnrt mmcgnrt))))
		(declare (type fixnum degr2 degr1 leng)
			(type allp allp)
			(type tnpr tnpr2))
		(the tnpr (tnpr degr1 tnpr1 degr2 abar))))

#|
CAT 26 > (gnrt-mmulticomplex-to-embii (gnrt-embii-to-mmulticomplex gnrt1))
<TnPr <TnPr <<GBar>> ((GRCRPR 0 . 1))> <<Abar>>>
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS TO CHANGE A COMPLEX OF TYPE EMBII TO M-MULTICOMPLEX ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN MMCM-CMPR-OF-EMBII (chcm)
  "--------------------------------------------------------------[function-doc]
Input: recieves a chain complex with generators of the form embii.
Output: returns the same comparison function of that chain complex but such that it applies on generators of type mmcgnrt.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(let ((cmpr-embii (cmpr chcm)))
		(flet ((comp (mmc1 mmc2)
			(declare (type MmcGnrt mmc1 mmc2))
			(funcall cmpr-embii (gnrt-mmulticomplex-to-embii mmc1) (gnrt-mmulticomplex-to-embii mmc2))
			)) 
			(the cmprf #'comp))))

#|
(setf gnrt2 (second (basis comp 1)))
(funcall (cmpr comp) gnrt1 gnrt2)
(funcall (mmcm-cmpr-of-embii comp) (gnrt-embii-to-mmulticomplex gnrt1) (gnrt-embii-to-mmulticomplex gnrt2))

(dotimes (degr 3)
  (dotimes (i 2)
    (dotimes (j 2)
      (print  (funcall (cmpr comp) (nth (+ 1 i) (basis comp (+ 1 degr))) (nth (+ 1 j) (basis comp (+ 1 degr)))))
             (print (funcall (mmcm-cmpr-of-embii comp) (gnrt-embii-to-mmulticomplex (nth (+ 1 i) (basis comp (+ 1 degr)))) (gnrt-embii-to-mmulticomplex (nth (+ 1 j) (basis comp (+ 1 degr)))))))))


Output:

CAT 27 > (setf gnrt2 (second (basis comp 1)))
<TnPr <TnPr <<GBar>> ((GRCRPR 1 . 0))> <<Abar>>>

CAT 28 > (funcall (cmpr comp) gnrt1 gnrt2)
:LESS

CAT 29 > (funcall (mmcm-cmpr-of-embii comp) (gnrt-embii-to-mmulticomplex gnrt1) (gnrt-embii-to-mmulticomplex gnrt2))
:LESS

CAT 30 > (dotimes (degr 3)
  (dotimes (i 2)
    (dotimes (j 2)
      (print  (funcall (cmpr comp) (nth (+ 1 i) (basis comp (+ 1 degr))) (nth (+ 1 j) (basis comp (+ 1 degr)))))
             (print (funcall (mmcm-cmpr-of-embii comp) (gnrt-embii-to-mmulticomplex (nth (+ 1 i) (basis comp (+ 1 degr)))) (gnrt-embii-to-mmulticomplex (nth (+ 1 j) (basis comp (+ 1 degr)))))))))

:EQUAL 
:EQUAL 
:LESS 
:LESS 
:GREATER 
:GREATER 
:EQUAL 
:EQUAL 
:EQUAL 
:EQUAL 
:LESS 
:LESS 
:GREATER 
:GREATER 
:EQUAL 
:EQUAL 
:EQUAL 
:EQUAL 
:LESS 
:LESS 
:GREATER 
:GREATER 
:EQUAL 
:EQUAL 
NIL
|#			
			
(DEFUN MMCM-BASIS-OF-EMBII (chcm)
  "--------------------------------------------------------------[function-doc]
Input: chain complex of the form embii.
Output: basis of the same chain complex seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(let ((embiibasis (basis chcm)))
		(declare (type basis embiibasis))
		(when (eq embiibasis :locally-effective)
			(return-from mmcm-basis-of-embii :locally-effective))
		
		(flet ((bas (degr)
			(declare (type fixnum degr))
			(the list 
				(mapcar
					#'(lambda (gnrt)
						(gnrt-embii-to-mmulticomplex gnrt)
					)
				(funcall embiibasis degr)))))
		(the basis #'bas))))	

#|
(basis comp 1)
(funcall (mmcm-basis-of-embii comp) 1)
(basis comp 2)
(funcall (mmcm-basis-of-embii comp) 2)

Output:

CAT 31 > (basis comp 1)
(<TnPr <TnPr <<GBar>> ((GRCRPR 0 . 1))> <<Abar>>> <TnPr <TnPr <<GBar>> ((GRCRPR 1 . 0))> <<Abar>>> <TnPr <TnPr <<GBar>> ((GRCRPR 1 . 1))> <<Abar>>>)

CAT 32 > (funcall (mmcm-basis-of-embii comp) 1)
((:MMCGNRT (1 0 0) (<TnPr <<GBar>> (#)> <<Abar>>)) (:MMCGNRT (1 0 0) (<TnPr <<GBar>> (#)> <<Abar>>)) (:MMCGNRT (1 0 0) (<TnPr <<GBar>> (#)> <<Abar>>)))

CAT 33 > (basis comp 2)
(<TnPr <TnPr <<GBar>> NIL> <<Abar[2 ((GRCRPR 0 . 1))]>>> <TnPr <TnPr <<GBar>> NIL> <<Abar[2 ((GRCRPR 1 . 0))]>>> <TnPr <TnPr <<GBar>> NIL> <<Abar[2 ((GRCRPR 1 . 1))]>>> <TnPr <TnPr <<GBar>> ((GRCRPR 0 . 1) (GRCRPR 0 . 1))> <<Abar>>> <TnPr <TnPr <<GBar>> ((GRCRPR 0 . 1) (GRCRPR 1 . 0))> <<Abar>>> <TnPr <TnPr <<GBar>> ((GRCRPR 0 . 1) (GRCRPR 1 . 1))> <<Abar>>> <TnPr <TnPr <<GBar>> ((GRCRPR 1 . 0) (GRCRPR 0 . 1))> <<Abar>>> <TnPr <TnPr <<GBar>> ((GRCRPR 1 . 0) (GRCRPR 1 . 0))> <<Abar>>> <TnPr <TnPr <<GBar>> ((GRCRPR 1 . 0) (GRCRPR 1 . 1))> <<Abar>>> <TnPr <TnPr <<GBar>> ((GRCRPR 1 . 1) (GRCRPR 0 . 1))> <<Abar>>> ...)

CAT 34 > (funcall (mmcm-basis-of-embii comp) 2)
((:MMCGNRT (0 1 1) (<TnPr <<GBar>> NIL> <<Abar[2 (#)]>>)) (:MMCGNRT (0 1 1) (<TnPr <<GBar>> NIL> <<Abar[2 (#)]>>)) (:MMCGNRT (0 1 1) (<TnPr <<GBar>> NIL> <<Abar[2 (#)]>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) ...)

CAT 35 > 
|#
		
(DEFUN MMCM-DFFR-OF-EMBII (chcm)
  "--------------------------------------------------------------[function-doc]
Input: chain complex of type embii.
Output: its internal differential seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(flet ((dif (degr mmc)
		(declare (type fixnum degr)
			(type MmcGnrt mmc))
		(the cmbn 
			(make-cmbn
				:degr (- degr 1)
				:list (mapcar #'(lambda (term)
						(declare (type term term))
						(let ((cffc (cffc term))
							(gnrt (gnrt term)))
							(declare (type fixnum cffc)
								(type gnrt gnrt))
							(term cffc (gnrt-embii-to-mmulticomplex gnrt))))
				(cmbn-list (dffr chcm degr (gnrt-mmulticomplex-to-embii mmc)))))
		)))
	(the intr-mrph #'dif)))
	
#|
(dffr comp 2 (first (basis comp 2)))
(funcall (mmcm-dffr-of-embii comp) 2 (gnrt-embii-to-mmulticomplex (first (basis comp 2))))

Output:
CAT 37 > (dffr comp 2 (first (basis comp 2)))

----------------------------------------------------------------------{CMBN 1}
<1 * <TnPr <TnPr <<GBar>> ((GRCRPR 0 . 1))> <<Abar>>>>
------------------------------------------------------------------------------

CAT 38 > (funcall (mmcm-dffr-of-embii comp) 2 (gnrt-embii-to-mmulticomplex (first (basis comp 2))))

----------------------------------------------------------------------{CMBN 1}
<1 * (MMCGNRT (1 0 0) (<TnPr <<GBar>> (#)> <<Abar>>))>
------------------------------------------------------------------------------


|#	
	
	
(DEFUN EMBII-TO-MULTICOMPLEX (chcm)
  "--------------------------------------------------------------[function-doc]
Input: a chain complex of type embii
Output: the same chain complex seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(the chain-complex
		(let ((chcm
			(build-chcm
				:cmpr (mmcm-cmpr-of-embii chcm)
				:basis (mmcm-basis-of-embii chcm)
				:intr-dffr (mmcm-dffr-of-embii chcm)
				:strt :gnrt
				:bsgn (if (eq (bsgn chcm) nil)
						nil
						(gnrt-embii-to-mmulticomplex (bsgn chcm))
					) 
				:orgn (append (list 'embii-TO-MMC) (orgn chcm)))))
			(declare (type chain-complex chcm))
		chcm)))

#|
(setf mmcm-comp (embii-to-multicomplex comp))
(basis mmcm-comp 1)
(basis mmcm-comp 2)
(funcall (cmpr mmcm-comp) (first (basis mmcm-comp 2)) (second (basis mmcm-comp 2)))
(bsgn mmcm-comp)
(dffr mmcm-comp 3 (first (basis mmcm-comp 3)))

Output:

CAT 39 > (setf mmcm-comp (embii-to-multicomplex comp))
[K220 Chain-Complex]

CAT 40 > (basis mmcm-comp 1)
((:MMCGNRT (1 0 0) (<TnPr <<GBar>> (#)> <<Abar>>)) (:MMCGNRT (1 0 0) (<TnPr <<GBar>> (#)> <<Abar>>)) (:MMCGNRT (1 0 0) (<TnPr <<GBar>> (#)> <<Abar>>)))

CAT 41 > (basis mmcm-comp 2)
((:MMCGNRT (0 1 1) (<TnPr <<GBar>> NIL> <<Abar[2 (#)]>>)) (:MMCGNRT (0 1 1) (<TnPr <<GBar>> NIL> <<Abar[2 (#)]>>)) (:MMCGNRT (0 1 1) (<TnPr <<GBar>> NIL> <<Abar[2 (#)]>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) (:MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>)) ...)

CAT 42 > (funcall (cmpr mmcm-comp) (first (basis mmcm-comp 2)) (second (basis mmcm-comp 2)))
:LESS

CAT 43 > (bsgn mmcm-comp)
(:MMCGNRT (0 0 0) (<TnPr <<GBar>> NIL> <<Abar>>))

CAT 44 > (dffr mmcm-comp 3 (first (basis mmcm-comp 3)))

----------------------------------------------------------------------{CMBN 2}
<-2 * (MMCGNRT (0 1 1) (<TnPr <<GBar>> NIL> <<Abar[2 (#)]>>))>
<1 * (MMCGNRT (0 1 1) (<TnPr <<GBar>> NIL> <<Abar[2 (#)]>>))>
<1 * (MMCGNRT (2 0 0) (<TnPr <<GBar>> (# #)> <<Abar>>))>
------------------------------------------------------------------------------
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BAR SPECTRAL SYSTEM WITH MMULTICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFVAR MMCM-BAR-GFLIN)
  "--------------------------------------------------------------[variable-doc]
Definition of the filtration for the generators of the bar.
------------------------------------------------------------------------------"
(setf MMCM-BAR-GFLIN
	#'(lambda (degr gnrt)
		(declare (ignore degr)
			(type mmcgnrt gnrt))
		(let ((mdegr (MmcGnrt-Mdegr gnrt)))
			(the list (list (list (list (+ (second Mdegr) (third Mdegr)) (second Mdegr))))))))

(DEFUN CHECK-MMCM-BAR-GFLIN (fltrcm n1 &rest rest)
  "--------------------------------------------------------------[variable-doc]
Function that checks whether the previous filtration is well defined.

Input:a filtered chain complex and an integer or two.
Output: t if the differential is well defined as a morphism of generalized filtered chain complex, that is, it preserves the index of filtration.
------------------------------------------------------------------------------"
	(let ((gf (change-chcm-to-gflcc fltrcm (dz2) mmcm-bar-gflin '(mmcm-bar-gflin))))
		(let ((n2 (if (> (length rest) 0) (first rest) (1+ n1))))
			(dotimes (n0 (- n2 n1))
				(let ((n (+ n0 n1)))
					(dotimes (i (length (basis gf n)))
						(if (not (gfltrd-mrph-order-p (dffr gf) n (nth i (basis gf n)) '(0 0)))
							(return-from check-mmcm-bar-gflin nil)))))
			t)))



#|
(check-mmcm-bar-gflin mmcm-comp 0 5)

Output:

CAT 45 > (check-mmcm-bar-gflin mmcm-comp 0 5)
T			
|#

(DEFUN MMCM-BAR-SPECTRAL-SYSTEM (fibration)
  "--------------------------------------------------------------[function-doc]
Function that defines the bar spectral system associated to a fibration. We take the effective homology of the eilenberg-moore bicomplex of a fibration. Then we filter it with the bar filtration and we build the associated spectral system.

Input:a fibration given by a twisted cartesian product.
Output: the associated spectral system.
------------------------------------------------------------------------------"
	(let* ((k (eilenberg-moore-bicomplex-ii fibration))
	       (ecc (rbcc (efhm k)))
	       (k-mmcm (embii-to-multicomplex k))
	       (ecc-mmcm (embii-to-multicomplex ecc))
	       )
		(declare
		 (type chain-complex k)
		 (type chain-complex ecc)
		 (type chain-complex k-mmcm)
		 (type chain-complex ecc-mmcm))
		(progn
			(setf k-mmcm (change-chcm-to-gflcc k-mmcm (dz2) mmcm-bar-gflin '(mmcm-bar-gflin)))
			(setf ecc-mmcm (change-chcm-to-gflcc ecc-mmcm (dz2) mmcm-bar-gflin '(mmcm-bar-gflin)))
			(the spectral-system
				(build-spectral-system ecc-mmcm `(mmcm-bar-spectral-system ,fibration))))))
				
				
				
				
#|     
For the examples see the file "bar-mmulticomplex-examples.lisp".
|#
