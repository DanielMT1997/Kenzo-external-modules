;;M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES
;;M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES
;;M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES


(IN-PACKAGE #:cat)

(PROVIDE "bicomplexes")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPES FOR M-MULTICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN MmcGnrt-p (object)
  "--------------------------------------------------------------[function-doc]
This function defines the type MmcGnrt for generators of type m-multicomplexes. In other words, tests if any object has their specified form.

Input: An object
Output: Returns true if the object is an m-multicomplex, meaning a list composed by the identificator :MmcGnrt, another list with the multi-index of the generator, and the generator itself.

Observation: for bicomplexes were implemented using a two element cons instead of a list. The multi-index of a bicomplex would be a cons of two elements, for example <<(cons 5 6)>>, which would be structured as <<(5.6)>>. On the other hand, a 2-multicomplex would be defined in Kenzo as <<(list 5 6)>>, which programs something like <<(5.(6.))>>.
------------------------------------------------------------------------------"
  (declare (type any object))
  (when (consp (cadr object))
  	(dotimes (i (length (cadr object)))
  		(when (eql (typep (nth i (cadr object)) 'fixnum) nil)
  			(return-from MmcGnrt-p nil))))
  (the boolean
  	(and (consp object)
  		(eql :MmcGnrt (car object))
  		(consp (cdr object))
  		(consp (cadr object)))))
  		


		
(DEFTYPE MmcGnrt () '(satisfies MmcGnrt-p))
  "--------------------------------------------------------------[type-doc]
We define the type MmcGnrt as the elements that satisfy the previous function.
------------------------------------------------------------------------------"  

#|
(typep `(:MmcGnrt ,(list 5 6) u) 'MmcGnrt) ==>T
|#



(DEFTYPE MmcBASIS () '(or function (eql :locally-effective))) 
  "--------------------------------------------------------------[type-doc]
We define the type MmcBasis as functions that recieve a list of degrees (a multidegree) and return a list with the generators for the corresponding module. The type tester just checks whether we have or not a function or a locally effective basis.

(function (l) (list gnrt))

Observation: the corresponding function for bicomplexes recieves two degrees instead of a list of two degrees.
------------------------------------------------------------------------------"  



(DEFUN OrgnMmCmpl-p (object)
  "--------------------------------------------------------------[function-doc]
This function defines the type Mmcomplex for chain complexes that have mmulticomplex structure. In other words, tests if any object has their specified form.

Input: An object
Output: Returns true if the object has in the car of its origin the term <<MmCmpl>>.
------------------------------------------------------------------------------"
  (declare (type chain-complex object))
  (let* ((orgn (orgn object)))
    (the boolean
      (and (consp orgn)
           (eql (car orgn) 'MmCmpl)))))

           
            
(DEFTYPE Mmcomplex () '(satisfies OrgnMmCmpl-p))
  "--------------------------------------------------------------[type-doc]
We define the type Mmcomplex as the elements that satisfy the previous function.
------------------------------------------------------------------------------" 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS FOR THE REPRESENTATION OF M-MULTICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(DEFUN build-MmcGnrt (l gnrt)
  "--------------------------------------------------------------[function-doc]
Input: A list of m degrees and a generator.
Output: An object of type MmcGnrt built with those.
------------------------------------------------------------------------------"
	(declare (type any gnrt)
		(type list l))
	(list :MmcGnrt l gnrt))

#|
 (build-MmcGnrt (list 2 3) 'c)==>(:MMCGNRT (2 3) C)
(typep  (build-MmcGnrt (list 2 2) 'd) 'MmcGnrt)==>T
|#
 


(DEFUN MmcGnrt-Degr (MmcGnrt i)
  "--------------------------------------------------------------[function-doc]
Input: A generator of type MmcGnrt and an integer.
Output: The ith degree of the generator.
------------------------------------------------------------------------------"
  (declare (type MmcGnrt MmcGnrt)
  	(type fixnum i))
  (nth i (cadr MmcGnrt)))

  

(DEFUN MmcGnrt-Mdegr (MmcGnrt)
  "--------------------------------------------------------------[function-doc]
Input: A generator of type MmcGnrt.
Output: The multidegree of generator.
------------------------------------------------------------------------------"
	(declare (type MmcGnrt MmcGnrt))
	(second MmcGnrt))



(DEFUN MmcGnrt-Gnrt (MmcGnrt)
  "--------------------------------------------------------------[function-doc]
Input: A generator of type MmcGnrt.
Output: El elemento que tiene dentro.
------------------------------------------------------------------------------"
  (declare (type MmcGnrt MmcGnrt))
  (caddr MmcGnrt))



(DEFUN MmcGnrt-Dmns (MmcGnrt)
  "--------------------------------------------------------------[function-doc]
Input: A generator of type MmcGnrt.
Output: The dimension of the generator.
------------------------------------------------------------------------------"
	(declare (type MmcGnrt MmcGnrt))
	(length (second MmcGnrt)))

#|
(build-MmcGnrt (list 5 6 8) 'd)
(MmcGnrt-Degr (build-MmcGnrt (list 5 6 8) 'd) 0)
(MmcGnrt-Degr (build-MmcGnrt (list 5 6 8) 'd) 1)
(MmcGnrt-Degr (build-MmcGnrt (list 5 6 8) 'd) 2)
(MmcGnrt-Gnrt (build-MmcGnrt (list 6 4) '(4 6)))
(MmcGnrt-Gnrt (build-MmcGnrt (list 2 8) 'u))
(MmcGnrt-Gnrt (build-MmcGnrt (list 1 2 3 4 5 5 6) 'u))
(MmcGnrt-Mdegr (build-MmcGnrt (list 1 2 3 4 5 5 6) 'u))

(build-MmcGnrt (list 5 6 8) 'd)==>(:MMCGNRT (5 6 8) D)
(MmcGnrt-Degr (build-MmcGnrt (list 5 6 8) 'd) 0)==>5
(MmcGnrt-Degr (build-MmcGnrt (list 5 6 8) 'd) 1)==>6
(MmcGnrt-Degr (build-MmcGnrt (list 5 6 8) 'd) 2)==>8
(MmcGnrt-Gnrt (build-MmcGnrt (list 6 4) '(4 6)))==>(4 6)
(MmcGnrt-Gnrt (build-MmcGnrt (list 2 8) 'u))==>U
(MmcGnrt-Gnrt (build-MmcGnrt (list 1 2 3 4 5 5 6) 'u))==>U
(MmcGnrt-Mdegr (build-MmcGnrt (list 1 2 3 4 5 5 6) 'u))==>(1 2 3 4 5 5 6)
|# 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS FOR CHAIN COMPLEXES OF M-MULTICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MMC-CMPR (cmpr)
  "--------------------------------------------------------------[function-doc]
Input: A comparison function for the elements of the generators.
Output: A comparison function for the generators of type Mmcgnrt.
------------------------------------------------------------------------------"
	(declare (type cmprf cmpr))
	(flet ((comp (mmc1 mmc2)
		(declare (type MmcGnrt mmc1 mmc2))
		(let ((l1 (length (MmcGnrt-Mdegr mmc1)));;(l1 (MmcGnrt-Dmns mmc1))
			(l2 (length (MmcGnrt-Mdegr mmc2)))
			(gnrt1 (MmcGnrt-Gnrt mmc1))
			(gnrt2 (MmcGnrt-Gnrt mmc2)))
			(declare (type any gnrt1 gnrt2))
			(if (eql l1 l2)
				(lexico
					(maplexico
						#'(lambda (d1 d2)
							(declare (type fixnum d1 d2))
							(f-cmpr d1 d2))
						(MmcGnrt-Mdegr mmc1) (MmcGnrt-Mdegr mmc2))
					(funcall cmpr gnrt1 gnrt2)
				)
				(return-from comp nil)
			)
		)))
	(the cmprf #'comp)))

#|
(setf cmpr (MMC-CMPR #'s-cmpr)) 
(funcall cmpr (build-MmcGnrt (list 5 6) 'a) (build-MmcGnrt (list 7 8) 'b))
(funcall cmpr (build-MmcGnrt (list 8 6) 'a) (build-MmcGnrt (list 7 8) 'b))
(funcall cmpr (build-MmcGnrt (list 8 5) 'c) (build-MmcGnrt (list 8 6) 'b))
(funcall cmpr (build-MmcGnrt (list 8 5) 'c) (build-MmcGnrt (list 8 5) 'b))
(funcall cmpr (build-MmcGnrt (list 8 5) 'a) (build-MmcGnrt (list 8 5) 'b))
(funcall cmpr (build-MmcGnrt (list 8 7) 'c) (build-MmcGnrt (list 8 6) 'b))
(funcall cmpr (build-MmcGnrt (list 8 7 6) 'c) (build-MmcGnrt (list 8 6) 'b))
(funcall cmpr (build-MmcGnrt (list 8 5) 'a) (build-MmcGnrt (list 8 5) 'a))
(funcall cmpr (build-MmcGnrt (list 8 7 6) 'c) (build-MmcGnrt (list 8 6 7) 'b))
(funcall cmpr (build-MmcGnrt (list 9 8 4 3) 'u) (build-MmcGnrt (list 9 8 4 3) 'w))

(setf cmpr (MMC-CMPR #'s-cmpr)) 
(funcall cmpr (build-MmcGnrt (list 5 6) 'a) (build-MmcGnrt (list 7 8) 'b))
:LESS
(funcall cmpr (build-MmcGnrt (list 8 6) 'a) (build-MmcGnrt (list 7 8) 'b))
:GREATER
(funcall cmpr (build-MmcGnrt (list 8 5) 'c) (build-MmcGnrt (list 8 6) 'b))
:LESS
(funcall cmpr (build-MmcGnrt (list 8 5) 'c) (build-MmcGnrt (list 8 5) 'b))
:GREATER
(funcall cmpr (build-MmcGnrt (list 8 5) 'a) (build-MmcGnrt (list 8 5) 'b))
:LESS
(funcall cmpr (build-MmcGnrt (list 8 7) 'c) (build-MmcGnrt (list 8 6) 'b))
:GREATER
(funcall cmpr (build-MmcGnrt (list 8 7 6) 'c) (build-MmcGnrt (list 8 6) 'b))
NIL
(funcall cmpr (build-MmcGnrt (list 8 5) 'a) (build-MmcGnrt (list 8 5) 'a))
:EQUAL
(funcall cmpr (build-MmcGnrt (list 8 7 6) 'c) (build-MmcGnrt (list 8 6 7) 'b))
:GREATER
(funcall cmpr (build-MmcGnrt (list 9 8 4 3) 'u) (build-MmcGnrt (list 9 8 4 3) 'w))
:LESS
|# 




(DEFUN MMC-BASIS (mmcbasis m)
  "--------------------------------------------------------------[function-doc]
MMC-Basis takes a basis of a multicomplex (i. e. a function that given a multiindex of m components returns the basis of the module that is indexed likewise) and returns the basis function of the totalization of the m-multicomplex, that is, a function that given a degree returns a list of all generators of that degree. degr is the degree on the totalization. Therefore we must take all the possible sums that give us the corresponding degr.

Input: A function of type mmcbasis and the dimension of the elements we are working with.
Output: The basis function for the totalization of the m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type MmcBasis mmcbasis)
		(type fixnum m))
	(when (eq mmcbasis :locally-effective)
		(return-from mmc-basis :locally-effective))
	(flet ((bas (degr)
		(declare (type fixnum degr))
		(let ((mindexlist (group-list (listmaker degr m nil) m)))
			(declare (type list mindexlist))
			(the list
				(mapcan
					#'(lambda (mindex)
						(declare (type list mindex))
						(let ((basis (funcall mmcbasis mindex)))
							(declare (type list basis))
							(the list 
								(mapcar 
									#'(lambda (gnrt)
										(declare (type gnrt gnrt))
										(the MmcGnrt
											(build-MmcGnrt mindex gnrt)))
								basis))))
				mindexlist)))))
		(the basis #'bas)
))
				
					


(DEFUN LISTMAKER (n m l)
  "--------------------------------------------------------------[function-doc]
Auxiliary function that, given an integer n, an integer m and a list l, returns a list composed of l followed by a list of m positive numbers that sum n.

Input: Two integers n and m, a list l.
Output: A list of lists, composed of l followed by m numbers that sum n.
------------------------------------------------------------------------------"
	(declare (type fixnum n m)
		(type any l))
	(when (minusp n)
		(return-from listmaker nil))
	(if (eql m 1)
		(return-from listmaker (append l (list n)))
		(mapcan
			#'(lambda (i)
				(listmaker (- n i) (- m 1) (append l (list i)))
			)
		(<a-b> 0 n))
	))
		
(DEFUN LISTMAKER-2 (n m l b)
  "--------------------------------------------------------------[function-doc]
Auxiliary function that, given an integer n, an integer m and a list l, returns a list composed of l followed by a list of lists of m numbers that sum n.

Input: Two integers n and m, a list l. A positive bound b so that the result is finite.
Output: A list of lists, composed of l followed by m numbers that sum n.
------------------------------------------------------------------------------"
	(declare (type fixnum n m b)
		(type any l))
	(if (eql m 1)
		(return-from listmaker-2 (append l (list n)))
		(mapcan
			#'(lambda (i)
				(listmaker-2 (- n i) (- m 1) (append l (list i)) b)
			)
		(if (> n 0)
			(<a-b> (- n b) b)
			(<a-b> (- 0 b) b))
		)
	))		
		
(defun group-list (list m)
  "--------------------------------------------------------------[function-doc]
Auxiliary function that collects the elements of a list in a list of lists that have length m.

Input: a list list and an integer m.
Output: that same list with its elements collected m by m.
------------------------------------------------------------------------------"
  (loop repeat (/ (length list) m) collect
       (loop repeat m collect (pop list))))
       

#|
(defun bas (mindex)
	(let ((degr1 (first mindex))
		(degr2 (second mindex)))
		(if (and (= degr1 0) (= degr2 1)) (return-from bas '(a)))
		(if (and (= degr1 1) (= degr2 0)) (return-from bas '(b)))
		(if (and (= degr1 1) (= degr2 1)) (return-from bas '(c)))
		(if (and (= degr1 2) (= degr2 0))  (return-from bas '(d)))
  (return-from bas nil)))
(setf basis (MMC-BASIS #'bas 2))
(dotimes (i 5)
  (print (funcall basis i)))==> 
NIL 
((:MMCGNRT (0 1) A) (:MMCGNRT (1 0) B)) 
((:MMCGNRT (1 1) C) (:MMCGNRT (2 0) D)) 
NIL 
NIL 
NIL


 (defun bas (mindex)
	(let ((degr1 (first mindex))
		(degr2 (second mindex)) (degr3 (third mindex)))
		(if (and (= degr1 0) (= degr2 1) (= degr3 1)) (return-from bas '(a)))
		(if (and (= degr1 1) (= degr2 0) (= degr3 3)) (return-from bas '(b)))
		(if (and (= degr1 1) (= degr2 1) (= degr3 1)) (return-from bas '(c)))
		(if (and (= degr1 2) (= degr2 0) (= degr3 2))  (return-from bas '(d)))
  (return-from bas nil)))
(setf basis (MMC-BASIS #'bas 3))
(dotimes (i 5)
  (print (funcall basis i)))==>

NIL 
NIL 
((:MMCGNRT (0 1 1) A)) 
((:MMCGNRT (1 1 1) C)) 
((:MMCGNRT (1 0 3) B) (:MMCGNRT (2 0 2) D)) 
NIL
|#



(DEFUN MMC-INTR-DFFR (l)
  "--------------------------------------------------------------[function-doc]
Input: A list of pairs of differentials and their multidegree (a list of integers of length m).
Output: An intr-mrph that, given a generator and a degree, returns a combination that results of appluing the differentials.
------------------------------------------------------------------------------"
	(declare (type list l))
	(flet ((dif (degr mmc)
		(declare (fixnum degr)
			(type MmcGnrt mmc))
		(the cmbn
			(let* ((gnrt (MmcGnrt-Gnrt mmc)))
				(make-cmbn
					:degr (1- degr)
					:list (mapcan #'(lambda (pair)
							(let ((dffr (first pair))
								(mindex (second pair)))
								(declare (type function dffr)
									(type list mindex))
								(mapcar #'(lambda (term1)
									(declare (type term term1))
									(let* ((cffc1 (cffc term1))
										(gnrt1 (gnrt term1)))
										(declare (type fixnum cffc1)
											(type gnrt gnrt1))
										(term cffc1 (build-MmcGnrt (mapcar #'(lambda (i1 i2) (+ i1 i2)) (MmcGnrt-Mdegr mmc) mindex) gnrt1))))
								(funcall dffr (MmcGnrt-Mdegr mmc) gnrt)))
						)l)
				)))))
(the intr-mrph #'dif)))

#|
(defun dif1 (l gnrt)
	(let ((degr1 (first l))
		(degr2 (second l)))
  (if (and (= degr1 1) (= degr2 1) (eql gnrt 'c)) (return-from dif1 (list (cons 2 'a))))
  (if (and (= degr1 2) (= degr2 0) (eql gnrt 'd)) (return-from dif1 (list (cons 2 'b))))
  (return-from dif1 nil)))
(defun dif2 (l gnrt)
	(let ((degr1 (first l))
		(degr2 (second l)))
  (if (and (= degr1 1) (= degr2 1) (eql gnrt 'c)) (return-from dif2 (list (cons 1 'b))))
  (return-from dif2 nil)))
(setf dif (MMC-INTR-DFFR (list (list #'dif1 (list -1 0)) (list #'dif2 (list 0 -1)))))
(funcall dif 2 (build-MmcGnrt (list 1 1) 'c))==> 
----------------------------------------------------------------------{CMBN 1}
<2 * (MMCGNRT (0 1) A)>
<1 * (MMCGNRT (1 0) B)>
------------------------------------------------------------------------------
|#




(DEFUN BUILD-MMCM (&key mmcbasis m l cmpr orgn)
  "--------------------------------------------------------------[function-doc]
Input: mmcbasis a basis function for the multidegree. m the dimension of the generators. l a list of differentials and their multidegrees. Orgn an origin.
Output: A chain complex built with the differential, basis and comparision functions of the previous programs.
------------------------------------------------------------------------------"
	(declare (type MmcBasis mmcbasis)
		(type list l)
		(type cmprf cmpr)
		(type fixnum m)
		(type list orgn))
	(the chain-complex
		(let ((chcm
			(build-chcm
				:cmpr (MMC-CMPR cmpr)
				:basis (MMC-BASIS mmcbasis m)
				:intr-dffr (MMC-INTR-DFFR l)
				:strt :gnrt
				:bsgn nil
				:orgn orgn)))
			(declare (type chain-complex chcm))
		chcm)))

#|(These are computed with the first ones of the previous examples: the ones with dimension 2.
(setf mmc (Build-Mmcm :mmcbasis #'bas :m 2 :l (list (list #'dif1 (list -1 0)) (list #'dif2 (list 0 -1))) :cmpr #'s-cmpr 
                     :orgn '(MMC-test)))
[K1 Chain-Complex]

(basis mmc)
#<interpreted function (HARLEQUIN-COMMON-LISP:SUBFUNCTION (FLET BAS) :UNKNOWN) 423000888C>

(basis mmc 1)
((:MMCGNRT (0 1) A) (:MMCGNRT (1 0) B))

(dotimes (i 5)
  (print (basis mmc i)))

NIL 
((:MMCGNRT (0 1) A) (:MMCGNRT (1 0) B)) 
((:MMCGNRT (1 1) C) (:MMCGNRT (2 0) D)) 
NIL 
NIL 
NIL


(cmpr mmc (build-MmcGnrt (list 0 1) 'a) (build-MmcGnrt (list 1 1) 'c))
:LESS



(? mmc (? mmc 2 (build-MmcGnrt (list 1 1) 'c))) 

----------------------------------------------------------------------{CMBN 0}
------------------------------------------------------------------------------

(? mmc 2 (build-MmcGnrt (list 2 0) 'd))

----------------------------------------------------------------------{CMBN 1}
<2 * (MMCGNRT (1 0) B)>
------------------------------------------------------------------------------
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERALIZED FILTRATIONS AND DOWNSETS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	


(DEFVAR MMCFlin)
  "--------------------------------------------------------------[variable-doc]
Definition of the filtration for the generators of type MmcGnrt. This is the same filtration that we use when we define the filtration for the Serre spectral sequence over tensor products with m factors.
------------------------------------------------------------------------------"
(setf MMCflin 
	#'(lambda (degr gnrt)
	(declare (type MmcGnrt gnrt))
	(let* ((Mdegr (MMcGnrt-Mdegr gnrt))
		(m (length Mdegr)))
		(if (= m 3)
			(the list 
				(let ((l (list (t-downset-list (cdr (reverse Mdegr)))))) 
				l))
			(if (= m 4)
			(the list 
				(let ((l  (list (t3-downset-list  (cdr (reverse Mdegr)))))) 
		l)))))))
		
#|
(funcall MMCflin 5 (build-Mmcgnrt (list 2 3) 'a))
(funcall MMCflin 5 (build-Mmcgnrt (list 3 4 5) 'c))

(funcall MMCflin 5 (build-Mmcgnrt (list 2 3) 'a))
(((2)))

(funcall MMCflin 5 (build-Mmcgnrt (list 3 4 5) 'c))
(((3 4)))
|# 

	
(DEFUN Zm (n)
  "--------------------------------------------------------------[function-doc]
Input: The dimension n
Output: The poset Z^n. The poset comparison function takes two lists and compares them like this: if all the elements are all greater, equal or less, returns greater, equal or less. Otherwise returns undefined.
------------------------------------------------------------------------------"
	(declare (type fixnum n))
	(build-poset :pocmpr
		#'(lambda (l1 l2)
			(declare (type list l1 l2))
				(let ((m (length l1))
					(m1 (length l2))
					(Ma NIL)
					(me nil))
					(if (eq m m1)
						(progn
							(do(
								(x (first l1) (first l1))
								(y (first l2) (first l2))
								(l1 (cdr l1) (cdr l1))
								(l2 (cdr l2) (cdr l2)))
								((eq x nil))
								(if (< x y);;(\= x y)
									(setq me 'T)
									(if (> x y)
										(setq Ma 'T)
									)
								)
							)
							(if Ma
								(if me
									:undefined
									:greater
								)
								(if me
									:less
									:equal
								)
							)
						)
						(princ "Las tuplas tienen diferente longitud.")
					)
				)
			)
			:orgn `('(zm) ,n)
	)
)		

					
               

		
(DEFUN DZm (n)
  "--------------------------------------------------------------[function-doc]
Input: The dimension n.
Output: The poset of downsets of Z^n.
------------------------------------------------------------------------------"
	(declare (type fixnum n))
  (downsets (zm n)))



(DEFUN CHANGE-MMCM-TO-GFLCC (mmcm n)
  "--------------------------------------------------------------[function-doc]
Input: An n-multicomplex.
Output: A generalized filtered multicomplex using the filtration previously defined.
------------------------------------------------------------------------------"
	(declare (type Mmcomplex mmcm)
		(type fixnum n))
	(the GENERALIZED-FILTERED-CHAIN-COMPLEX
		(change-chcm-to-gflcc mmcm (Dzm n) mmcflin 'mmcflin)))			

#|
(defun bas (mindex)
	(let ((degr1 (first mindex))
		(degr2 (second mindex)))
		(if (and (= degr1 0) (= degr2 1)) (return-from bas '(a)))
		(if (and (= degr1 1) (= degr2 0)) (return-from bas '(b)))
		(if (and (= degr1 1) (= degr2 1)) (return-from bas '(c)))
		(if (and (= degr1 2) (= degr2 0)) (return-from bas '(d)))
		(return-from bas nil)))

(defun dif1 (l gnrt)
	(let ((degr1 (first l))
		(degr2 (second l)))
		(if (and (= degr1 1) (= degr2 1) (eql gnrt 'c)) (return-from dif1 (list (cons 2 'a))))
		(if (and (= degr1 2) (= degr2 0) (eql gnrt 'd)) (return-from dif1 (list (cons 2 'b))))
  		(return-from dif1 nil)))
(defun dif2 (l gnrt)
	(let ((degr1 (first l))
		(degr2 (second l)))
		(if (and (= degr1 1) (= degr2 1) (eql gnrt 'c)) (return-from dif2 (list (cons 1 'b))))
		(return-from dif2 nil)))

(setf mmc (Build-Mmcm :mmcbasis #'bas :m 2 :l (list (list #'dif1 (list -1 0)) (list #'dif2 (list 0 -1))) :cmpr #'s-cmpr :orgn '(MMC-test)))

(change-mmcm-to-gflcc mmc)
(setf ss1 (change-mmcm-to-gflcc mmc));;mmc sigue siendo un complejo normal.
(gen-flin ss1 1 (build-Mmcgnrt (list 0 1) 'a))
((0))
(gen-fltrd-basis ss1 1 '((0)))
((:MMCGNRT (0 1) A))


(mgrd-basis ss1 l) ;;FALTA (tendríamos que darle la lista con el multiindice)
(ordered-basis bc 2);;No encuentro el análogo
(gen-flin mmc (cmbn 2 4 (build-Mmcgnrt (list 1 1) 'c)));;No sé si está definido para combinaciones.
(flin mmc (cmbn 2 4 (build-Mmcgnrt (list 1 1) 'c) 2 (build-Mmcgnrt (list 2 0) 'd)))
|#			


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS FOR THE PERTURBATIONS AND THE DIFFERENTIALS OF M-MULTICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN ADD-LIST (mmcm arrows)
  "--------------------------------------------------------------[function-doc]
Unused.

Input: An m-multicomplex together with a list of the differentials that we want to add to the multicomplex.
Output: A new perturbed multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex mmcm)
		(type list arrows))
	(add (intr-dffr mmcm) (MMC-INTR-DFFR arrows)))

	
(DEFUN ADD-PERTURBATION (h delta)
  "--------------------------------------------------------------[function-doc]
Unused.

Input: A homotopy equivalence and a perturbation.
Output: A new homotopy equivalence perturbed by the latter.
------------------------------------------------------------------------------"
	(declare (type list delta)
		(type homotopy-equivalence h))
	(let* ((pert (mmc-intr-dffr delta)))
		(the homotopy-equivalence
			(let ((hmeq (add h pert)))
			hmeq))))
			
(DEFUN TNPR-DIFF-ARROW (chcm mindex)
  "--------------------------------------------------------------[function-doc]
Input: A chain complex of type tnsr-prdc and a multi-index.
Output: The arrow of the differential that has that multi-index, seeing the chain complex as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm)
		(type list md))
	(flet ((dif (mdegr gnrt)
		(declare (list mdegr)
			(type gnrt gnrt))
		(let ((l +empty-list+)
			(degr (apply '+ mdegr)))
			(declare (list l))
			(progn 
				(mapcar #'(lambda (term)
					(let ((cffc (cffc term))
						(dgnrt (gnrt term)))
						(if (= (tnpr-multidegree dgnrt +empty-list+) (+ mindex mdegr))
							(append l term)
						)))
				(cmbn-list (dffr chcm (apply '+ mdegr) gnrt)))
				(the cmbn
					(make-cmbn
						:degr (1- degr)
						:list  l)
			))))
	(list #'dif mindex))))


(DEFUN TNPR-DIFF-LIST (chcm m b)
  "--------------------------------------------------------------[function-doc]
Input: A chain complex of type tnsr-prdc, the dimension of the tnpr m and a bound for the indexes b.
Output: Returns a list of differentials together with their multi-indexes. The sum of the differentials is the original differential of the chain complex, as in a totalization. Thus, this is a decomposition of the original differential in different arrows, indexed by their direction on the multidegree.
------------------------------------------------------------------------------"
	(let ((mindexlist (group-list (listmaker-2 -1 m nil b) m))
		(l +empty-list+))
		(declare (type list mindexlist l))
		(progn
			(mapcar #'(lambda (mindex)
				(declare (type list mindex))
				(append l (tnpr-diff-arrow chcm mindex))
				)
			mindexlist)
			(return-from tnpr-diff-list l))))
;;Duda: aquí me hace falta poner progn y return o me devuelve diréctamente el resultado del mapcar, sin hacer nada? Porque claro, no es que mapcar devuelva nada sino que estoy modificando l en cada iteración.		
					
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TENSOR PRODUCTS AS M-MULTICOMPLEXES AS TENSOR PRODUCTS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
(DEFUN GNRT-TNPR-TO-MMULTICOMPLEX (tnpr)
  "--------------------------------------------------------------[function-doc]
Input: a generator of type tnpr with m factors.
Output: the same generator seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type tnpr tnpr))
		(the MmcGnrt (build-MmcGnrt (tnpr-multidegree tnpr nil) (tnpr-mmcmgnrt tnpr nil))))


#|		
(setf tensor (tnpr 1 'a 2 'b))
(setf tensor (tnpr 1 tensor 2 'c))
(tnpr-mmcmgnrt tensor nil)
(tnpr-multidegree tensor nil)
(gnrt-tnpr-to-mmulticomplex tensor)

;;(setf tensor (tnpr 1 'a 2 'b))
;;(setf tensor (tnpr 1 tensor 2 'c))
;;<TnPr <TnPr A B> C>
;;(tnpr-mmcmgnrt tensor nil)
;;(A B C)
;;(tnpr-multidegree tensor nil)
;;(1 2 2)
;; (gnrt-tnpr-to-mmulticomplex tensor)
;;(:MMCGNRT (1 2 2) (A B C))

|#

			
(DEFUN TNPR-MULTIDEGREE (tnpr l)
  "--------------------------------------------------------------[function-doc]
Input: a generator of type tnpr and a list.
Output: returns the multidegree associated to the tensor product together with the list (if the list is empty, is just the multidegree).
------------------------------------------------------------------------------"
	(declare (type tnpr tnpr)
		(type list l))
	(let ((degr1 (degr1 tnpr))
		(degr2 (degr2 tnpr))
		(gnrt1 (gnrt1 tnpr))
		(gnrt2 (gnrt2 tnpr)))
		(declare (type fixnum degr1 degr2))
		(progn
			(if (tnpr-p gnrt1)
				(progn
					(setq l (append l (TNPR-MULTIDEGREE gnrt1 l)))
					(if (tnpr-p gnrt2)
						(setq l (append l (TNPR-MULTIDEGREE gnrt2 l)))
						(setq l (append l (list degr2)))
					)
				)
				(progn
					(setq l (append l (list degr1)))
					(if (tnpr-p gnrt2)
						(setq l (append l (TNPR-MULTIDEGREE gnrt2 l)))
						(setq l (append l (list degr2)))
					)
				)
			)
			(the list l)
		)
	)
)


(DEFUN TNPR-MMCMGNRT (tnpr l)
  "--------------------------------------------------------------[function-doc]
Input: a generator of type tnpr and a list.
Output: the generator associated to the tensor product, plus the list (if the list is empty, is just the generator).
------------------------------------------------------------------------------"
	(declare (type tnpr tnpr)
		(type list l))
	(let ((gnrt1 (gnrt1 tnpr))
		(gnrt2 (gnrt2 tnpr)))
		(progn
			(if (tnpr-p gnrt1)
				(progn
					(setq l (append l (TNPR-MMCMGNRT gnrt1 l)))
					(if (tnpr-p gnrt2)
						(setq l (append l (TNPR-MMCMGNRT gnrt2 l)))
						(setq l (append l (list gnrt2)))
					)
				)
				(progn
					(if (eql l nil)
						(setq l (list gnrt1))
						(setq l (append l (list gnrt1)))
					)
					(if (tnpr-p gnrt2)
						(setq l (append l (TNPR-MMCMGNRT gnrt2 l)))
						(setq l (append l (list gnrt2)))
					)
				)
			)
			(the list l)
		)
	)
)

#|
(setf tensor (tnpr 1 'a 2 'b))
(setf tensor (tnpr 1 tensor 2 'c))
(tnpr-mmcmgnrt tensor nil)

;;(setf tensor (tnpr 1 'a 2 'b))
;;<TnPr A B>
;;(setf tensor (tnpr 1 tensor 2 'c))
;;<TnPr <TnPr A B> C>
;;(tnpr-mmcmgnrt tensor nil)
;;(A B C)
|#


(DEFUN GNRT-MMULTICOMPLEX-TO-TNPR (mmcgnrt)
  "--------------------------------------------------------------[function-doc]
Input: a generator of type m-multicomplex that came from a tensor product.
Output: the same generator as a tensor product. By convinience, the generators are built in a way that the products are structured as ((((((,),),),),),) and so on. The case of towers of fibrations is programmed like this.
------------------------------------------------------------------------------"
	(declare (type mmcgnrt mmcgnrt))
	(let* ((mdgri (rest (rest (mmcgnrt-mdegr mmcgnrt))))
		(gnrti (rest (rest (mmcgnrt-gnrt mmcgnrt))))
		(gnrt1 (first (mmcgnrt-gnrt mmcgnrt)))
		(degr1 (first (mmcgnrt-mdegr mmcgnrt)))
		(gnrt2 (second (mmcgnrt-gnrt mmcgnrt)))
		(degr2 (second (mmcgnrt-mdegr mmcgnrt)))
		(tensori (tnpr degr1 gnrt1 degr2 gnrt2)))
		(declare (type list mdgr gnrt l)
			(type fixnum degr1)
			(type gnrt gnrt1))
		(if (eq (length mdgri) 0)
			(the tnpr tensori)
			(the tnpr (do 
				(
					
					(d1 degr1 (+ d1 d2))
					(d2 degr2 (first mdgr))
					(g1 gnrt1 tensor)
					(g2 gnrt2 (first gnrt))
					
					(mdgr mdgri (rest mdgr))
					(gnrt gnrti (rest gnrt))
					(tensor tensori tensor)
				)
				(
					(eq d2 nil)
					tensor
				)
				(setf tensor (tnpr d1 g1 d2 g2))
			)))))



#|
(setf tensor (tnpr 1 'a 2 'b))
(setf tensor (tnpr 1 tensor 3 'c))
(setf tensor (tnpr 5 tensor 4 'd))
(setf mmc (gnrt-tnpr-to-mmulticomplex tensor))
(gnrt-mmulticomplex-to-tnpr mmc)

;;(setf tensor (tnpr 1 'a 2 'b))
;;(setf tensor (tnpr 1 tensor 3 'c))
;;<TnPr <TnPr A B> C>
;;(setf tensor (tnpr 5 tensor 4 'd));;El 5 ese termina no sirviendo para nada, pero bueno.
;;<TnPr <TnPr <TnPr A B> C> D>
;;(setf mmc (gnrt-tnpr-to-mmulticomplex tensor))
;;(:MMCGNRT (1 2 3 4) (A B C D))
;;(gnrt-mmulticomplex-to-tnpr mmc)
;;<TnPr <TnPr <TnPr A B> C> D>

|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS TO CHANGE A COMPLEX OF TYPE TNPR TO M-MULTICOMPLEX ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			

(DEFUN MMCM-CMPR-OF-TNPR-1 (chcm)
  "--------------------------------------------------------------[function-doc]
Input: recieves a chain complex with generators of type tnpr.
Output: returns the same comparison function of that chain complex but such that it applies on generators of type mmcgnrt.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(let ((cmpr-tnpr (cmpr chcm)))
		(flet ((comp (mmc1 mmc2)
			(declare (type MmcGnrt mmc1 mmc2))
			(funcall cmpr-tnpr (gnrt-mmulticomplex-to-tnpr mmc1) (gnrt-mmulticomplex-to-tnpr mmc2))
			)) 
			(the cmprf #'comp))))
			
#|
(defun cdelta (dmns)
	(build-chcm
	:cmpr #'l-cmpr
	:basis #'(lambda (n)
	(mapcar #'dlop-int-ext (funcall (delta-n-basis dmns) n)))
		:bsgn '(0)
		:intr-dffr #'(lambda (degr gmsm)
			(make-cmbn
				:degr (1- degr)
				:list (do ((rslt +empty-list+
					(cons (cons sign
						(append
						(subseq gmsm 0 nark)
						(subseq gmsm (1+ nark))))
						rslt))
						(sign 1 (- sign))
					(nark 0 (1+ nark)))
					((> nark degr) rslt))))
		:strt :gnrt
		:orgn ‘(Effective version of C_* delta ,dmns)))

(setf triangle (cdelta 2))
(basis triangle 1)
(setf tpr-triangles (tnsr-prdc triangle triangle))
(setf base (basis tpr-triangles))
(first (funcall base 1))
(second (funcall base 1))
(funcall (cmpr tpr-triangles) (first (funcall base 1)) (second (funcall base 1)))
(funcall (mmcm-cmpr-of-tnpr-1 tpr-triangles) (gnrt-tnpr-to-mmulticomplex (first (funcall base 1)))  (gnrt-tnpr-to-mmulticomplex (second (funcall base 1)))) 
(funcall (mmcm-cmpr-of-tnpr-1 tpr-triangles) (gnrt-tnpr-to-mmulticomplex (first (funcall base 1)))  (gnrt-tnpr-to-mmulticomplex (first (funcall base 1))))


Output:

(defun cdelta (dmns)
	(build-chcm
	:cmpr #'l-cmpr
	:basis #'(lambda (n)
	(mapcar #'dlop-int-ext (funcall (delta-n-basis dmns) n)))
		:bsgn '(0)
		:intr-dffr #'(lambda (degr gmsm)
			(make-cmbn
				:degr (1- degr)
				:list (do ((rslt +empty-list+
					(cons (cons sign
						(append
						(subseq gmsm 0 nark)
						(subseq gmsm (1+ nark))))
						rslt))
						(sign 1 (- sign))
					(nark 0 (1+ nark)))
					((> nark degr) rslt))))
		:strt :gnrt
		:orgn ‘(Effective version of C_* delta ,dmns)))

(setf triangle (cdelta 2))
[K3 Chain-Complex]
(basis triangle 1)
((0 1) (0 2) (1 2))
(setf tpr-triangles (tnsr-prdc triangle triangle))
[K5 Chain-Complex]
(setf base (basis tpr-triangles))
(first (funcall base 1))
<TnPr (0) (0 1)>
(second (funcall base 1))
<TnPr (0) (0 2)>
(funcall (cmpr tpr-triangles) (first (funcall base 1)) (second (funcall base 1)))
:LESS
(funcall (mmcm-cmpr-of-tnpr-1 tpr-triangles) (gnrt-tnpr-to-mmulticomplex (first (funcall base 1)))  (gnrt-tnpr-to-mmulticomplex (second (funcall base 1)))) 
:LESS
(funcall (mmcm-cmpr-of-tnpr-1 tpr-triangles) (gnrt-tnpr-to-mmulticomplex (first (funcall base 1)))  (gnrt-tnpr-to-mmulticomplex (first (funcall base 1)))) 
:EQUAL

|#


;;ALTERNATIVE:
  "--------------------------------------------------------------
(DEFUN MMCM-CMPR-OF-TNPR-2 ()
Entrada: nada.
Salida: una función de comparación para generadores del tipo mmulticomplejo que vienen de producto tensorial. Hacemos lo mismo que con los mmulticomplejos usuales, pero aquí comparamos las listas de generadores del producto tensorial como MMCM-CMPRlistas, en vez de pasar una función de comparación diferente.
	(flet ((comp (mmc1 mmc2)
		(declare (type MmcGnrt mmc1 mmc2));;Duda: orden entre let y flet,
		(let ((cmpr-mmcm (mmc-cmpr l-cmpr)));;Duda: asignar a una veriable (cmmc-cmpr (l-cmpr)) o ponerlo diréctamente.
			(declare (type cmprf cmpr-mmcm))
			(funcall cmpr-mmcm mmc1 mmc2))))
	(the cmprf #'comp));;Duda: también podría poner (the cmprf ....) y definir la función ahí de alguna manera?		
)
;;Esto sería análogo, entiendo, a asumir que l-cmpr es la función de comparación de generadores, y poner en el slot :cmpr de la definición del complejo de cadenas, (mmc-cmpr (l-cmpr))
------------------------------------------------------------------------------"


(DEFUN MMCM-BASIS-OF-TNPR (chcm)
  "--------------------------------------------------------------[function-doc]
Input: chain complex of type tnpr.
Output: basis of the same chain complex seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(let ((tnprbasis (basis chcm)))
		(declare (type basis tnprbasis))
		(when (eq tnprbasis :locally-effective)
			(return-from mmcm-basis-of-tnpr :locally-effective))
		
		(flet ((bas (degr)
			(declare (type fixnum degr))
			(the list 
				(mapcar
					#'(lambda (gnrt)
						(gnrt-tnpr-to-mmulticomplex gnrt)
					)
				(funcall tnprbasis degr)))))
		(the basis #'bas)))			
)
#|
(basis tpr-triangles 1)
(funcall (mmcm-basis-of-tnpr tpr-triangles) 1)


Output:

(basis tpr-triangles 1)
(<TnPr (0) (0 1)> <TnPr (0) (0 2)> <TnPr (0) (1 2)> <TnPr (1) (0 1)> <TnPr (1) (0 2)> <TnPr (1) (1 2)> <TnPr (2) (0 1)> <TnPr (2) (0 2)> <TnPr (2) (1 2)> <TnPr (0 1) (0)> ...)
 (funcall (mmcm-basis-of-tnpr tpr-triangles) 1)
((:MMCGNRT (0 1) ((0) (0 1))) (:MMCGNRT (0 1) ((0) (0 2))) (:MMCGNRT (0 1) ((0) (1 2))) (:MMCGNRT (0 1) ((1) (0 1))) (:MMCGNRT (0 1) ((1) (0 2))) (:MMCGNRT (0 1) ((1) (1 2))) (:MMCGNRT (0 1) ((2) (0 1))) (:MMCGNRT (0 1) ((2) (0 2))) (:MMCGNRT (0 1) ((2) (1 2))) (:MMCGNRT (1 0) ((0 1) (0))) ...)

|#



(DEFUN MMCM-DFFR-OF-TNPR (chcm)
  "--------------------------------------------------------------[function-doc]
Input: chain complex of type tnpr.
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
							(term cffc (gnrt-tnpr-to-mmulticomplex gnrt))))
				(cmbn-list (dffr chcm degr (gnrt-mmulticomplex-to-tnpr mmc)))))
		)))
	(the intr-mrph #'dif)))		

#|
(dffr tpr-triangles 1 (first (basis tpr-triangles 1)))
(funcall (mmcm-dffr-of-tnpr tpr-triangles) 1 (gnrt-tnpr-to-mmulticomplex (first (basis tpr-triangles 1))))

;;(dffr tpr-triangles 1 (first (basis tpr-triangles 1)))
;;
;;----------------------------------------------------------------------{CMBN 0}
;;<1 * <TnPr NIL (0 1)>>
;;<-1 * <TnPr (0) (0)>>
;;<1 * <TnPr (0) (1)>>
;;------------------------------------------------------------------------------

;;(funcall (mmcm-dffr-of-tnpr tpr-triangles) 1 (gnrt-tnpr-to-mmulticomplex (first (basis tpr-triangles 1))))
;;
;;----------------------------------------------------------------------{CMBN 0}
;;<1 * (MMCGNRT (-1 1) (NIL (0 1)))>
;;<-1 * (MMCGNRT (0 0) ((0) (0)))>
;;<1 * (MMCGNRT (0 0) ((0) (1)))>
|#


(DEFUN TNPR-TO-MULTICOMPLEX (chcm)
  "--------------------------------------------------------------[function-doc]
Input: a chain complex of type tnpr with m factors.
Output: the same chain complex seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(the chain-complex
		(let ((chcm
			(build-chcm
				:cmpr (mmcm-cmpr-of-tnpr-1 chcm)
				:basis (mmcm-basis-of-tnpr chcm)
				:intr-dffr (mmcm-dffr-of-tnpr chcm)
				:strt :gnrt
				:bsgn (if (eq (bsgn chcm) nil)
						nil
						(gnrt-tnpr-to-mmulticomplex (bsgn chcm))
					) 
				:orgn `(tnpr-to-mmcm ,chcm))))
			(declare (type chain-complex chcm))
		chcm)))

#|
(defun cdelta (dmns)
	(build-chcm
	:cmpr #'l-cmpr
	:basis #'(lambda (n)
	(mapcar #'dlop-int-ext (funcall (delta-n-basis dmns) n)))
		:bsgn '(0)
		:intr-dffr #'(lambda (degr gmsm)
			(make-cmbn
				:degr (1- degr)
				:list (do ((rslt +empty-list+
					(cons (cons sign
						(append
						(subseq gmsm 0 nark)
						(subseq gmsm (1+ nark))))
						rslt))
						(sign 1 (- sign))
					(nark 0 (1+ nark)))
					((> nark degr) rslt))))
		:strt :gnrt
		:orgn ‘(Effective version of C_* delta ,dmns)))
(setf triangle (cdelta 2))
(setf tpr-triangles (tnsr-prdc triangle triangle))
(setf tpr (tnpr-to-multicomplex tpr-triangles))
(basis tpr 1)
(first (basis tpr 1))
(second (basis tpr 1))
(funcall (cmpr tpr) (first (basis tpr 1)) (second (basis tpr 1)))
(bsgn tpr)
(dffr tpr 1 (first (basis tpr 1)))


Output:

CAT 74 > (defun cdelta (dmns)
	(build-chcm
	:cmpr #'l-cmpr
	:basis #'(lambda (n)
	(mapcar #'dlop-int-ext (funcall (delta-n-basis dmns) n)))
		:bsgn '(0)
		:intr-dffr #'(lambda (degr gmsm)
			(make-cmbn
				:degr (1- degr)
				:list (do ((rslt +empty-list+
					(cons (cons sign
						(append
						(subseq gmsm 0 nark)
						(subseq gmsm (1+ nark))))
						rslt))
						(sign 1 (- sign))
					(nark 0 (1+ nark)))
					((> nark degr) rslt))))
		:strt :gnrt
		:orgn ‘(Effective version of C_* delta ,dmns)))
CDELTA

CAT 75 > 
(setf triangle (cdelta 2))
[K1 Chain-Complex]

CAT 76 > (setf tpr-triangles (tnsr-prdc triangle triangle))
[K3 Chain-Complex]

CAT 77 > (setf tpr (tnpr-to-multicomplex tpr-triangles))
[K5 Chain-Complex]

CAT 78 > (basis tpr 1)
((:MMCGNRT (0 1) ((0) (0 1))) (:MMCGNRT (0 1) ((0) (0 2))) (:MMCGNRT (0 1) ((0) (1 2))) (:MMCGNRT (0 1) ((1) (0 1))) (:MMCGNRT (0 1) ((1) (0 2))) (:MMCGNRT (0 1) ((1) (1 2))) (:MMCGNRT (0 1) ((2) (0 1))) (:MMCGNRT (0 1) ((2) (0 2))) (:MMCGNRT (0 1) ((2) (1 2))) (:MMCGNRT (1 0) ((0 1) (0))) ...)

CAT 79 > (first (basis tpr 1))
(:MMCGNRT (0 1) ((0) (0 1)))

CAT 80 > (second (basis tpr 1))
(:MMCGNRT (0 1) ((0) (0 2)))

CAT 81 > (funcall (cmpr tpr) (first (basis tpr 1)) (second (basis tpr 1)))
:LESS

CAT 82 > (bsgn tpr)
(:MMCGNRT (0 0) ((0) (0)))

CAT 83 > (dffr tpr 1 (first (basis tpr 1)))

----------------------------------------------------------------------{CMBN 0}
<1 * (MMCGNRT (-1 1) (NIL (0 1)))>
<-1 * (MMCGNRT (0 0) ((0) (0)))>
<1 * (MMCGNRT (0 0) ((0) (1)))>
------------------------------------------------------------------------------

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SERRE SPECTRAL SEQUENCE. INTERMEDIATE AND FINAL COMPLEXES AS M-MULTICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN SERRE-INTM-RDCT (fibration
                        &aux (base (sorc fibration))
                          (fibre (trgt fibration)))
  "--------------------------------------------------------------[function-doc]
Input: a fibration given by several twisted cartesian products.
Output: the reduction that results when we apply the Twisted Eilenberg-Zilber theorem to the initial one. In other words, the twisted tensor product of all of its factors.
------------------------------------------------------------------------------"
	(declare (type fibration fibration)
		(type simplicial-set base fibre))
	(multiple-value-bind (tnsr-rdct b-perturbation) (brown-reduction fibration)
				(declare (type reduction tnsr-rdct)
					(type morphism b-perturbation))
		(if (eq (first (orgn base)) 'FIBRATION-TOTAL)
			(if (eq (first (orgn fibre)) 'FIBRATION-TOTAL)
				(the reduction
					(cmps (basic-perturbation-lemma (tnsr-prdc (serre-intm-rdct (second (orgn base))) (serre-intm-rdct (second (orgn fibre)))) b-perturbation) tnsr-rdct))
				(the reduction 
					(cmps (basic-perturbation-lemma (tnsr-prdc (serre-intm-rdct (second (orgn base))) (trivial-rdct fibre)) b-perturbation) tnsr-rdct)))
			(if (eq (first (orgn fibre)) 'FIBRATION-TOTAL)
				(the reduction
					(cmps (basic-perturbation-lemma (tnsr-prdc (trivial-rdct base) (serre-intm-rdct (second (orgn fibre)))) b-perturbation) tnsr-rdct))
				(the reduction tnsr-rdct)))))
				
				
(DEFUN SERRE-INTM-CHCM (fibration)
  "--------------------------------------------------------------[function-doc]
Input: a fibration given by several twisted cartesian products.
Output: the chain complex that results when we apply the Twisted Eilenberg-Zilber theorem to the initial one. In other words, the twisted tensor product of all of its factors.
------------------------------------------------------------------------------" 
	(declare (type fibration fibration)
		(type simplicial-set base fibre))
	(the chain-complex 
		(bcc (serre-intm-rdct fibration))))			
				


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EFFECTIVE HOMOLOGY FOR M-MULTICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFMETHOD SEARCH-EFHM (chcm (orgn (eql 'add)))
	(declare (type chain-complex chcm))
	(the homotopy-equivalence
		(add (efhm (second (orgn chcm))) (third (orgn chcm)))))
	
		
(DEFMETHOD SEARCH-EFHM (chcm (orgn (eql 'TNPR-TO-MMCM)))
	(declare (type chain-complex chcm))
	(the homotopy-equivalence
		(mmcm-hmeq-of-tnpr (efhm (second (orgn chcm))))))
		
		
(DEFUN MMCM-INTR-MRPH-OF-TNPR (mrph)
  "--------------------------------------------------------------[function-doc]
Input: chain complex of type tnpr.
Output: its internal differential seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type morphism mrph))
	(flet ((mmcm-mrph (degr mmc)
		(declare (type fixnum degr)
			(type MmcGnrt mmc))
		(the cmbn 
			(make-cmbn
				:degr (+ degr (degr mrph))
				:list (mapcar #'(lambda (term)
						(declare (type term term))
						(let ((cffc (cffc term))
							(gnrt (gnrt term)))
							(declare (type fixnum cffc)
								(type gnrt gnrt))
							(term cffc (gnrt-tnpr-to-mmulticomplex gnrt))))
				(cmbn-list (gnrt-? mrph degr (gnrt-mmulticomplex-to-tnpr mmc)))))
		)))
	(the intr-mrph #'mmcm-mrph)))		

(DEFUN MMCM-MRPH-OF-TNPR (mrph)
	(declare (type morphism mrph))
	(the morphism
		(build-mrph 
			:sorc (tnpr-to-multicomplex (sorc mrph))
			:trgt (tnpr-to-multicomplex (trgt mrph))
			:degr (degr mrph)
			:intr (mmcm-intr-mrph-of-tnpr mrph)
			:strt (strt mrph)
			:orgn `(tnpr-to-mmcm ,mrph))
			))
			
(DEFUN MMCM-HMEQ-OF-TNPR (hmeq)
	(declare (type homotopy-equivalence hmeq))
	(the homotopy-equivalence
		(build-hmeq
			:lf (mmcm-mrph-of-tnpr (lf hmeq))
			:lg (mmcm-mrph-of-tnpr (lg hmeq))
			:lh (mmcm-mrph-of-tnpr (lh hmeq))
			:rf (mmcm-mrph-of-tnpr (rf hmeq))
			:rg (mmcm-mrph-of-tnpr (rg hmeq))
			:rh (mmcm-mrph-of-tnpr (rh hmeq))
			:orgn `(tnpr-to-mmcm ,hmeq))
			))	
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BICOMPLEXES AS 2-MULTICOMPLEXES AS BICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN GNRT-BICM-TO-2MCM (BcGnrt)
  "--------------------------------------------------------------[function-doc]
Input: A generator of type BcGnrt.
Output: The same generator of type MmcGnrt.
------------------------------------------------------------------------------"
	(declare (type BcGnrt BcGnrt))
	(the MmcGnrt
		(build-MmcGnrt (list (BcGnrt-Degr1 BcGnrt) (BcGnrt-Degr2 BcGnrt)) (BcGnrt-Gnrt BcGnrt))))
			
(DEFUN GNRT-2MCM-TO-BICM (MmcGnrt)
  "--------------------------------------------------------------[function-doc]
Input: A generator of type BcGnrt.
Output: The same generator of type MmcGnrt.
------------------------------------------------------------------------------"
	(declare (type MmcGnrt MmcGnrt))
	(the BcGnrt
		(build-BCGnrt (MmcGnrt-Degr MmcGnrt 1) (MmcGnrt-Degr MmcGnrt 2) (MmcGnrt-Gnrt MmcGnrt))))


(DEFUN MMCM-CMPR-OF-BICM (chcm)
  "--------------------------------------------------------------[function-doc]
Input: recieves a chain complex with generators of type bicm.
Output: returns the same comparison function of that chain complex but such that it applies on generators of type mmcgnrt.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(let ((cmpr-bicm (cmpr chcm)))
		(flet ((comp (mmc1 mmc2)
			(declare (type MmcGnrt mmc1 mmc2))
			(funcall cmpr-bicm (gnrt-2mcm-to-bicm mmc1) (gnrt-2mcm-to-bicm mmc2))
			)) 
			(the cmprf #'comp))))
			
(DEFUN MMCM-BASIS-OF-BICM (chcm)
  "--------------------------------------------------------------[function-doc]
Input: chain complex of type bicm.
Output: basis of the same chain complex seen as an m-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(let ((bicmbasis (basis chcm)))
		(declare (type basis bicmbasis))
		(when (eq bicmbasis :locally-effective)
			(return-from mmcm-basis-of-bicm :locally-effective))
		
		(flet ((bas (degr)
			(declare (type fixnum degr))
			(the list 
				(mapcar
					#'(lambda (gnrt)
						(gnrt-bicm-to-2mcm gnrt)
					)
				(funcall bicmbasis degr)))))
		(the basis #'bas)))			
)


(DEFUN MMCM-DFFR-OF-BICM (chcm)
  "--------------------------------------------------------------[function-doc]
Input: chain complex of type bicm.
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
							(term cffc (gnrt-bicm-to-2mcm gnrt))))
				(cmbn-list (dffr chcm degr (gnrt-2mcm-to-bicm mmc)))))
		)))
	(the intr-mrph #'dif)))	
	
(DEFUN BICM-TO-MULTICOMPLEX (chcm)
  "--------------------------------------------------------------[function-doc]
Input: a chain complex of type bicomplex.
Output: the same chain complex seen as a 2-multicomplex.
------------------------------------------------------------------------------"
	(declare (type chain-complex chcm))
	(the chain-complex
		(let ((chcm
			(build-chcm
				:cmpr (mmcm-cmpr-of-bicm chcm)
				:basis (mmcm-basis-of-bicm chcm)
				:intr-dffr (mmcm-dffr-of-bicm chcm)
				:strt :gnrt
				:bsgn (if (eq (bsgn chcm) nil)
						nil
						(gnrt-bicm-to-2mcm (bsgn chcm))
					) 
				:orgn (append (list 'BICM-TO-MMC) (orgn chcm)))))
			(declare (type chain-complex chcm))
		chcm)))





