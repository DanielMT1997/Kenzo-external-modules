;;M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES
;;M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES
;;M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES M-MULTICOMPLEXES


(IN-PACKAGE #:cat)

(PROVIDE "bicomplexes")

;;Print keycons? No sé lo que hace.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPES FOR M-MULTICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TYPE MmcGnrt (a generator in an m-multicomplex)
(DEFUN MmcGnrt-p (object)
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

;;Para bicomplejos teníamos un cons de dos elementos, en el caso de abajo, (cons 5 6), que sería algo así: (5.6). Una lista son cons encadenados de manera que el último lugar está vacío. Es decir, para m-multicomplejos tenelos (list 5 6) que es algo así: (5.(6.)). 

#|
(typep `(:MmcGnrt ,(list 5 6) u) 'MmcGnrt) ==>T
|#

 
;; TYPE MmcBasis (basis of an m-multicomplex, function of the variables p and q or
;; ":locally-effective")
(DEFTYPE MmcBASIS () '(or function (eql :locally-effective))) 


;; (function (degr degr) (list gnrt)) 
;;En el caso de los multicomplejos, habrá m-1 degrs.

;; TYPE Mmulticomplex
(DEFUN OrgnMmCmpl-p (object)
  (declare (type chain-complex object))
  (let* ((orgn (orgn object)))
    (the boolean
      (and (consp orgn)
           (eql (car orgn) 'MmCmpl)))))
(DEFTYPE Mmcomplex () '(satisfies OrgnMmCmpl-p))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS FOR THE REPRESENTATION OF M-MULTICOMPLEXES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Function that builds a generator of a multicomplex from a list of m+1 degrees and 
;; a simple generator.
(DEFUN build-MmcGnrt (l gnrt)
	(declare (type any gnrt)
		(type list l))
	(list :MmcGnrt l gnrt));;Entiendo que esto se puede hacer así. Igual hay alguna manera de pasar tan solo los grados en vez de la lista.

#|
 (build-MmcGnrt (list 2 3) 'c)==>(:MMCGNRT (2 3) C)
(typep  (build-MmcGnrt (list 2 2) 'd) 'MmcGnrt)==>T
|#
 


;; Function that returns the i-th degree from a multicomplex generator.
(DEFUN MmcGnrt-Degr (MmcGnrt i)
  (declare (type MmcGnrt MmcGnrt)
  	(type fixnum i))
  (nth i (cadr MmcGnrt)))
  
;;Function that returns the multidegree of a multicomplex generator.
(DEFUN MmcGnrt-Mdegr (MmcGnrt)
	(declare (type MmcGnrt MmcGnrt))
	(second MmcGnrt))

;; Function that returns the original generator from an m-multicomplex generator.
(DEFUN MmcGnrt-Gnrt (MmcGnrt)
  (declare (type MmcGnrt MmcGnrt))
  (caddr MmcGnrt))
  
;;Function that returns the dimension of the generator.
(DEFUN MmcGnrt-Dmns (MmcGnrt)
	(declare (type MmcGnrt MmcGnrt))
	(length (second MmcGnrt)))

#|
(build-MmcGnrt (list 5 6 8) 'd)==>(:MMCGNRT (5 6 8) D)
(MmcGnrt-Degr (build-MmcGnrt (list 5 6 8) 'd) 0)==>5
(MmcGnrt-Degr (build-MmcGnrt (list 5 6 8) 'd) 1)==>6
(MmcGnrt-Degr (build-MmcGnrt (list 5 6 8) 'd) 2)==>8
(MmcGnrt-Gnrt (build-MmcGnrt (list 6 4) '(4 6)))==>(4 6)
(MmcGnrt-Gnrt (build-MmcGnrt (list 2 8) 'u))==>U
(MmcGnrt-Gnrt (build-MmcGnrt (list 1 2 3 4 5 5 6) 'u))==>U
(MmcGnrt-Mdegr (build-MmcGnrt (list 1 2 3 4 5 5 6) 'u))==>(1 2 3 4 5 5 6)
|# 

 
;; Comparison function of a multicomplex.
(defun MMC-CMPR (cmpr)
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



	
;;(concatenate 'list (mapcar
;;						#'(lambda (d1 d2)
;;							(declare (type fixnum d1 d2))
;;							(f-cmpr d1 d2))
;;						(MmcGnrt-Mdegr mmc1) (MmcGnrt-Mdegr mmc2))
;;						(list (funcall cmpr gnrt1 gnrt2)))

;;(dotimes (i l1)
;;						(f-cmpr (MmcGnrt-degr mmc1 i) (MmcGnrt-degr mmc2 i)))
;;					(funcall cmpr gnrt1 gnrt2)

#|
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

;;MMC-Basis takes a basis of a multicomplex (i. e. a function that given a multiindex of m components returns the basis of the module that is indexed likewise) and returns the basis function of the totalization of the m-multicomplex, that is, a function that given a degree returns a list of all generators of that degree.
;;degr is the degree on the totalization. Therefore we must take all the possible sums that give us the corresponding degr.
(DEFUN MMC-BASIS (mmcbasis m)
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
				
					
;;Auxiliary function that, given an integer n, an integer m and a list l, returns a list of lists which are composed of l followed by a list of m numbers that sum n.

(DEFUN LISTMAKER (n m l)
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
		
		
		
(defun group-list (list m)
  (loop repeat (/ (length list) m) collect
       (loop repeat m collect (pop list))))
       
       			
;;(DEFUN LISTMINORS (n m l)
;;	(declare (type fixnum n m)
;;		(type list l))
;;		(when (minusp n)
;;			(return-from listminors nil))
;;		(if (eql m 1)
;;			(mapcar 
;;				#'(lambda (lista)
;;					(append lista n)
;;				)
;;			l)
;;			(listminors (- n i) (- m 1) (mapcar #'(lambda (i)(mapcar #'(lambda (lista)(append lista i))l))(<a-b> 0 (+ 1 n)))
;;			)
;;		)
;;)


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

;; Differential function of an m-multicomplex (from a list of the basic differentials of multidegree e_i, from i=0,...,m). This function revieves a list of pairs of differentials and their multidegree (m+1 components), and returns an intr-mrph that given a generator and a degree, returns a combination of applying the differentials, combination that lies on the totalization complex.
(DEFUN MMC-INTR-DFFR (l)
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

;; Function that builds an m-multicomplex from a basis function, the two differential
;; functions and the original comparison. 
(DEFUN BUILD-MMCM (&key mmcbasis m l cmpr orgn)
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
				:orgn orgn)))
			(declare (type chain-complex chcm))
			(slot-makunbound chcm 'bsgn)
		chcm)))

#|
(setf mmc (Build-Mmcm :mmcbasis #'bas :l (list (list #'dif1 (list -1 0)) (list #'dif2 (list 0 -1))) :cmpr #'s-cmpr 
                     :orgn '(MMC-test)))
(dotimes (i 5)
  (print (basis mmc i)))
(cmpr mmc (build-MmcGnrt 0 1 'a) (build-MmcGnrt 1 1 'c))

(? mmc (? bc 2 (build-MmcGnrt 1 1 'c))) 
(? mmc 2 (build-MmcGnrt 2 0 'd))
(? mmc 2 '(:MmcGnrt (1 . 1) c))
(? mmc (cmbn 2 3 '(:MmcGnrt (1 . 1) c)))
(? mmc 1 (build-MmcGnrt 0  1 'a))
|#




(DEFVAR MMCFlin)

;; Filtration index function of a Multicomplex.
(setf MMCflin 
	#'(lambda (degr gnrt)
	(declare 
		(type fixnum degr)
		(type MmcGnrt gnrt))
	(the list 
		(let ((l (cdr (MmcGnrt-Mdegr gnrt)))) ;; (reverse (cdr (reverse (MmcGnrt-Mdegr gnrt))))
		l))))
		
		
		

#|
(funcall MMCflin 5 (build-bcgnrt 2 3 'a))
(funcall MMCflin 7 (build-bcgnrt 6 1 'c))
|# 


;;También se podría pasar el m como argumento, no sería muy diferente a esto.		
;;Función que define los posets de tipo zm. Toma dos listas y las compara.
;;Entrada: no tiene.
;;Salida: El poset Zm.		
(DEFUN Zm ()
	(build-poset :pocmpr
		#'(lambda (l1 l2)
			(declare (type list l1 l2))
				(let ((m (length l1))
					(m1 (lenght l2)))
					(if (= m m1)
						(progn
							(do(
								(x (first l1))
								(y (first l2))
								(l1 (cdr l1))
								(l2 (cdr l2))
								(= (length l1) 0))
								((if (<= x y);;(\= x y)
									(if (< x y)
										(progn
											(setq rslt (- rslt 1))
											(setq c 'T)
										)
										(setq rslt (+ rslt 1))
									)
								))
							)
							(if (= rslt m)
								:less
								(if (= rslt -m)
									:greater
									(if (and (= rslt 0) (= c 'T))
										:equal
										:undefined
									)
								)
							)
						)
						(format t "Las tuplas tienen diferente longitud.")
					)
				)
			)
			:orgn '(zm)
	)
)		

					
               

		
(DEFUN DZm ()
  (downsets (zm)))



;; Function that changes the bicomplex bc to a filtered complex, with filtration
;; index of a generator defined as its first degree. 
(defun CHANGE-MMCM-TO-GFLCC (mmcm) 
	(declare (type Mmcomplex mmcm))
	(let* ((flin MMCflin))
		(declare (type gen-chcm-flin flin))
		(the FILTERED-CHAIN-COMPLEX
			(change-chcm-to-gflcc mmcm (zm) mmcflin 'mmcflin))))

#|
(defun bas (degr1 degr2)
  (if (and (= degr1 0) (= degr2 1)) (return-from bas '(a)))
  (if (and (= degr1 1) (= degr2 0)) (return-from bas '(b)))
  (if (and (= degr1 1) (= degr2 1)) (return-from bas '(c)))
  (if (and (= degr1 2) (= degr2 0))  (return-from bas '(d)))
  (return-from bas nil))

(defun dif1 (degr1 degr2 gnrt)
  (if (and (= degr1 1) (= degr2 1) (eql gnrt 'c)) (return-from dif1 (list (cons 2 'a))))
  (if (and (= degr1 2) (= degr2 0) (eql gnrt 'd)) (return-from dif1 (list (cons 2 'b))))
  (return-from dif1 nil))
(defun dif2 (degr1 degr2 gnrt)
  (if (and (= degr1 1) (= degr2 1) (eql gnrt 'c)) (return-from dif2 (list (cons 1 'b))))
  (return-from dif2 nil))

(setf bc (Build-Bicm :bcbasis #'bas :dffr1 #'dif1 :dffr2 #'dif2 :cmpr 's-cmpr 
                     :orgn '(BC-test)))

(change-bicm-to-flcc bc)
(flin bc 1 (build-bcgnrt 0 1 'a))
(flin bc (cmbn 2 4 (build-bcgnrt 1 1 'c)))
(flin bc (cmbn 2 4 (build-bcgnrt 1 1 'c) 2 (build-bcgnrt 2 0 'd)))
(bigrd-basis bc 1 1)
(bigrd-basis bc 2 0)
(fltrd-basis bc 2 1)
(ordered-basis bc 2)
(flcc-dffr-mtrx bc 2 1)
(flcc-dffr-mtrx bc 2 2)
|#			



;;Construir un multicomplejo con tan solo una diferencial tensorial. En teoría se debe poder utilizar BUILD-MMCM.

;;Añadir una perturbación a un multicomplejo. Creo que no hace falta.
;;Input: An m-multicomplex together with a list of the differentials that we want to add to the multicomplex.
;;Output: A new perturbed multicomplex.
(DEFUN ADD-LIST (mmcm arrows)
	(declare (type chain-complex mmcm)
		(type list arrows))
	(setf (intr-dffr mmcm) (add (intr-dfrr mmcm) (MMC-INTR-DFFR arrows))))
	
;;Añadir una perturbación a una equivalencia de homotopía.
;;Input: An homotopy equivalence h and a list of morphisms that	
(DEFUN ADD-PERTURBATION (h delta)
	(declare (type chain-complex mmcm)
		(type list delta)
		(type homotopy-equivalence h))
	(let* ((mmcm (lbcc h))
		(pert (mmc-intr-dffr delta)))
		(the homotopy-equivalence
			(let ((hmeq (add h pert)))
			hmeq))))
			
			
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TENSOR PRODUCTS AS M-MULTICOMPLEXES AS TENSOR PRODUCTS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Seeing the tensor product generators as m-multicomplexes generators.

;;Recibe un generador de tipo tnpr y lo ve como un mmulticomplejo.		
(DEFUN GNRT-TNPR-TO-MMULTICOMPLEX (tnpr)
	(declare (type tnpr tnpr))
		(the MmcGnrt (build-MmcGnrt (tnpr-multidegree tnpr nil) (tnpr-mmcmgnrt tnpr nil))))
		
;;*1: Non recursive alternative for fibrations.

;;Recibe un generador de tipo producto tensorial y una lista. Devuelve el multigrado asociado al producto tensorial, más la lista (arriba le pasamos la lista vacía).				
(DEFUN TNPR-MULTIDEGREE (tnpr l)
	(declare (type tnpr tnpr)
		(type list l))
	(let ((degr1 (degr1 tnpr))
		(degr2 (degr2 tnpr))
		(gnrt1 (gnrt1 tnpr))
		(gnrt2 (gnrt2 tnpr)))
		(progn
			(declare (type fixnum degr1 degr2))
			(if (tnpr-p gnrt1)
				(setq l (append l (TNPR-MULTIDEGREE gnrt1 l)))
				(progn
					(setq l (append l '(degr1)))
					(if (tnpr-p gnrt2)
						(setq l (append l (TNPR-MULTIDEGREE gnrt2 l)))
						(setq l (append l '(degr2)))
					)
				)
			)
			(the list l)
		)
	)
)

;;Recibe un generador de tipo producto tensorial y una lista. Devuelve el generador asociado al producto tensorial, más la lista (arriba le pasamos la lista vacía).
(DEFUN TNPR-MMCMGNRT (tnpr l)
	(declare (type tnpr tnpr)
		(type list l))
	(let ((gnrt1 (gnrt1 tnpr))
		(gnrt2 (gnrt2 tnpr)))
		(progn
			(if (tnpr-p gnrt1)
				(setq l (append l (TNPR-MMCMGNRT gnrt1 l)))
				(progn
					(setq l (append l '(gnrt1)))
					(if (tnpr-p gnrt2)
						(setq l (append l (TNPR-MMCMGNRT gnrt2 l)))
						(setq l (append l '(gnrt2)))
					)
				)
			)
			(the list l)
		)
	)
)

;;Recibe un generador de tipo m-multicomplejo del que sabemos que provenía de un producto tensorial. Devuelve el mismo generador como producto tensorial.	Por conveniencia, y dado que el producto tensorial es asociativo, se consruyen los generadores de manera que queda ((((((,),),),),),). Se elige así porque en el caso de las torres de fibraciones está hecho así.
(DEFUN GNRT-MMULTICOMPLEX-TO-TNPR (mmcgnrt)
	(declare (type mmcgnrt mmcgnrt))
	(let ((mdgr (rest (mmcgnrt-mdegr mmcgnrt)))
		(gnrt (rest (mmcgnrt-gnrt mmcgnrt)))
		(gnrt1 (first (mmcgnrt-mdegr mmcgnrt)))
		(degr1 (first (mmcgnrt-gnrt mmcgnrt))))
		(declare (type list mdgr gnrt l)
			(type fixnum degr1)
			(type gnrt gnrt))
		(progn
			(do 
				(
					(degr2 (first mdgr) (first mdgr))
					(gnrt2 (first gnrt) (first gnrt))
					(tnpr (tnpr degr1 gnrt1 degr2 gnrt2) (tnpr degr1 gnrt1 degr2 gnrt2))
					(degr1 degr1 (+ degr1 degr2))
					(gnrt1 gnrt1 tnpr)
					(mdgr mdgr (rest mdgr))
					(gnrt gnrt (rest mmcgnrt))
				)
				(
					(> (length mdgr) 0);;duda: (mdgr) y si es nil sale, que realmente es lo que quiero.
				)
			)
			(the tnpr tnpr))))
					
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPARISON FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Entrada: recibe un complejo de cadenas con generadores del tipo producto tensorial. 
;;Salida: devuelve la misma función de comparación de dicho complejo pero que aplica en objetos de tipo mmmulticomplejo.
(DEFUN MMCM-CMPR-OF-TNPR-1 (chcm)
	(declare (type chain-complex chcm))
	(let ((cmpr-tnpr (cmpr chcm)))
		(flet ((comp (mmc1 mmc2)
			(declare (type MmcGnrt mmc1 mmc2))
			(cmpr-tnpr (gnrt-mmulticomplex-to-tnpr mmc1) (gnrt-mmulticomplex-to-tnpr mmc2))
			)) 
			(the cmprf #'comp))))
			
;;Entrada: nada.
;;Salida: una función de comparación para generadores del tipo mmulticomplejo que vienen de producto tensorial. Hacemos lo mismo que con los mmulticomplejos usuales, pero aquí comparamos las listas de generadores del producto tensorial como listas, en vez de pasar una función de comparación diferente.
(DEFUN MMCM-CMPR-OF-TNPR-2 ()
	(flet ((comp (mmc1 mmc2)
		(declare (type MmcGnrt mmc1 mmc2));;Duda: orden entre let y flet,
		(let ((cmpr-mmcm (mmc-cmpr (l-cmpr))));;Duda: asignar a una veriable (cmmc-cmpr (l-cmpr)) o ponerlo diréctamente.
			(declare (type cmprf cmpr-mmcm))
			(funcall cmpr-mmcm mmc1 mmc2))))
	(the cmprf #'comp));;Duda: también podría poner (the cmprf ....) y definir la función ahí de alguna manera?		
)
;;Esto sería análogo, entiendo, a asumir que l-cmpr es la función de comparación de generadores, y poner en el slot :cmpr de la definición del complejo de cadenas, (mmc-cmpr (l-cmpr))


;;;;;;;;;;;;;;;;;;;;;;
;;; BASIS FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;	

;;Entrada: complejo de cadenas de tipo producto tensorial.
;;Salida: base del mismo complejo de cadenas visto como mmulticomplejo.
(DEFUN MMCM-BASIS-OF-TNPR (chcm)
	(declare (type chain-complex chcm))
	(let ((tnprbasis (basis chcm)));;Mejor definir aquí y llamar con funcall o llamar abajo?
		(when (eq tnprbasis :locally-effective)
			(return-from mmcm-basis-of-tnpr :locally-effective))
		(declare (type basis tnprbasis))
		(flet ((bas (degr)
			(declare (type fixnum degr))
			(the list 
				(mapcan
					#'(lambda (gnrt)
						(gnrt-tnpr-to-mmulticomplex gnrt)
					)
				(funcall tnprbasis degr)))))
		(the basis #'bas)))			
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTERNAL DIFFERENTIAL FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Entrada: complejo de cadenas de tipo producto tensorial.
;;Salida: diferencial interna del mismo complejo de cadenas visto como multicomplejo.
(DEFUN MMCM-DFFR-OF-TNPR (chcm)
	(declare (type chain-complex chcm))
	(flet ((dif (degr mmc)
		(declare (type fixnum degr)
			(type MmcGnrt mmc))
		(the cmbn 
			(make-cmbn
				:degr (- degr 1)
				:list (mapcan #'(lambda (term)
						(declare (type term term))
						(let ((cffc (cffc term))
							(gnrt (gnrt term)))
							(declare (type fixnum cffc)
								(type gnrt gnrt))
							(term cffc (gnrt-mmulticomplex-to-tnpr gnrt))))
				(cmbn-list (funcall (dffr chcm degr (gnrt-mmulticomplex-to-tnpr mmc))))));;Esto por ejemplo es diferente a lo de antes. Podría haber declarado dffr chcm como variable.
		)))
	(the intr-mrph #'dif)))		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TENSOR PRODUCT AS M-MULTICOMPLEX ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Entrada: un complejo de cadenas de tipo producto tensorial.
;;Salida: el mismo complejo de cadenas visto como multicomplejo.
(DEFUN TNPR-TO-MULTICOMPLEX (chcm)
	(declare (type chain-complex chcm))
	(the chain-complex
		(let ((chcm
			(build-chcm
				:cmpr (mmcm-cmpr-of-tnpr-1 chcm)
				:basis (mmcm-basis-of-tnpr chcm)
				:intr-dffr (mmcm-dffr-of-tnpr chcm)
				:strt :gnrt 
				:orgn (orgn chcm))))
			(declare (type chain-complex chcm))
			(slot-makunbound chcm 'bsgn)
		chcm)))


;;*1:Non-recursive alternative for fibrations.				
;;(DEFUN GNRT-TNPR-TO-MMULTICOMPLEX (tnpr)				
;;	(declare (type tnpr tnpr))
;;		(let ((l nil)
;;			(g nil)
;;			)
;;			(declare (type fixnum degr1 degr2)
;;				(type gnrt gnrt2)
;;				(type list l g))
;;			(progn
;;				(do
;;					(;;¿Orden? Siguiendo con el orden habitual de Kenzo asumo ((((((A t B)t C ) t )...)...) ... t F)
;;						(degr2 (degr2 tnpr))
;;						(gnrt1 (gnrt1 tnpr))
;;						(gnrt2 (gnrt2 tnpr))
;;						(tnpr (gnrt1 tnpr))
;;					)
;;					((tnpr-p tnpr))
;;					((progn
;;						(push degr2 l)
;;						(push gnrt2 g)
;;					))
;;				)
;;				(the MmcGnrt (build-MmcGnrt l (push gnrt1 g))))))
