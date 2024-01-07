(defparameter *classes-specs* (make-hash-table))

(defun add-class-spec (class class-spec)
  (setf (gethash class *classes-specs*) class-spec))

(defun class-spec-parents (class)
  (car (gethash class *classes-specs*)))
(defun class-spec-fields (class)
  (cadr (gethash class *classes-specs*)))
(defun class-spec-methods (class)
  (caddr (gethash class *classes-specs*)))

;;; Definisce una nuova classe, memorizzandola in una hashtable
(defun def-class (class-name parents-list &rest parts)
  (progn (if (is-class class-name)
	     (error "Class already defined."))
	 (if (not (parents-exist parents-list))
	     (error "One or more parent classes are not defined."))
	 (progn (add-class-spec
		 class-name
		 (list parents-list
		       (inherit-field-type
			(get-fields parts)
			(inherite-fields-from-parents parents-list))
		       (get-methods parts)))
		class-name)))

;;; OK
(defun parents-exist (parents-list)
  (cond ((null parents-list) T)
	((is-class (car parents-list))
	 (parents-exist (cdr parents-list)))))

;;; Crea e ritorna una lista rappresentante un'istanza, senza salvarla.
(defun make (class-name &rest args)
  (cond ((and (equal (length args) 1)
	      (is-instance (car args) class-name))
	 (car args))
	((is-class class-name)
	 (list 'oolinst
	       class-name
	       (update-fields (append-fields-lists
			       (class-spec-fields class-name)
			       (inherite-fields-from-parents
				(class-spec-parents class-name)))
			      args)))
	((and (equal class-name 'T)
	      (equal (length args) 1))
	 (list 'oolinst 'T (car args)))
	((and (equal (length args) 1)
	      (subtypep (type-of (car args)) class-name))
	 (list 'oolinst class-name (car args)))
	(T (error "Args not valid for the specified type or class."))))

;;; Verifica l'esistenza di una classe.
(defun is-class (class-name)
  (if (gethash class-name *classes-specs*)
      T
      NIL))

;;; Verifica se il parametro è un'istanza valida ed, eventualmente, se è
;;; istanza di una certa classe.
(defun is-instance (instance &optional (class T))
  (if (and (listp instance)
	   (equal (car instance) 'oolinst))
      (if (equal class T)
	  (or (is-class (cadr instance))
	      (subtypep (type-of (caddr instance)) (cadr instance)))
	  (and (is-subtype-of (cadr instance) class)
	       (or (is-class (cadr instance))
		   (subtypep (type-of (caddr instance)) (cadr instance)))))
      NIL))



(defun field (instance field)
  (if (is-instance instance)
      (if (field-in-fields-list (symbol-name field) (caddr instance))
	  (field-in-fields-list (symbol-name field) (caddr instance))
	  (error "unknown field."))
      (error "Instance expected as parameter!")))

(defun field* (instance &rest fields-name-list)
  (if (not (equal (length fields-name-list) 0))
      (cond ((equal (length fields-name-list) 1)
	     (field instance (car fields-name-list)))
	    (T (field* (field instance (car fields-name-list))
		       (cdr fields-name-list))))
      (error "List of fields name is empty")))
				  
; OK estrae dalla lista di parti tutti i campi e ritorna una lista
; di campi ((nome1 istanza1) (nome2 istanza2) ...)
(defun get-fields (parts)
  (cond ((null parts) '())
	((equal (caar parts) 'fields)
	 (append (recursive-validate-fields (cdar parts))
		 (get-fields (cdr parts))))
	(T (get-fields (cdr parts)))))

;OK ritorna una lista di campi ((nome1 istanza1) (nome2 istanza2) ...)
(defun recursive-validate-fields (fields)
  (if (null fields)
      '()
      (append (list (validate-field (car fields)))
	      (recursive-validate-fields (cdr fields)))))

; OK ritorna una lista (nome-campo istanza)
(defun validate-field (field)
  (cond ((equal (length field) 2)
	 (list (symbol-name (car field)) (make 'T (cadr field))))
	((equal (length field) 3)
	 (list (symbol-name (car field)) (make (caddr field) (cadr field))))))

(defun inherit-field-type (fields-list parents-fields-list)
  (cond ((null fields-list) '())
	((null parents-fields-list) fields-list)
	((and (equal (caar fields-list) (caar parents-fields-list))
	      (equal (cadadr (car fields-list)) 'T))
	 (if (is-instance (list 'oolinst
				(cadadr (car parents-fields-list))
				(caddr (cadr (car fields-list)))))
	     (append (list (list (caar fields-list)
				 (list 'oolinst
				       (cadadr (car parents-fields-list))
				       (caddr (cadr (car fields-list))))))
		     (inherit-field-type (cdr fields-list)
					 (cdr parents-fields-list)))
	     (error "Invalid value for a field.")))
	((equal (caar fields-list) (caar parents-fields-list))
	 (if (is-subtype-of (cadadr (car fields-list))
			    (cadadr (car parents-fields-list)))
	     (append (list (car fields-list))
		     (inherit-field-type
		      (cdr fields-list)
		      (cdr parents-fields-list)))
	     (error "A field type is supertype of an inherited one")))
	(T (append (inherit-field-type (list (car fields-list))
				       (cdr parents-fields-list))
		   (inherit-field-type (cdr fields-list)
				       parents-fields-list)))))

; OK estrae da una lista di parti tutti i metodi, dopo aver definito delle
; funzioni trampolino per essi.
(defun get-methods (parts)
  (cond ((null parts) '())
	((equal (caar parts) 'methods)
	 (append (recursive-process-methods (cdar parts))
		 (get-methods (cdr parts))))
	(T (get-methods (cdr parts)))))

; OK ritorna una lista di funzioni lambda (una per ogni membro)
;;; dopo aver creato le funzioni trampolino
(defun recursive-process-methods (methods)
  (if (null methods)
      '()
      (append (list (process-method (caar methods) (cdar methods)))
	      (recursive-process-methods (cdr methods)))))


;;; Crea una funzione lambda trampolino, per richiamare il metodo specificato
;;; come parametro, e una funzione lambda equivalente al metodo.
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name)
	(lambda (this &rest args)
	  (apply (eval (invoke-method this method-name))
		 (append (list this) args))))
  (rewrite-method-code method-name method-spec))

;;; OK
;;; Riscrive un metodo come una funzione lambda, con un parametro aggiuntivo
;;; this che rappresenta l'istanza.
(defun rewrite-method-code (method-name method-spec)
  (list method-name (append (list 'lambda)
			    (list (append (list 'this) (car method-spec)))
			    (cdr method-spec))))

;;; Ritorna la funzione lambda più adatta (tenendo conto dell'ereditarietà)
;;; equivalente al metodo che si vuole richiamare sull'istanza.
(defun invoke-method (instance method-name)
  (cond ((method-in-methods-list method-name
				 (class-spec-methods (cadr instance))))
	((parents-method (class-spec-parents (cadr instance)) method-name))
	(T (error "Method does not exist!"))))


(defun update-fields (old-fields args)
  (cond ((null args) old-fields)
	((field-in-fields-list (symbol-name (car args)) old-fields)
	 (update-fields (update-field old-fields
				      (list (car args) (cadr args)))
			(cddr args)))
	(T (error "Error when specifying fields to update."))))

(defun update-field (old-fields field)
  (cond ((null old-fields) (error "Field not found"))
	((equal (caar old-fields) (symbol-name (car field)))
	 (append (list (list (caar old-fields)
			     (make (cadr (cadar old-fields)) (cadr field))))
		 (cdr old-fields)))
	(T (append (list (car old-fields))
		   (update-field (cdr old-fields) field)))))

(defun inherite-fields-from-parents (parents-list)
  (if (not (null parents-list))
      (append-fields-lists
       (append-fields-lists (class-spec-fields (car parents-list))
			    (inherite-fields-from-parents
			     (class-spec-parents (car parents-list))))
       (inherite-fields-from-parents (cdr parents-list)))
      '()))

(defun append-fields-lists (list1 list2)
  (if (not (null list2))
      (if (not (field-in-fields-list (caar list2) list1))
	  (append-fields-lists (append list1 (list (car list2)))
			       (cdr list2))
	  (append-fields-lists list1 (cdr list2)))
      list1))


(defun is-subtype-of (subclass parent-class)
  (cond ((equal parent-class 'T) T)
	((equal subclass parent-class) T)
	((is-derivated-class subclass parent-class) T)
	((subtypep subclass parent-class) T)))

;OK e discendenti
(defun is-derivated-class (derivated-class parent-class)
  (is-derivated-class-helper (list derivated-class) parent-class))

(defun is-derivated-class-helper (derivated-classes parent-class)
  (cond ((null derivated-classes) NIL)
	((equal (car derivated-classes) parent-class) T)
	((is-derivated-class-helper (class-spec-parents
				     (car derivated-classes))
				    parent-class) T)
	((is-derivated-class-helper (cdr derivated-classes) parent-class) T)))

(defun parents-method (parents-list method-name)
  (cond ((null parents-list) NIL)
	((method-in-methods-list method-name
				 (class-spec-methods (car parents-list))))
	((parents-method (class-spec-parents (car parents-list)) method-name))
	((parents-method (cdr parents-list) method-name))))

(defun method-in-methods-list (method-name methods-list)
  (cond ((null methods-list) NIL)
	((equal method-name (caar methods-list)) (cadar methods-list))
	(T (method-in-methods-list method-name (cdr methods-list)))))

(defun field-in-fields-list (field fields-list)
  (cond ((null fields-list) NIL)
	((equal field (caar fields-list))
	 (if (is-class (cadr (cadar fields-list)))
	     (cadar fields-list)
	     (caddr (cadar fields-list))))
	(T (field-in-fields-list field (cdr fields-list)))))
