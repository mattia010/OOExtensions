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
  (add-class-spec class-name
		  (list parents-list
			(get-fields parts)
			(get-methods parts))))

;;; Crea e ritorna una lista rappresentante un'istanza, senza salvarla.
(defun make (class-name &rest args)
  (cond ((is-class class-name)
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
  (if (equal (car instance) 'oolinst)
      (if (equal class T)
	  T
	  (is-derivated-class (cadr instance) class))
      NIL))

(defun field (instance field)
  (field-in-fields-list field (caddr instance)))

(defun get-fields (parts)
  (cond ((null parts) '())
	((equal (caar parts) 'fields)
	 (append (recursive-validate-fields (cdar parts))
	       (get-fields (cdr parts))))
	(T (get-fields (cdr parts)))))

(defun recursive-validate-fields (fields)
  (if (null fields)
      '()
      (append (list (validate-field (car fields)))
	      (recursive-validate-fields (cdr fields)))))

(defun validate-field (field)
  (cond ((equal (length field) 2)
	 (list (car field) (make 'T (cadr field))))
	((equal (length field) 3)
	 (list (car field) (make (caddr field) (cadr field))))))

(defun get-methods (parts)
  (cond ((null parts) '())
	((equal (caar parts) 'methods)
	 (append (recursive-process-methods (cdar parts))
		 (get-methods (cdr parts))))
	(T (get-methods (cdr parts)))))

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
	((field-in-fields-list (car args) old-fields)
	 (update-fields (update-field old-fields
				      (list (car args) (cadr args)))
			(cddr args)))
	(T (error "Error when specifying fields to update."))))

(defun update-field (old-fields field)
  (cond ((null old-fields) (error "Field not found"))
	((equal (caar old-fields) (car field))
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



;OK e discendenti
(defun is-derivated-class (derivated-class parent-class)
  (cond ((equal derivated-class parent-class))
	((are-derivated-parents (class-spec-parents derivated-class)
				parent-class))))
	 
(defun are-derivated-parents (parents-list parent-class)
  (cond ((null parents-list) NIL)
	((equal (car parents-list) parent-class))
	((are-derivated-parents (class-spec-parents (car parents-list))
				parent-class))
	((are-derivated-parents (cdr parents-list) parent-class))))



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
