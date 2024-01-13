;;;; Bolognini Mattia 870401
;;;; Nessuna collaborazione.

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

;;; Verifica che tutti le classi parent nella lista siano classi già
;;; definite
(defun parents-exist (parents-list)
  (cond ((null parents-list) T)
	((is-class (car parents-list))
	 (parents-exist (cdr parents-list)))))

;;; Crea e ritorna una lista rappresentante un'istanza, senza salvarla.
(defun make (class-name &rest args)
  (cond ((and (equal (length args) 1)
	      (listp (car args))
	      (equal (length (car args)) 3)
	      (equal (caar args) 'oolinst)
	      (is-subtype-of (cadar args) class-name))
	 (if (is-subtype-of (cadar args) class-name)
	     (list 'oolinst class-name (caddar args))
	     (error "Args not valid for the specified type.")))
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
	   (equal (length instance) 3)
	   (equal (car instance) 'oolinst)
	   (is-subtype-of (cadr instance) class))
      (if (is-class (cadr instance))
	  (is-instance-helper (cddr instance))
	  (is-subtype-of (type-of (caddr instance)) (cadr instance)))
      (is-subtype-of (type-of instance) class)))

;;; Funzione helper per usare la ricorsione sulla lista di campi
(defun is-instance-helper (fields-list)
  (cond ((equal (length fields-list) 1)
	 (is-instance (car fields-list)))
	((is-instance (car fields-list))
	 (is-instance-helper (cdr fields-list)))
	(T NIL)))


;;; Estrae da un'istanza il valore del campo specificato.
(defun field (instance field)
  (if (and (is-instance instance)
	   (listp instance)
	   (equal (length instance) 3)
	   (equal (car instance) 'oolinst))
      (if (field-in-fields-list (symbol-name field) (caddr instance))
	  (field-in-fields-list (symbol-name field) (caddr instance))
	  (error "unknown field."))
      (error "Instance expected as parameter!")))

;;; Estrae da un'istanza il valore di un campo, dopo aver percorso una
;;; lista di attributi.
(defun field* (instance &rest fields-name-list)
  (if (and (is-instance instance)
	   (listp instance)
	   (equal (length instance) 3)
	   (equal (car instance) 'oolinst))
      (if (not (equal (length fields-name-list) 0))
	  (cond ((equal (length fields-name-list) 1)
		 (field instance (car fields-name-list)))
		(T (apply #'field*
			  (field instance (car fields-name-list))
			  (cdr fields-name-list))))
	  (error "List of fields name is empty"))
      (error "Instance not valid!")))
				  
;;; Data una lista di parti, estrae tutti i campi.
(defun get-fields (parts)
  (cond ((null parts) '())
	((equal (caar parts) 'fields)
	 (append (recursive-validate-fields (cdar parts))
		 (get-fields (cdr parts))))
	(T (get-fields (cdr parts)))))

;;; Helper per scorrere una lista di campi, in modo da poterli processare
;;; e validare individualmente.
(defun recursive-validate-fields (fields)
  (if (null fields)
      '()
      (append (list (validate-field (car fields)))
	      (recursive-validate-fields (cdr fields)))))

;;; Valida un campo e lo riscrive in una forma standard, definita
;;; dall'estensione.
(defun validate-field (field)
  (cond ((equal (length field) 2)
	 (list (symbol-name (car field)) (make 'T (cadr field))))
	((equal (length field) 3)
	 (list (symbol-name (car field)) (make (caddr field) (cadr field))))))

;;; Date due liste di campi, i campi della prima lista ereditano i tipi
;;; dai campi della seconda lista.
(defun inherit-field-type (fields-list parents-fields-list)
  (cond ((null fields-list) '())
	((null parents-fields-list) fields-list)
	((and (equal (caar fields-list) (caar parents-fields-list))
	      (equal (cadadr (car fields-list)) 'T))
	 (if (is-instance (list 'oolinst
				(cadadr (car parents-fields-list))
				(caddr (cadr (car fields-list)))))
	     (let ((new-field
		    (append
		     (list (list (caar fields-list)
				 (list 'oolinst
				       (cadadr (car parents-fields-list))
				       (caddr (cadr (car fields-list))))))
		     (inherit-field-type (cdr fields-list)
					 (cdr parents-fields-list)))))
	       (if (is-instance new-field)
		   new-field
		   (error "Invalid value for a field")))
	     (error "Invalid value for a field.")))
	((equal (caar fields-list) (caar parents-fields-list))
	 (if (is-subtype-of (cadadr (car fields-list))
			    (cadadr (car parents-fields-list)))
	     (let ((new-field (append (list (car fields-list))
				      (inherit-field-type
				       (cdr fields-list)
				       (cdr parents-fields-list)))))
	       (if (is-instance new-field)
		   new-field
		   (error "Invalid value for a field")))
	     (error "A field type is supertype of an inherited one")))
	(T (append (inherit-field-type (list (car fields-list))
				       (cdr parents-fields-list))
		   (inherit-field-type (cdr fields-list)
				       parents-fields-list)))))

;;; Data una lista di parti, estrae tutti i metodi, dopo averli processati
;;; e aver definito le funzioni trampolino.
(defun get-methods (parts)
  (cond ((null parts) '())
	((equal (caar parts) 'methods)
	 (append (recursive-process-methods (cdar parts))
		 (get-methods (cdr parts))))
	(T (get-methods (cdr parts)))))

;;; Helper per scorrere una lista di metodi, in modo tale da poterli
;;; processare individualmente.
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

;;; Aggiorna i campi della lista con i nuovi valori passati come argomento.
(defun update-fields (old-fields args)
  (cond ((null args) old-fields)
	((field-in-fields-list (symbol-name (car args)) old-fields)
	 (update-fields (update-field old-fields
				      (list (car args) (cadr args)))
			(cddr args)))
	(T (error "Error when specifying fields to update."))))

;;; Estrae e aggiorna un singolo campo presente in una lista di campi.
(defun update-field (old-fields field)
  (cond ((null old-fields) (error "Field not found"))
	((equal (caar old-fields) (symbol-name (car field)))
	 (append (list (list (caar old-fields)
			     (make (cadr (cadar old-fields)) (cadr field))))
		 (cdr old-fields)))
	(T (append (list (car old-fields))
		   (update-field (cdr old-fields) field)))))

;;; Eredita, in maniera corretta, tutti i campi dalle classi parent.
(defun inherite-fields-from-parents (parents-list)
  (if (not (null parents-list))
      (append-fields-lists
       (append-fields-lists (class-spec-fields (car parents-list))
			    (inherite-fields-from-parents
			     (class-spec-parents (car parents-list))))
       (inherite-fields-from-parents (cdr parents-list)))
      '()))

;;; Metodo helper che permette di appendere due liste, in modo tale che alla
;;; prima lista vengano aggiunti tutti i campi della seconda lista
;;; non ancora presenti.
(defun append-fields-lists (list1 list2)
  (if (not (null list2))
      (if (not (field-in-fields-list (caar list2) list1))
	  (append-fields-lists (append list1 (list (car list2)))
			       (cdr list2))
	  (append-fields-lists list1 (cdr list2)))
      list1))

;;; Verifica se un tipo è sottotipo di un altro.
(defun is-subtype-of (subclass parent-class)
  (cond ((equal parent-class 'T) T)
	((equal subclass parent-class) T)
	((is-derivated-class (list subclass) parent-class) T)
	((and (not (is-class subclass))
	      (not (is-class parent-class))
	      (subtypep subclass parent-class))
	 T)))

;;; Verifica se almeno una classe presente nella lista di classi deriva
;;; dalla classe parent.
(defun is-derivated-class (derivated-classes parent-class)
  (cond ((null derivated-classes) NIL)
	((equal (car derivated-classes) parent-class) T)
	((is-derivated-class (class-spec-parents
			      (car derivated-classes))
			     parent-class) T)
	((is-derivated-class (cdr derivated-classes) parent-class) T)))

;;; Ritorna il migliore metodo di nome specificato data una lista di classi
;;; da cui si vuole ereditare il metodo.
(defun parents-method (parents-list method-name)
  (cond ((null parents-list) NIL)
	((method-in-methods-list method-name
				 (class-spec-methods (car parents-list))))
	((parents-method (class-spec-parents (car parents-list)) method-name))
	((parents-method (cdr parents-list) method-name))))

;;; Estrae un metodo da una lista di metodi.
(defun method-in-methods-list (method-name methods-list)
  (cond ((null methods-list) NIL)
	((equal method-name (caar methods-list)) (cadar methods-list))
	(T (method-in-methods-list method-name (cdr methods-list)))))

;;; Estrae un campo da una lista di campi.
(defun field-in-fields-list (field fields-list)
  (cond ((null fields-list) NIL)
	((equal field (caar fields-list))
	 (if (is-class (cadr (cadar fields-list)))
	     (cadar fields-list)
	     (caddr (cadar fields-list))))
	(T (field-in-fields-list field (cdr fields-list)))))
