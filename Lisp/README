# OOΛ in Common Lisp

OOΛ è un'estensione object-oriented per il linguaggio Common Lisp.
Permette di definire classi, istanziare oggetti di classi user-defined e accedere ai valori dei campi di un'istanza

## Primitive
### DEF-CLASS
def-class permette di definire una nuova classe, che verrà poi salvata  in una hash table.

Sintassi:
'(' def-class <class-name> <parents> <part>* ')'.

Ritorna il nome della classe se la sua costruzione è andata a buon fine.

### MAKE
make permette di creare un'istanza della classe specificata, eventualmente inizializzando i campi con i valori specificati.

Sintassi: '(' make <class-name> [<field-name> <value>]* ')'.

Ritorna una lista rappresentante l'istanza. Se non si vuole perdere l'accesso all'istanza, è necessario salvare la lista in una locazione, solitamente con def-parameter.

### IS-CLASS
Verifica se il simbolo specificato è il nome di una classe.

Sintassi: '(' is-class <class-name> ')'

Ritorna true se <class-name> è una classe, false altrimenti.

### IS-INSTANCE
Verifica se un oggetto è istanza di una determinata classe. Se non viene specificata una classe, allora essa sarà considerata T.
Controlla anche che l'istanza sia valida.

Sintassi: '(' is-instance <value> [<class-name>] ')'

Ritorna true se l'oggetto è valido e istanza della classe specificata, false altrimenti.

### FIELD
Estrae il valore del campo specificato da un'istanza.

Sintassi: '(' field <instance> <field-name> ')'

Ritorna il valore del campo specificato.

### FIELD*
Estrae il valore di un campo di una classe percorrendo una lista di attributi.

Sintassi: '(' field* <instance> <field-name>+ ')'

Ritorna il valore dell'ultimo campo della lista di nomi di campi.

## ALTRE FUNZIONI
### PARENTS-EXIST
Data una lista di classi parent, verifica che tutti gli elementi della lista siano delle classi già definite.

Sintassi: '(' parents-exist <parents-list> ')'

Ritorna true se tutti gli elementi della lista sono classi già definite, false altrimenti.

### GET-FIELDS
Data una lista di parti (metodi + campi), estrae tutti i metodi, dopo averli riscritti in una forma standard definita dall'estensione.

Sintassi: '(' get-fields <parts-list> ')'

Ritorna una lista di campi standardizzati.

### RECURSIVE-VALIDATE-FIELDS
Utilizzata come funzione helper da get-fields.
Data una lista di campi non standardizzati, si occupa di riscriverli in una forma standardizzata.

Sintassi: '(' recursive-validate-fields <fields-list> ')'

Ritorna una lista di campi standardizzati.

### VALIDATE-FIELD
Utilizzata come funzione helper da recursive-validate-fields.
Dato un campo non standardizzato, lo riscrive in una forma standard, dopo aver controllato la sua validità.

Sintassi: '(' validate-field <field> ')'

Ritorna il campo riscritto in una forma standard.

### INHERIT-FIELD-TYPE
Data una lista di campi standardizzati e una lista di campi standardizzati ereditati, eredita i tipi dei campi.
Un campo non può definire un tipo pià ampio dello stesso campo ereditato.

Sintassi: '(' inherit-field-type <fields-list> <parents-fields-list> ')'

Ritorna la lista di campi riscritta in modo da aver ereditato i tipi dalla lista di campi parent.

### GET-METHODS
Data una lista di parti (metodi + campi) estrae tutti i metodi, dopo aver creato le funzioni trampolino e averli riscritti come funzioni lambda.

Sintassi: '(' get-methods <parts-list> ')'

Ritorna una lista contenente i nomi dei metodi e le rispettive funzioni lambda.

### RECURSIVE-PROCESS-METHODS
Usata come funzione helper da get-methods.
Data una lista di metodi, processa ogni metodo.

Sintassi: '(' recursive-process-methods <methods-list> ')'

Ritorna una lista contenente i nomi dei metodi e le rispettive funzioni lambda associate.

### PROCESS-METHOD
Usata come funzione helper da recursive-process-methods.
Dato un metodo, crea la sua funzione trampolino e lo riscrive come una funzione lambda.

Sintassi: '(' process-method <method-name> <method-spec> ')'

Ritorna una lista di due elementi, il cui primo elemento è il nome del metodo e il secondo e la funzione lambda associata.

### REWRITE-METHOD-CODE
Usata come funzione helper da process-method.
Riscrive un metodo come una funzione lambda, a cui viene aggiunto come parametro this.

Sintassi: '('rewrite-method-code <method-name> <method-spec> ')'

Ritorna una lista di due elementi, il cui primo elemento è il nome del metodo e il secondo e la funzione lambda associata.

### INVOKE-METHOD
Funzione eseguita quando viene chiamato un trampolino.
Si occupa di selezionare il migliore metodo da eseguire per una certa istanza, tenendo conto dell'ereditarietà.

Sintassi: '(' invoke-method <instance> <method-name> ')'

Ritorna la funzione lambda valuata come migliore.

### UPDATE-FIELDS
Usata come funzione helper da make.
Si occupa di inizializzare i campi di un'istanza con i valori specificati alla chiamata di make.

Sintassi: '(' update-fields <fields-list> [<field-name> <initializer-value>]* ')'

Ritorna la lista dei campi di un'istanza inizializzati con i valori specificati.

### UPDATE-FIELD
Usata come funzione helper da update-fields.
Estrae un campo da una lista di campi e lo inizializza al valore specificato.

Sintassi: '(' update-field <fields-list> (<field-name> <initializer-value>) ')'

Ritorna la lista di campi dopo aver inizializzato il campo specificato.

### INHERITE-FIELDS-FROM-PARENTS
Eredita correttamente tutti i campi da una lista di classi parent.

Sintassi: '(' inherite-fields-from-parents <parents-list> ')'

Ritorna una lista contenente tutti i campi ereditati.

### APPEND-FIELDS-LISTS
Usato come funzione helper per l'ereditarietà.
Date due liste di campi, aggiuge alla prima lista tutti i campi della seconda lista che non sono già presenti.

Sintassi: '(' append-fields-lists <first-fields-list> <second-fields-list> ')'

### IS-SUBTYPE-OF
Verifica che un tipo è sottotipo di un altro.

Sintassi: '(' is-subtype-of <subtype> <type> ')'

Ritorna true se <subtype> è sottotipo di <type>, false altrimenti.

### IS-DERIVATED-CLASS
Usata come funzione helper da is-subtype-of.
Data una lista di classi, verifica che almeno una di esse sia derivata dalla classe parent.

Sintassi: '(' is-derivated-class <classes-list> <parent-class> ')'

Ritorna true se almeno una classe discende dalla classe parent, false altrimenti.

### PARENTS-METHOD
Data una lista di classi parent, estrae il miglior metodo che si vuole richiamare.

Sintassi: '(' parents-method <parents-list> <method-name> ')'

Ritorna la funzione lambda del metodo richiamato.

### METHOD-IN-METHODS-LIST
Estrae da una lista di metodi il metodo di nome specificato, se presente.

Sintassi: '(' method-in-methods-list <method-name> <methods-list> ')'

Ritorna una lista di due elementi, il cui primo elemento è il nome del metodo e il secondo la funzione lambda associata.

### FIELD-IN-FIELDS-LIST
Estrae da una lista di campi il campo di nome specificato, se presente.

Sintassi: '(' field-in-fields-list <field-name> <fields-list> ')'

Ritorna il campo estratto dalla lista, se presente.
