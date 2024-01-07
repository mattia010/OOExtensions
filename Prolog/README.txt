Bolognini Mattia 870401
Nessuna collaborazione.

## Primitive
### DEF_CLASS/2
Definisce una nuova classe senza campi e metodi e la asserisce nella base di conoscenza.

Sintassi: def_class(ClassName, ParentsList).

### DEF_CLASS/3
Definisce una nuova classe e la asserisce nella base di conoscenza.

Sintassi: def_class(ClassName, ParentsList, PartsList).

### MAKE/2
Crea, se non esiste, un istanza della classe specificata inizializzando i campi ai loro valori di default e le assegna il nome specificato.

Sintassi: make(Name, ClassType).

### MAKE/3
Crea, se non esiste, un istanza della classe specificata inizializzando i campi ai valori specificati e le assegna il nome specificato.

Sintassi: make(Name, ClassType, InitializerFieldsList).

### IS_CLASS/1
Verifica se il parametro è una classe già definita.

Sintassi: is_class(Name).

### IS_INSTANCE/2
Verifica se l'oggetto è valido e istanza della classe specificata.

Sintassi: is_instance(Instance, Class).

### IS_INSTANCE/1
Verifica se l'oggetto è un'istanza valida.

Sintassi: is_instance(Instance).

### INST/2
Ritorna l'istanza identificata dal nome specificato.

Sintassi: inst(Name, Instance).

### FIELD/3
Estrae da un'istanza il valore del campo specificato.

Sintassi: field(Instance, FieldName, Value).

### FIELDX/3
Estrae il valore di un campo percorrendo una lista di attributi.

Sintassi: fieldx(Instance, FieldsNameList, Value).

## ALTRI PREDICATI
### GET_METHODS_AND_FIELDS/3
Data una lista di parti, estrae tutti i campi e tutti i metodi in due liste separate.

Sintassi: get_methods_and_fields(PartsList, FieldsList, MethodsList).

### STANDARDIZE_FIELDS/2
Riscrive i campi in una forma standard definita dall'estensione.

Sintassi: standardize_fields(FieldsList, StandardizedFieldsList).

### STANDARDIZE_METHODS/2
Riscrive i metodi in modo da aggiungere il parametro This e sostituire il termine this nel corpo con la variabile
logica This.

Sintassi: standardize_fields(MethodsList, StandardizeMethodsList).

### REPLACE/4
Dato in input un termine, una variabile logica o termine composto, sostituisce un termine presente al suo interno
con un altro termine o variabile logica.

Sintassi: replace(Replacee, Replacement, OldTerm, NewTerm).

### INHERIT_FIELD_TYPE/3
Data una lista di campi, eredita il loro tipo da un'altra lista di campi.

Sintassi: inherit_field_type(FieldsList1, FieldsList2, InheritedFieldsList).

### INHERIT/3
Data una lista di classi parent, eredita tutti i campi e tutti i metodi in due liste separate.

Sintassi: inherit(Classes, Fields, Methods).

### HELPER_APPEND_FIELDS/3
Aggiunge alla prima lista tutti campi della seconda lista che non sono già presenti.

Sintassi: helper_append_fields(List1, List2, Result).

### HELPER_APPEND_METHODS/3
Aggiunge alla prima lista tutti i metodi della seconda lista che non sono già presenti.

Sintassi: helper_append_methods(List1, List2, Result).

### NEW/2
Crea un nuovo oggetto del tipo specificato e lo inizializza a un valore di default.

Sintassi: new(Type, Object).

### NEW/3
Crea un nuovo oggetto del tipo specificato e lo inizializza al valore specificato. Se è una classe inizializza i campi.

Sintassi: new(Type, Args, Object).

### INIT_FIELDS/3
Inizializza i campi contenuti in una lista con i valori specificati in una seconda lista.

Sintassi: init_fields(Fields, Args, InitializedFields).

### IS_SUBTYPE_OF/2
Verifica se un tipo è sottotipo di un altro.

Sintassi: is_subtype_of(Subtype, Supertype).

### IS_BUILTIN/1
Verifica se il parametro è un tipo built-in.

Sintassi: is_builtin(Type).

### ASSERT_TRAMPOLINES/1
Asserisce nella base di conoscenza i predicati trampolini dei metodi contenuti nella lista.

Sintassi: assert_trampolines(MethodsList).

### INVOKE_METHOD/2
Predicato invocato quando viene eseguito un trampolino.
Si occupa di scegliere il miglior metodo da eseguire e di eseguirlo con gli argomenti specificati, di cui il primo
è l'istanza su cui viene chiamato.

Sintassi: invoke_method(MethodName, [Instance | Args]).

### GET_BEST_METHOD/3
Data una lista di classi e un nome di metodo, sceglie il metodo migliore da eseguire della lista di classi, implementando un'ereditarietà depth-first left-most.

Sintassi: get_best_method(ClassesList, MethodName, BestMethod).

### GET_METHOD_FROM_METHODS_LIST/3
Estrae da una lista di metodi il metodo di nome specificato.

Sintassi: get_method_from_methods_list(MethodName, MethodsList, Method).
