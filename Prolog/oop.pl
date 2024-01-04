%%%% -*- Mode: Prolog -*-

%%%% Bolognini Mattia 870401
%%%% Nessuna collaborazione

class(object, [], [], []).
point_to(ptr, nullptr).
:- dynamic class/4.
:- dynamic point_to/2.

%%% Definisce una nuova classe e la inserisce nella base di conoscenza
def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).
def_class(ClassName, Parents, Parts) :-
    \+ class(ClassName, _, _, _),
    forall(member(Parent, Parents), class(Parent, _, _, _)),
    get_methods_and_fields(Parts, TmpFields, TmpMethods),
    standardize_fields(TmpFields, StandardFields),
    standardize_methods(TmpMethods, StandardMethods),
    inherit(Parents, InheritedFields, _),
    inherit_field_type(StandardFields, InheritedFields, FinalFields),
    assert_trampolines(StandardMethods),
    asserta(class(ClassName, Parents, FinalFields, StandardMethods)).

%%% Data una lista di metodi e campi, separa campi e metodi in due
%%% liste differenti
get_methods_and_fields([], [], []) :- !.
get_methods_and_fields([Field | Rest],
		       [Field | FieldsRest],
		       MethodsRest) :-
    Field =.. [field | _],
    !,
    get_methods_and_fields(Rest, FieldsRest, MethodsRest).
get_methods_and_fields([Method | Rest],
		       FieldsRest,
		       [Method | MethodsRest]) :-
    Method =.. [method | _],
    !,
    get_methods_and_fields(Rest, FieldsRest, MethodsRest).

%%% Riscrive i campi contenuti in una lista in una forma standard
%%% scelta in fase di implementazione, ovvero field(Nome, Oggetto)
standardize_fields([], []) :- !.
standardize_fields([field(Name, Value) | Rest],
		   [field(Name, Obj) | StandardRest]) :-
    \+ is_instance(Value),
    !,
    new(void, Value, Obj),
    standardize_fields(Rest, StandardRest).
standardize_fields([field(Name, Value, Type) | Rest],
		   [field(Name, Obj) | StandardRest]) :-
    \+ is_instance(Value),
    !,
    new(Type, Value, Obj),
    standardize_fields(Rest, StandardRest).
standardize_fields([field(Name, object(X, Y)) | Rest],
		   [field(Name, object(X, Y)) | StandardRest]) :-
    is_instance(X, Y),
    !,
    standardize_fields(Rest, StandardRest).
standardize_fields([field(Name, object(Type1, Value), Type2) | Rest],
		   [field(Name, object(Type2, Value)) | StandardRest]) :-
    is_instance(object(Type1, Value)),
    !,
    is_subtype_of(Type1, Type2),
    standardize_fields(Rest, StandardRest).

%%% Riscrive i metodi contenuti in una lista in una forma standard
%%% scelta in fase di implementazione, ovvero (Testa :- Corpo), dove
%%% a testa è stato aggiunto il parametro This e nel corpo il termine this
%%% è stato sostituito con This.
standardize_methods([], []) :- !.
standardize_methods([method(Name, Args, Body) | Rest],
		    [(StandardHead :- StandardBody) | StandardRest]) :-
    StandardHead =.. [Name, This | Args],
    replace(this, This, Body, StandardBody),
    standardize_methods(Rest, StandardRest).

%%% Dato in input un termine (anche composto), sostituisce un sotto-termine
%%% con un nuovo termine o variabile logica.
replace(_, _, Variable, Variable) :-
    var(Variable),
    !.
replace(Replacee, Replacement, OldAtom, Replacement) :-
    atomic(OldAtom),
    OldAtom = Replacee,
    !.
replace(Replacee, _, OldAtom, OldAtom) :-
    atomic(OldAtom),
    Replacee \= OldAtom,
    !.
replace(Replacee, Replacement, OldCompound, NewCompound) :-
    nonvar(OldCompound),
    compound(OldCompound),
    !,
    OldCompound =.. [Functor | OldArgs],
    maplist(replace(Replacee, Replacement), OldArgs, NewArgs),
    NewCompound =.. [Functor | NewArgs].

%%% Data una lista di campi, eredita il loro tipo da un'altra lista di campi.
inherit_field_type([], [], []) :- !.
inherit_field_type(Fields, [], Fields) :-
    Fields \= [],
    !.
inherit_field_type([], InheritedFields, []) :-
    InheritedFields \= [],
    !.
inherit_field_type([field(FieldName, object(void, Value1)) | Rest1],
		   [field(FieldName, object(Type, _)) | Rest2],
		   [field(FieldName, object(Type, Value1)) | Rest3]) :-
    !,
    inherit_field_type(Rest1, Rest2, Rest3).
inherit_field_type([field(FieldName, object(Type1, Value1)) | Rest1],
		   [field(FieldName, object(Type2, _)) | Rest2],
		   [field(FieldName, object(Type1, Value1)) | Rest3]) :-
    Type1 \= void,
    !,
    is_subtype_of(Type1, Type2),
    inherit_field_type(Rest1, Rest2, Rest3).
inherit_field_type([field(FieldName1, Obj1) | FieldsRest1],
		   [field(FieldName2, Obj2) | FieldsRest2],
		   Result) :-
    FieldName1 \= FieldName2,
    !,
    inherit_field_type([field(FieldName1, Obj1)], FieldsRest2, Tmp1),
    inherit_field_type(FieldsRest1,
		       [field(FieldName2, Obj2) | FieldsRest2],
		       Tmp2),
    append(Tmp1, Tmp2, Result).

%%% Istanzia un nuovo oggetto del tipo specificato,
%%% a cui viene assegnato un nome.
make(Name, Type) :-
    \+ point_to(Name, _),
    new(Type, Obj),
    asserta(point_to(Name, Obj)).
make(Name, Type, Params) :-
    \+ point_to(Name, _),
    new(Type, Params, Obj),
    asserta(point_to(Name, Obj)).

%%% Eredita, utilizzando un'ereditarietà depth-first left-most,
%%% tutti i campi e i metodi dalle classi specificate.
inherit([], [], []) :- !.
inherit([Parent | Rest], Fields, Methods) :-
    !,
    class(Parent, Grandpas, ParentFields, ParentMethods),
    inherit(Grandpas, GrandpasFields, GrandpasMethods),
    helper_append_fields(ParentFields, GrandpasFields, TmpFields),
    helper_append_methods(ParentMethods, GrandpasMethods, TmpMethods),
    inherit(Rest, RestFields, RestMethods),
    helper_append_fields(TmpFields, RestFields, Fields),
    helper_append_methods(TmpMethods, RestMethods, Methods).

%%% Appende due liste di campi. Nel caso sia presente un campo con lo stesso
%%% nome sia nella prima lista che nella seconda, il campo della seconda lista
%%% viene scartato.
helper_append_fields([], [], []) :- !.
helper_append_fields(X, [], X) :- X \= [], !.
helper_append_fields([], X, X) :- X \= [], !.
helper_append_fields([field(Name, Value) | Rest1],
		     [field(Name, _) | Rest2],
		     Result) :-
    !,
    helper_append_fields([field(Name, Value) | Rest1], Rest2, Result).
helper_append_fields([field(Name1, Value1) | Rest1],
		     [field(Name2, Value2) | Rest2],
		     Result) :-
    Name1 \= Name2,
    !,
    helper_append_fields(Rest1, [field(Name2, Value2)], Tmp1),
    helper_append_fields([field(Name1, Value1) | Tmp1], Rest2, Result).

%%% Appende due liste di metodi. Nel caso sia presente un metodo con lo stesso
%%% nome sia nella prima lista che nella seconda, il metodo della seconda
%%% lista viene scartato.
helper_append_methods([], [], []) :- !.
helper_append_methods(X, [], X) :- X \= [], !.
helper_append_methods([], X, X) :- X \= [], !.
helper_append_methods([(Head1 :- Body1) | Rest1],
		      [(Head2 :- _) | Rest2],
		      Result) :-
    Head1 =.. [MethodName | _],
    Head2 =.. [MethodName | _],
    !,
    helper_append_methods([(Head1 :- Body1) | Rest1], Rest2, Result).
helper_append_methods([(Head1 :- Body1) | Rest1],
		      [(Head2 :- Body2) | Rest2],
		      Result) :-
    Head1 =.. [MethodName1 | _],
    Head2 =.. [MethodName2 | _],
    MethodName1 \= MethodName2,
    !,
    helper_append_methods(Rest1, [(Head2 :- Body2)], Tmp1),
    helper_append_methods([(Head1 :- Body1) | Tmp1], Rest2, Result).

%%% Crea un nuovo oggetto del tipo specificato, inizializzato a un valore
%%% di default.
new(void, object(void, null)) :- !.
new(bool, object(bool, false)) :- !.
new(number, object(number, 0)) :- !.
new(float, object(float, 0.0)) :- !.
new(rational, object(rational, 0)) :- !.
new(atom, object(atom, '')) :- !.
new(string, object(string, "")) :- !.
new(integer, object(integer, 0)) :- !.
new(Type, object(Type, DefaultFields)) :-
    is_class(Type),
    !,
    inherit([Type], DefaultFields, _).

%%% Crea un nuovo oggetto del tipo specificato, con valore uguale a quello
%%% specificato (se accettabile).
new(void, X, object(void, X)) :- !.
new(bool, true, object(bool, true)) :- !.
new(bool, false, object(bool, false)) :- !.
new(number, X, object(number, X)) :- !, number(X).
new(float, X, object(float, X)) :- !, float(X).
new(rational, X, object(rational, X)) :- !, rational(X).
new(atom, X, object(atom, X)) :- !, atom(X).
new(string, X, object(string, X)) :- string(X), !.
new(string, X, object(string, X)) :- atom(X), !.
new(integer, X, object(integer, X)) :- !, integer(X).
new(Type, Params, object(Type, Fields)) :-
    is_class(Type),
    inherit([Type], DefaultFields, _),
    init_fields(DefaultFields, Params, Fields).

%%% Inizializza i campi con i valori passati come parametro.
%%% Se il campo è del tipo di una classe, allora il suo valore sarà una lista
%%% di campi.
init_fields(DefaultFields, [], DefaultFields) :- !.
init_fields(DefaultFields, [FirstParam | Rest], Fields) :-
    compound_name_arguments(FirstParam, =, [FieldName, NewValue]),
    delete(DefaultFields, field(FieldName, object(Type, _)), FieldsTmp),
    new(Type, NewValue, NewObj),
    init_fields([field(FieldName, NewObj) | FieldsTmp], Rest, Fields).

%%% Estrae il valore del campo di un'istanza. Se il campo è di tipo
%%% built-in verrà ritornato il suo valore, se invece è del tipo di una
%%% classe verrà ritornata una lista contenente tutti i campi
%%% dell'istanza di quella classe.
field(Ptr, Name, Value) :-
    inst(Ptr, Obj),
    !,
    field(Obj, Name, Value).
field(object(_, [field(Name, object(_, Value)) | _]), Name, Value) :- !.
field(object(_, [field(FieldName, _) | FieldsRest]), Name, Value) :-
    FieldName \= Name,
    !,
    field(object(_, FieldsRest), Name, Value).

%%% Restituisce il valore di un campo percorrendo una lista di campi.
fieldx(InstanceName, FieldNames, Result) :-
    inst(InstanceName, Obj),
    InstanceName \= object(_, _),
    !,
    fieldx(Obj, FieldNames,  Result).
fieldx(object(_, Value), FieldNames, Result) :-
    helper_fieldx(Value, FieldNames, Result).

%%% Date due liste, una contenente una lista di campi e una contenente
%%% una lista di nomi di campi, restituisce il valore di un campo percorrendo
%%% una lista di campi.
helper_fieldx(Value, [], Value) :- !.
helper_fieldx([field(FieldName, object(_, Value)) | _],
       [FieldName],
       Value) :- !.
helper_fieldx([field(FieldName1, _) | FieldsRest],
       [FieldName],
       Result) :-
    FieldName1 \= FieldName,
    !,
    helper_fieldx(FieldsRest, [FieldName], Result).
helper_fieldx(FieldsList, [FieldName | FieldNameRest], Result) :-
    FieldNameRest \= [],
    !,
    helper_fieldx(FieldsList, [FieldName], Tmp1),
    helper_fieldx(Tmp1, FieldNameRest, Result).

%%% Verifica se è stata definita una classe con un certo nome.
is_class(Name) :- class(Name, _, _, _).

%%% Dato un nome, ritorna l'istanza con quel nome.
inst(Ptr, Obj) :- point_to(Ptr, Obj).

%%% Verifica se l'oggetto è istanza di un certo tipo.
is_instance(object(Type1, Value), Type2) :-
    is_subtype_of(Type1, Type2),
    is_instance(object(Type1, Value)).

%%% Verifica se l'istanza è valida.
is_instance(object(void, _)) :- !.
is_instance(object(number, X)) :- !, number(X).
is_instance(object(rational, X)) :- !, rational(X).
is_instance(object(atom, X)) :- !, atom(X).
is_instance(object(bool, true)) :- !.
is_instance(object(bool, false)) :- !.
is_instance(object(float, X)) :- !, float(X).
is_instance(object(string, X)) :- string(X), !.
is_instance(object(string, X)) :- atom(X), !.
is_instance(object(integer, X)) :- !, integer(X).
is_instance(object(Type, Fields)) :-
    \+ is_builtin(Type),
    !,
    class(Type, _, _, _),
    inherit([Type], ClassFields, _),
    forall(member(field(Name, _), Fields),
	   member(field(Name, _), ClassFields)).

%%% Verifica se un tipo è sottotipo di un altro.
is_subtype_of(X, X) :- !.
is_subtype_of(float, number) :- !.
is_subtype_of(rational, float) :- !.
is_subtype_of(integer, rational) :- !.
is_subtype_of(string, atom) :- !.
is_subtype_of(Class, ParentClass) :-
    class(Class, Parents, _, _),
    !,
    helper_is_subtype_of(Parents, ParentClass).

%%% Helper usato per scorrere le liste di parent.
helper_is_subtype_of([X | _], Class) :-
    is_subtype_of(X, Class),
    !.
helper_is_subtype_of([X | Xs], Class) :-
    \+ is_subtype_of(X, Class),
    !,
    helper_is_subtype_of(Xs, Class).

%%% Verifica se un tipo è built-int
is_builtin(void).
is_builtin(bool).
is_builtin(atom).
is_builtin(string).
is_builtin(number).
is_builtin(float).
is_builtin(rational).
is_builtin(integer).

%%% Aggiunge alla base di conoscenza un predicato trampolino per
%%% richiamare correttamente i metodi di una classe.
assert_trampolines([]).
assert_trampolines([(Head :- _) | Rest]) :-
    Head =.. [Name | Args],
    functor(Head, Name,  Arity),
    abolish(Name, Arity),
    asserta((Head :- invoke_method(Name, Args))),
    assert_trampolines(Rest).

%%% Esegue il metodo di un'istanza.
invoke_method(Name, [Ptr | Args]) :-
    inst(Ptr, Instance),
    !,
    invoke_method(Name, [Instance | Args]).
invoke_method(Name, [object(Type, Value) | Args]) :-
    get_best_method([Type], Name, (Head :- Body)),
    asserta((Head :- Body)),
    CalledHead =.. [Name | [object(Type, Value) | Args]],
    !,
    call(CalledHead), !,      %%% questo è un red cut
    retract(Head :- Body), !.

%%% Sceglie il migliore metodo da eseguire data una gerarchia di classi.
%%% Per ereditare i metodi viene utilizzata un'ereditarietà depth-first
%%% left-most.
get_best_method([Class], MethodName, BestMethod) :-
    class(Class, _, _, MethodsList),
    get_method_from_methods_list(MethodName, MethodsList, BestMethod),
    !.
get_best_method([Class], MethodName, BestMethod) :-
    class(Class, Parents, _,  MethodsList),
    \+ get_method_from_methods_list(MethodName, MethodsList, _),
    !,
    get_best_method(Parents, MethodName, BestMethod).
get_best_method([Class | Classes], MethodName, BestMethod) :-
    Classes \= [],
    get_best_method([Class], MethodName, BestMethod),
    !.
get_best_method([Class | Classes], MethodName, BestMethod) :-
    Classes \= [],
    \+ get_best_method([Class], MethodName, _),
    class(Class, Parents, _, _),
    get_best_method(Parents, MethodName, BestMethod),
    !.
get_best_method([Class | Classes], MethodName, BestMethod) :-
    Classes \= [],
    \+ get_best_method([Class], MethodName, _),
    class(Class, Parents, _, _),
    \+ get_best_method(Parents, MethodName, _),
    get_best_method(Classes, MethodName, BestMethod),
    !.

%%% Ottiene il metodo di nome specificato da una lista di metodi.
get_method_from_methods_list(MethodName,
			     [(Head :- Body) | _],
			     (Head :- Body)) :-
    Head =.. [MethodName | _].
get_method_from_methods_list(MethodName,
			     [(Head :- _) | Rest],
			     Result) :-
    Head =.. [Functor | _],
    Functor \= MethodName,
    !,
    get_method_from_methods_list(MethodName, Rest, Result).
    

