%%%% -*- Mode: Prolog -*-

%%%% Bolognini Mattia 870401
%%%% Nessuna collaborazione



%%% TERMINI COMPOSTI UTILIZZATI %%%

% Classi = class(ClassName, FieldsList, MethodsList),

% Metodi = method(Name, ArgsList, Body).

% Campi = field(Name, Object).

% Oggetti = object(Type, Value) per i tipi builtin.
%           object(Type, FieldsList) per i tipi userdefined (classi).
% Type void definisce un oggetto non tipizzato.
% Un oggetto deve avere sempre un valore iniziale,
% non può rimanere indefinito.

% Puntatori a oggetti = point_to(PtrName, Object).
% Un puntatore point_to(PtrName, nullptr) è un puntatore che non punta a
% nessun oggetto.

% Ereditarietà tra classi = child_of(ChildName, ParentName).

%SISTEMARE STRING

class(object, [], []).
point_to(ptr, nullptr).
:- dynamic class/3.
:- dynamic point_to/2.

% Definisce una nuova classe, ovvero crea fatti e regole per esprimere
% relazioni di ereditarietà, metodi e campi della classe.
def_class(ClassName, Parents) :-
    forall(member(Parent, Parents), class(Parent, _, _)),
    forall(member(Parent, Parents), asserta(child_of(ClassName, Parent))).
def_class(ClassName, Parents, Parts) :-
    \+ class(ClassName, _, _),
    forall(member(Parent, Parents), class(Parent, _, _)),
    forall(member(Parent, Parents),
	   asserta(child_of(ClassName, Parent))),
    get_methods(Parts, Methods),
    get_fields(Parts, Fields),
    inherit(Methods, Fields, Parents, AllMethods, AllFields),
    forall(member(Method, AllMethods),
	   assert_method(ClassName, Method)),
    asserta(class(ClassName, AllFields, AllMethods)).


inherit(Methods, Fields, [], Methods, Fields).
inherit(Methods, Fields, [Parent | Rest], AllMethods, AllFields) :-
    class(Parent, ParentFields, ParentMethods),
    inherit(Methods, Fields, Rest, RestMethods, RestFields),
    append(ParentFields, RestFields, AllFields),
    append(ParentMethods, RestMethods, AllMethods).

get_methods([], []).
get_methods([method(Name, Params, Body) | Rest],
	    [method(Name, Params, Body) | MethodRest]) :-
    get_methods(Rest, MethodRest).
get_methods([field(_, _, _) | Rest], Result) :-
    get_methods(Rest, Result).
get_methods([field(_, _) | Rest], Result) :-
    get_methods(Rest, Result).

get_fields([], []).
get_fields([field(Name, Value) | Rest], Result) :-
    get_fields([field(Name, Value, void) | Rest], Result).
get_fields([field(Name, Value, Type) | Rest],
	   [field(Name, Object) | FieldsRest]) :-
    new(Type, Value, Object),
    get_fields(Rest, FieldsRest).
get_fields([method(_, _, _) | Rest], Result) :-
    get_fields(Rest, Result).

assert_method(ClassName, method(MethodName, Parameters, BadBody)) :-
    replace(this, This, BadBody, GoodBody),
    Head =.. [MethodName, This | Parameters],
    asserta((Head :- is_instance(This, ClassName), call(GoodBody))).

%replace(Replacee, Replacement, OldTerm, NewTerm) :-
%        (   OldTerm == Replacee -> NewTerm = Replacement
%        ;   var(OldTerm) -> NewTerm = OldTerm
%        ;   OldTerm =.. [Functor | OldClauses],
%            maplist(replace(Replacee, Replacement), OldClauses, NewClauses),
%            NewTerm =.. [Functor | NewClauses]
%        ).

replace(Replacee, Replacement, Replacee, Replacement) :- !.
replace(_, _, VariableTerm, VariableTerm) :-
    var(VariableTerm),
    !.
replace(Replacee, Replacement, OldTerm, NewTerm) :-
    OldTerm =.. [Functor | OldClauses],
    maplist(replace(Replacee, Replacement), OldClauses, NewClauses),
    NewTerm =.. [Functor | NewClauses].


retract_class(ClassName) :-
    class(ClassName, _, _),
    retract(class(ClassName, _, _)).
    %MANCA DA FARE RETRACT DEI METODI DI UNA CLASSE.


% Istanzia un oggetto e crea un puntatore ad esso. OK
make(Name, Type) :-
    make(Name, Type, []).
make(Name, Type, Params) :-
    \+ point_to(Name, _),
    new(Type, Params, Obj),
    asserta(point_to(Name, Obj)).


% Crea un oggetto del tipo specificato.
new(void, X, object(void, X)) :- !.
new(bool, true, object(bool, true)) :- !.
new(bool, false, object(bool, false)) :- !.
new(float, X, object(float, X)) :- !, float(X).
new(string, X, object(string, X)) :- !, string(X).
new(integer, X, object(integer, X)) :- !, integer(X).
new(Type, Params, object(Type, Fields)) :-
    \+ is_builtin(Type),
    !,
    class(Type, DefaultFields, _),
    init_fields(DefaultFields, Params, Fields).

% Inizializza i campi nella costruzione di un oggetto di una classe. OK
init_fields(DefaultFields, [], DefaultFields).
init_fields(DefaultFields, [FirstParam | Rest], Fields) :-
    compound_name_arguments(FirstParam, =, [FieldName, NewValue]),
    member(field(FieldName, object(Type, _)), DefaultFields),
    is_instance(object(Type, NewValue)),
    delete(DefaultFields, field(FieldName, _), FieldsTmp),
    init_fields([field(FieldName, object(Type, NewValue)) | FieldsTmp],
		Rest,
		Fields).

% Distrugge un'oggetto e il puntatore.  OK
free(Ptr) :-
    point_to(Ptr, _),
    retract(point_to(Ptr, _)).


% Ottiene il valore del campo dell'istanza di una classe.   OK
field(Ptr, Name, Value) :-
    point_to(Ptr, object(Type, Fields)),
    is_class(Type),
    member(field(Name, object(_, Value)), Fields).

% Aggiorna un campo di un'istanza di una classe con un nuovo valore.  OK
set_field(Ptr, Name, Value) :-
    point_to(Ptr, object(Type, OldFields)),
    is_class(Type),
    member(field(Name, _), OldFields),
    delete(OldFields, field(Name, object(FieldType, _)), Tmp),
    is_instance(object(FieldType, Value)),
    retract(point_to(Ptr, _)),
    asserta(
	point_to(Ptr,
		 object(Type,
			[field(Name,
			       object(FieldType, Value)) | Tmp]))).
    

% Verifica l'esistenza di una classe.
is_class(Type) :- class(Type, _, _).

% Ritorna l'oggetto puntato da un puntatore.
inst(Ptr, Obj) :- point_to(Ptr, Obj).

% Ritorna il tipo di un oggetto. Può verificare se un oggetto è istanza di
% un certo tipo.
is_instance(object(Type, Value), Type) :-
    is_instance(object(Type, Value)).


is_instance(object(void, _)) :- !.
is_instance(object(bool, true)) :- !.
is_instance(object(bool, false)) :- !.
is_instance(object(float, X)) :- !, float(X).
is_instance(object(string, X)) :- !, string(X).
is_instance(object(integer, X)) :- !, integer(X).
is_instance(object(Type, Fields)) :-
    \+ is_builtin(Type),
    !,
    class(Type, ClassFields, _),
    forall(member(field(Name, _), Fields),
	   member(field(Name, _), ClassFields)).

is_builtin(bool).
is_builtin(void).
is_builtin(float).
is_builtin(string).
is_builtin(integer).


%%% TESTS
:- begin_tests(is_instance).

test(is_instance_bool_true) :-
    % manca il test per is_instance con classi.
    is_instance(object(bool, true)).
test(not_is_instance_bool) :-
    \+ is_instance(object(bool, cur)).
test(is_instance_bool_false) :-
    is_instance(object(bool, false)).
test(is_instance_string1) :-
    is_instance(object(string, "cduri")).
test(is_instance_string2) :-
    is_instance(object(string, "a")).
test(is_instance_integer1) :-
    is_instance(object(integer, 234)).
test(is_instance_integer2) :-
    is_instance(object(integer, 34)).
test(not_is_instance_integer1) :-
    \+ is_instance(object(integer, as)).
test(not_is_instance_integer2) :-
    \+ is_instance(object(integer, 34.45)).
test(is_instance_float1) :-
    is_instance(object(float, 3.4)).
test(is_instance_float2) :-
    is_instance(object(float, 4.0)).
test(is_not_instance_float) :-
    \+ is_instance(object(float, e)).
test(is_instance_void1) :-
    is_instance(object(void, 45)).
test(is_instance_void2) :-
    is_instance(object(void, as)).

:- end_tests(is_instance).


:- begin_tests(instances).

test(def_class_only_fields) :-
    def_class(astudent, [],[field(name, 'Eva Lu Ator' ),
			   field(university, 'Berkeley')]),
    retract_class(astudent).

test(def_class_with_method) :-
    def_class(bstudent, [],[field(name, 'Eva Lu Ator' ),
				 field(university, 'Berkeley'),
				 method(talk, [],
					(write('My name is '),
					 field(this, name, N),
					 writeln(N),
					 write('My age is '),
					 field(this, age, A),
					 writeln(A)))
			  ]),
    retract_class(bstudent).

test(make_instance) :-
    def_class(cstudent, [],[field(name, 'Eva Lu Ator' ),
				 field(university, 'Berkeley'),
				 method(talk, [],
					(write('My name is '),
					 field(this, name, N),
					 writeln(N),
					 write('My age is '),
					 field(this, age, A),
					 writeln(A)))
				]),
    make(s1, cstudent, [name = 108]),
    free(s1),
    retract_class(cstudent).
:- end_tests(instances).



:- begin_tests(inheritance).

test(inheritance_1) :-
    def_class(person, [], [field(name, 'Eve'), field(age, 21, integer)]),
    \+ def_class(person, [], [field(name, 'Eve'), field(age, 21, integer)]),
    def_class(student, [person], [field(name, 'Eva Lu Ator'),
				  field(university, 'Berkeley'),
				  method(talk, [],
					 (write('My name is '),
					  field(this, name, N),
					  writeln(N),
					  write('My age is '),
					  field(this, age, A),
					  writeln(A)))]),
    class(student, _, _),
    class(person, _, _),
    make(eve, person),
    make(adam, person, [name = 'Adam']),
    make(s1, student, [name = 'Eduardo De Filippo', age = 108]),
    make(s2, student),
    point_to(s2, _),
    point_to(s1, object(student, _)),
    \+ make(s3, student, [name = 'Harry Potter', age = "12"]),
    \+ point_to(s3, _),
    field(eve, age, 21),
    field(s1, age, 108),
    field(s2, name, 'Eva Lu Ator'),
    \+ field(eve, age, 234),
    free(eve),
    free(adam),
    free(s1),
    free(s2),
    retract_class(person),
    retract_class(student).

:- end_tests(inheritance).
