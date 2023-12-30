%%%% -*- Mode: Prolog -*-

%%%% Bolognini Mattia 870401
%%%% Nessuna collaborazione

% DA FINIRE: COMPLETARE SOLO FUNZIONI TRAMPOLINO PER LA CHIAMATA AI METODI
% E GESTIRE CORRETTAMENTE LE CHIAMATE AI METODI.
% assert_trampolines, invoke_method, get_best_method,
% get_method_from_methods_list


class(object, [], [], []).
point_to(ptr, nullptr).
:- dynamic class/4.
:- dynamic point_to/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% PREDICATI PER LA DEFINIZIONE DI UNA CLASSE %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).
def_class(ClassName, Parents, Parts) :-
    \+ class(ClassName, _, _, _),
    forall(member(Parent, Parents), class(Parent, _, _, _)),
    get_methods(Parts, TmpMethods),
    get_fields(Parts, TmpFields),
    standardize_fields(TmpFields, StandardFields),
    standardize_methods(TmpMethods, StandardMethods),
    assert_trampolines(StandardMethods),
    asserta(class(ClassName, Parents, StandardFields, StandardMethods)).

get_methods([], []) :- !.
get_methods([Field | Rest], Result) :-
    Field =.. [field | _],
    !,
    get_methods(Rest, Result).
get_methods([Method | Rest], [Method | Tmp]) :-
    Method =.. [method | _],
    !,
    get_methods(Rest, Tmp).

get_fields([], []) :- !.
get_fields([Method | Rest], Result) :-
    Method =.. [method | _],
    !,
    get_fields(Rest, Result).
get_fields([Field | Rest], [Field | Tmp]) :-
    Field =.. [field | _],
    !,
    get_fields(Rest, Tmp).


% OK TESTATO
standardize_fields([], []) :- !.
standardize_fields([field(Name, Value) | Rest],
		   [field(Name, Obj) | StandardRest]) :-
    !,
    new(void, Value, Obj),
    standardize_fields(Rest, StandardRest).
standardize_fields([field(Name, Value, Type) | Rest],
		   [field(Name, Obj) | StandardRest]) :-
    !,
    new(Type, Value, Obj),
    standardize_fields(Rest, StandardRest).

standardize_methods([], []) :- !.
standardize_methods([method(Name, Args, Body) | Rest],
		    [(StandardHead :- StandardBody) | StandardRest]) :-
    StandardHead =.. [Name, This | Args],
    replace(this, This, Body, StandardBody),
    standardize_methods(Rest, StandardRest).

replace(Replacee, Replacement, Replacee, Replacement) :- !.
replace(_, _, VariableTerm, VariableTerm) :- var(VariableTerm), !.
replace(Replacee, Replacement, OldTerm, NewTerm) :-
    OldTerm =.. [Functor | OldClauses],
    maplist(replace(Replacee, Replacement), OldClauses, NewClauses),
    NewTerm =.. [Functor | NewClauses].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% PREDICATI PER L'ISTANZIAZIONE %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Istanzia un oggetto e aggiunge alla base di conoscenza un puntatore ad esso.
make(Name, Type) :-
    \+ point_to(Name, _),
    new(Type, Obj),
    asserta(point_to(Name, Obj)).
make(Name, Type, Params) :-
    \+ point_to(Name, _),
    new(Type, Params, Obj),
    asserta(point_to(Name, Obj)).

% Eredita campi e metodi, implementando un'ereditarietà Depth-first left-most.
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

% DA FARE
helper_append_methods([], [], []) :- !.
helper_append_methods(X, [], X) :- X \= [], !.
helper_append_methods([], X, X) :- X \= [], !.
helper_append_methods([(Head1 :- Body1) | Rest1],
		      [(Head2 :- _) | Rest2],
		      [(Head1 :- Body1) | Rest3]) :-
    Head1 =.. [MethodName | _],
    Head2 =.. [MethodName | _],
    !,
    helper_append_methods([(Head1 :- Body1) | Rest1], Rest2, Rest3).
helper_append_methods([(Head1 :- Body1) | Rest1],
		      [(Head2 :- Body2) | Rest2],
		      Result) :-
    Head1 =.. [MethodName1 | _],
    Head2 =.. [MethodName2 | _],
    MethodName1 \= MethodName2,
    !,
    helper_append_methods(Rest1, [(Head2 :- Body2)], Tmp1),
    helper_append_methods([(Head1 :- Body1) | Tmp1], Rest2, Result).

% Crea un oggetto del tipo specificato.
new(void, object(void, null)) :- !.
new(bool, object(bool, false)) :- !.
new(float, object(float, 0.0)) :- !.
new(string, object(string, "")) :- !.
new(integer, object(integer, 0)) :- !.
new(Type, object(Type, DefaultFields)) :-
    is_class(Type),
    !,
    inherit([Type], DefaultFields, _).

new(void, X, object(void, X)) :- !.
new(bool, true, object(bool, true)) :- !.
new(bool, false, object(bool, false)) :- !.
new(float, X, object(float, X)) :- !, float(X).
new(string, X, object(string, X)) :- !, string(X).
new(integer, X, object(integer, X)) :- !, integer(X).
new(Type, Params, object(Type, Fields)) :-
    is_class(Type),
    inherit([Type], DefaultFields, _),
    init_fields(DefaultFields, Params, Fields).

% Inizializza i campi nella costruzione di un oggetto di una classe. OK
init_fields(DefaultFields, [], DefaultFields) :- !.
init_fields(DefaultFields, [FirstParam | Rest], Fields) :-
    !,
    compound_name_arguments(FirstParam, =, [FieldName, NewValue]),
    member(field(FieldName, object(Type, _)), DefaultFields),
    is_instance(object(Type, NewValue)),
    delete(DefaultFields, field(FieldName, _), FieldsTmp),
    init_fields([field(FieldName, object(Type, NewValue)) | FieldsTmp],
		Rest,
		Fields).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% PREDICATI UTILS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

% Distrugge un'oggetto e il puntatore ad esso (solo se presenti),
% rimuovendoli dalla base di conoscenza.
free(Ptr) :-
    point_to(Ptr, _),
    retract(point_to(Ptr, _)).

% DA SISTEMARE PER L'EREDITARIETA
% Ottiene il valore del campo dell'istanza di una classe.   OK
field(Ptr, Name, Value) :-
    inst(Ptr, Obj),
    !,
    field(Obj, Name, Value).
field(object(_, [field(Name, object(_, Value)) | _]), Name, Value) :- !.
field(object(_, [field(FieldName, _) | FieldsRest]), Name, Value) :-
    FieldName \= Name,
    !,
    field(object(_, FieldsRest), Name, Value).


% Verifica l'esistenza di una classe.
is_class(Type) :- class(Type, _, _, _).

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

retract_class(ClassName) :-
    class(ClassName, _, _, _),
    retract(class(ClassName, _, _, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% DA RIFINIRE MA FUNZIONANTI %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

assert_trampolines([]).
assert_trampolines([(Head :- _) | Rest]) :-
    Head =.. [Name | Args],
    asserta((Head :- invoke_method(Name, Args))),
    assert_trampolines(Rest).

invoke_method(Name, [Ptr | Args]) :-
    inst(Ptr, Instance),
    invoke_method(Name, [Instance | Args]),
    !.
invoke_method(Name, [object(Type, Value) | Args]) :-
    !,
    get_best_method([Type], Name, Method),
    asserta(Method),
    CalledHead =.. [Name | [object(Type, Value) | Args]],
    call(CalledHead),
    !, retract(Method).

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

% OK TESTATO   
get_method_from_methods_list(MethodName,
			     [(Head :- Body) | _],
			     (Head :- Body)) :-
    Head =.. [MethodName | _].
get_method_from_methods_list(MethodName,
			     [(Head :- _) | Rest],
			     Result) :-
    Head =.. [Functor | _],
    Functor \= MethodName,
    get_method_from_methods_list(MethodName, Rest, Result).
    
