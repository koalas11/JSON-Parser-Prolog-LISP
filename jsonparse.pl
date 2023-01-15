%%%% -*- Mode: Prolog -*-

%%%% jsonparse.pl --

%%%% COMPONENTI GRUPPO:
%%%% Sanvito Marco 886493
%%%% Tugrul Emre 886027
%%%% Piccioni Matteo 879377

% JSONPARSE
% Prende in input un json (di diversi tipi) e lo
% parsa (gestisce anche il caso in cui si abbia già
% un oggetto parsato)

% Atom
jsonparse({}, jsonobj([])) :- !.

jsonparse([], jsonarray([])) :- !.

jsonparse(Atom, JSONResult) :-
    atom(Atom),
    catch(term_to_atom(Term, Atom), _, fail),
    !,
    jsonparse(Term, JSONResult).

% Stringhe / Codes / Chars
jsonparse(String, JSONResult) :-
    string(String),
    !,
    catch(term_string(Term, String), _, fail),
    jsonparse(Term, JSONResult).

jsonparse(Codes, JSONResult) :-
    is_list(Codes),
    catch(atom_codes(Atom, Codes), _, fail),
    jsonparse(Atom, JSONResult),
    !.

jsonparse(Chars, JSONResult) :-
    is_list(Chars),
    catch(atom_chars(Atom, Chars), _, fail),
    jsonparse(Atom, JSONResult),
    !.

% Term
jsonparse(Term, JSONResult) :-
    compound(Term),
    !,
    jsonparse_(Term, JSONResult).

% Jsonparse Reverse
jsonparse(Result, JSON) :-
    var(Result),
    nonvar(JSON),
    !,
    jsonparseRev_(Result, JSON).

% Funzioni di supporto per il parser
jsonparse_({}, jsonobj([])) :- !.

jsonparse_({Members}, jsonobj(Result)) :-
    !,
    Members =.. List,
    jsonparseobj_(List, Result).

jsonparse_(List, jsonarray(Result)) :-
    is_list(List),
    !,
    jsonparsearray_(List, Result).

jsonparseobj_([':', String, Val], [(String , RValue)]) :-
    string(String),
    checkValue(Val, RValue),
    !.

jsonparseobj_([',', (Str : Val), Rest], [(Str , RVal) | Tail]) :-
    !,
    string(Str),
    checkValue(Val, RVal),
    Rest =.. LRest,
    jsonparseobj_(LRest, Tail).

jsonparsearray_([], []) :- !.

jsonparsearray_([Val | Tail], [RVal | RTail]) :-
    checkValue(Val, RVal),
    !,
    jsonparsearray_(Tail, RTail).

% Reverse Parser
jsonparseRev_({}, jsonobj([])) :- !.

jsonparseRev_({Members}, jsonobj(List)) :-
    !,
    jsonparseObjRev_(Members, List).

jsonparseRev_(RList, jsonarray(List)) :-
    !,
    jsonparseArrayRev_(RList, List).

jsonparseObjRev_(Str : RVal, [',', Str, Val]) :-
    string(Str),
    checkValue(RVal, Val),
    !.

jsonparseObjRev_(Result, [Head]) :-
    !,
    functor(Head, ',', 2),
    Head =.. List,
    jsonparseObjRev_(Result, List).

jsonparseObjRev_(Result, [Head | Tail]) :-
    !,
    Head =.. List,
    jsonparseObjRev_(TempResult1, List),
    jsonparseObjRev_(TempResult2, Tail),
    Result =.. [',', TempResult1, TempResult2].

jsonparseArrayRev_([], []) :- !.

jsonparseArrayRev_(Result, [Head | Tail]) :-
    !,
    checkValue(Pair, Head),
    jsonparseArrayRev_(ResultTail, Tail),
    append([Pair], ResultTail, Result).

% CHECKVALUE
% Controlla la correttezza del valore e ritorna
% il valore stesso o il valore parsato nel caso
% sia un oggetto o array
checkValue(Str, Str) :-
    string(Str),
    !.

checkValue(Num, Num) :-
    number(Num),
    !.

checkValue('true', 'true') :- !.

checkValue('false', 'false') :- !.

checkValue('null', 'null') :- !.

checkValue(JSON, Result) :-
    nonvar(JSON),
    !,
    jsonparse_(JSON, Result).

checkValue(Result, JSON) :-
    !,
    nonvar(JSON),
    jsonparseRev_(Result, JSON).

% JSONACCESS
% Accede al json parsato in base ai fields passati
% in input permettendo anche l'uso di variabili
% nel json o nei fields
jsonaccess(jsonarray(_), [], _) :- !, fail.

jsonaccess(JSON, Fields, Result) :-
    nonvar(JSON),
    !,
    jsonaccess_(JSON, Fields, Result).

jsonaccess_(jsonobj(List), String, Result) :-
    string(String),
    !,
    member((String , Result), List),
    !.

jsonaccess_(jsonobj(List), Var, Result) :-
    var(Var),
    !,
    member((Var , Result), List).

jsonaccess_(jsonarray(List), Index, Result) :-
    integer(Index),
    !,
    nth0(Index, List, Result).

jsonaccess_(jsonarray(List), Var, Result) :-
    var(Var),
    !,
    nth0(Var, List, Result).

jsonaccess_(Obj, [], Obj) :- !.

jsonaccess_(Obj, [Field | OtherFields], Result) :-
    !,
    jsonaccess_(Obj, Field, TRes),
    jsonaccess_(TRes, OtherFields, Result).

% INPUT
% JSONREAD
jsonread(FileName, JSON) :-
    nonvar(FileName),
    catch(open(FileName, read, In), _, fail),
    catch(
        (
        read_string(In, _, String),
        jsonparse(String, JSON)
    ),
        _,
        (close(In), fail)
    ),
    !,
    close(In).

% Caso per gestire la presenza di \/ nel file
% nel caso il primo read_string fallisca
jsonread(FileName, JSON) :-
    nonvar(FileName),
    catch(open(FileName, read, In), _, fail),
    catch(
        (
        get_char(In, Char),
        jsonread(In, Char, Chars),
        jsonparse(Chars, JSON)
    ),
        _,
        (close(In), fail)
    ),
    !,
    close(In).

jsonread(_, end_of_file, []) :- !.

jsonread(Stream, '\\', Chars) :-
    get_char(Stream, '/'),
    !,
    Chars = ['/' | Rest],
    get_char(Stream, Next),
    jsonread(Stream, Next, Rest).

jsonread(Stream, Char, Chars) :-
    !,
    Chars = [Char | Rest],
    get_char(Stream, Next),
    jsonread(Stream, Next, Rest).

% OUTPUT
% JSONDUMP
% Stampa su file il json parsato in formato
% json creando un nuovo file se non presente
% o sovrascrivendo se presente
jsondump(JSON, File) :-
    nonvar(JSON),
    atom(File),
    open(File, write, Out),
    JSON =.. ListJSON,
    jsondump_(ListJSON, Out, 0),
    close(Out).

jsondump_([jsonobj, {}], Out, _) :-
    !,
    write(Out, "{}").

jsondump_([jsonarray, []], Out, _) :-
    !,
    write(Out, "[]").

jsondump_([jsonobj, List], Out, Index) :-
    !,
    writeln(Out, "{"),
    Index1 is Index + 2,
    jsondumpobj_(List, Out, Index1),
    tab(Out, Index),
    write(Out, "}").

jsondump_([jsonarray, List], Out, Index) :-
    !,
    writeln(Out, "["),
    Index1 is Index + 2,
    jsondumparray_(List, Out, Index1),
    tab(Out, Index),
    write(Out, "]").

%OBJ DUMP
jsondumpobj_([',', Str, Value], Out, Index) :-
    checkValue(Value, _),
    !,
    tab(Out, Index),
    format(Out, '"~w" : ', [Str]),
    printValue(Out, Value).

jsondumpobj_([',', Str, Value], Out, Index) :-
    !,
    tab(Out, Index),
    format(Out, '"~w" : ', [Str]),
    Value =.. ListValue,
    jsondump_(ListValue, Out, Index).

jsondumpobj_([Head], Out, Index) :-
    !,
    Head =.. ListHead,
    jsondumpobj_(ListHead, Out, Index),
    nl(Out).

jsondumpobj_([Head | Tail], Out, Index) :-
    !,
    Head =.. ListHead,
    jsondumpobj_(ListHead, Out, Index),
    writeln(Out, ","),
    jsondumpobj_(Tail, Out, Index).

% ARRAY DUMP
jsondumparray_([Head], Out, Index) :-
    !,
    tab(Out, Index),
    jsondumparray_(Head, Out, Index),
    nl(Out).

jsondumparray_([Head | Tail], Out, Index) :-
    !,
    tab(Out, Index),
    jsondumparray_(Head, Out, Index),
    writeln(Out, ","),
    jsondumparray_(Tail, Out, Index).

jsondumparray_(Value, Out, _) :-
    checkValue(Value, _),
    !,
    format(Out, '"~w"', [Value]).

jsondumparray_(Value, Out, Index) :-
    !,
    Value =.. List,
    jsondump_(List, Out, Index).

% PRINTVALUE
% Stampa il valore tenendo conto se è una stringa
% oppure un altro valore
printValue(Out, Str) :-
    string(Str),
    !,
    write(Out, "\""),
    write(Out, Str),
    write(Out, "\"").

printValue(Out, Value) :-
    !,
    write(Out, Value).

%%%% end of file -- jsonparse.pl --
