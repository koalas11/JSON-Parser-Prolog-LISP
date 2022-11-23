check_value(X) :-
    string(X),
    !.

check_value(X) :-
    number(X),
    !.

check_value(X) :-
    X = 'true',
    !.

check_value(X) :-
    X = 'false',
    !.

check_value(X) :-
    X = 'null',
    !.




jsonparse(X, jsonobj([])) :-
    atom(X),
    catch(term_to_atom({}, X), _, fail),
    !.

jsonparse(X, jsonobj(Y)) :-
    atom(X),
    catch(term_to_atom({T}, X), _, fail),
    !,
    T =.. L,
    jsonparseobj_(L, Y),
    is_list(Y).


jsonparse(X, jsonarray([])) :-
    atom(X),
    catch(term_to_atom([], X), _, fail),
    term_to_atom([], X),
    !.

jsonparse(X, jsonarray(Y)) :-
    atom(X),
    catch(term_to_atom([T], X), _, fail),
    !,
    T =.. L,
    jsonparsearray_(L, Y).


jsonparseobj_([':', X, Y], [(Res)]) :-
    string(X),
    Y = {},
    !,
    Res =.. [',', X, jsonobj([])].

jsonparseobj_([':', X, Y], [(Res)]) :-
    string(X),
    Y = {Y2},
    !,
    Y2 =.. L,
    jsonparseobj_(L, TRes2),
    Res =.. [',', X, jsonobj(TRes2)],
    is_list(TRes2).

jsonparseobj_([':', X, Y], [(Res)]) :-
    string(X),
    is_list(Y),
    !,
    jsonparsearray_(Y, TRes1),
    Res =.. [',', X, jsonarray(TRes1)].

jsonparseobj_([':', X, Y], [(Res)]) :-
    !,
    string(X),
    check_value(Y),
    Res =.. [',', X, Y].

jsonparseobj_([',', X, Rest], [Head | Tail]) :-
    !,
    X =.. LX,
    jsonparseobj_(LX, LHead),
    LHead = [Head],
    Rest =.. Rest2,
    jsonparseobj_(Rest2, Tail).

jsonparsearray_([], []) :- !.

jsonparsearray_([{X} | Xs], [jsonobj(Res) | Ys]) :-
    X =.. L,
    !,
    jsonparseobj_(L, Res),
    is_list(Res),
    jsonparsearray_(Xs, Ys).

jsonparsearray_([X | Xs], [jsonarray(Res) | Ys]) :-
    is_list(X),
    !,
    jsonparsearray_(X, Res),
    jsonparsearray_(Xs, Ys).

jsonparsearray_([X | Xs], [X | Ys]) :-
    check_value(X),
    !,
    jsonparsearray_(Xs, Ys).


jsonaccess(jsonobj(O), L, Result) :-
    is_list(L),
    !,
    jsonaccess_(O, L, Result).

jsonaccess(jsonobj(O), S, Result) :-
    string(S),
    !,
    jsonaccess_(O, [S], Result).

jsonaccess_([Head | _], [X | [Number]], Result) :-
    Head =.. L,
    [_, X, Y] = L,
    Y = jsonarray(List),
    nth0(Number, List, Result),
    !.

jsonaccess_([Head | _], [X | _], Result) :-
    Head =.. L,
    [_, X, Result] = L,
    !.

jsonaccess_([Head | _], List, Result) :-
    Head =.. L,
    [_, _, Y] = L,
    Y = jsonobj(O),
    jsonaccess_(O, List, Result),
    !.

jsonaccess_([_ | Tail], L, Result) :-
    !,
    jsonaccess_(Tail, L, Result).



:- begin_tests(progetto).

test("Esempio 1", true) :-
    jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', O),
    jsonaccess(O, ["nome"], R),
    O = jsonobj([("nome", "Arthur"), ("cognome", "Dent")]),
    R = "Arthur".

test("Esempio 2", true) :-
    jsonparse('{"nome": "Arthur", "cognome": "Dent"}', O),
    jsonaccess(O, "nome", R),
    O = jsonobj([("nome", "Arthur"), ("cognome", "Dent")]),
    R = "Arthur".

test("Esempio 3", true) :-
    jsonparse('{"nome" : "Zaphod",
                "heads" : ["Head1", "Head2"]}', % Attenzione al newline.
              Z),
    jsonaccess(Z, ["heads", 1], R),
    Z = jsonobj([("nome", "Zaphod"), ("heads", jsonarray(["Head1", "Head2"]))]), % da nome a name???
    R = "Head2".

test("Esempio 4", true) :-
    jsonparse('[]', X),
    X = jsonarray([]).

test("Esempio 5", true) :-
    jsonparse('{}', X),
    X = jsonobj([]).

test("Esempio 6", fail) :-
    jsonparse('[}', _).

test("Esempio 7", fail) :-
    jsonparse('[1, 2, 3]', A),
    jsonaccess(A, [3], _).

test("Esempio 8", true) :-
    jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', JSObj),
    jsonaccess(JSObj, ["cognome"], R),
    R = "Dent".

test("Esempio 9", true) :-
    jsonparse('{"menu": {
                    "header": "SVG Viewer",
                    "items": [
                        {"id": "Open"},
                        {"id": "OpenNew", "label": "Open New"},
                        null,
                        {"id": "ZoomIn", "label": "Zoom In"},
                        {"id": "ZoomOut", "label": "Zoom Out"},
                        {"id": "OriginalView", "label": "Original View"},
                        null,
                        {"id": "Quality"},
                        {"id": "Pause"},
                        {"id": "Mute"},
                        null,
                        {"id": "Find", "label": "Find..."},
                        {"id": "FindAgain", "label": "Find Again"},
                        {"id": "Copy"},
                        {"id": "CopyAgain", "label": "Copy Again"},
                        {"id": "CopySVG", "label": "Copy SVG"},
                        {"id": "ViewSVG", "label": "View SVG"},
                        {"id": "ViewSource", "label": "View Source"},
                        {"id": "SaveAs", "label": "Save As"},
                        null,
                        {"id": "Help"},
                        {"id": "About", "label": "About Adobe CVG Viewer..."}
                    ]
                }}', O),
    jsonaccess(O, ["items", X], R),
    writeln(X),
    writeln(R).


:- end_tests(progetto).
