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

% Term

jsonparse({Term}, jsonobj(Y)) :-
    compound(Term),
    !,
    Term =.. List,
    jsonparseobj_(List, Y),
    is_list(Y).

jsonparse([Term], jsonarray(Y)) :-
    compound(Term),
    !,
    Term =.. List,
    jsonparsearray_(List, Y).

jsonparse({}, jsonobj([])) :-
    !.

jsonparse([], jsonarray([])) :-
    !.

% Atom

jsonparse(Atom, Result) :-
    atom(Atom),
    catch(term_to_atom(Term, Atom), _, false),
    !,
    jsonparse(Term, Result).

% Stringhe / Codes / Chars
jsonparse(String, Result) :-
    string(String),
    !,
    term_string(Term, String),
    jsonparse(Term, Result).

jsonparse(Codes, Result) :-
    is_list(Codes),
    atom_codes(Atom, Codes),
    !,
    jsonparse(Atom, Result).

jsonparse(Chars, Result) :-
    is_list(Chars),
    atom_chars(Atom, Chars),
    !,
    jsonparse(Atom, Result).

% COSE NASCOSTE
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



%OLD WORKS

jsonaccess(jsonobj(O), L, Result) :-
    is_list(L),
    !,
    jsonaccess_(O, L, Result).

jsonaccess(jsonobj(O), S, Result) :-
    string(S),
    !,
    jsonaccess_(O, [S], Result).

jsonaccess_(List, [Number], Result) :-
    number(Number),
    !,
    nth0(Number, List, Result).

jsonaccess_(List, [Number | Xs], Result) :-
    number(Number),
    nth0(Number, List, jsonarray(TResult)),
    !,
    jsonaccess_(TResult, Xs, Result).

jsonaccess_(List, [Number | Xs], Result) :-
    number(Number),
    nth0(Number, List, jsonobj(TResult)),
    !,
    jsonaccess_(TResult, Xs, Result).

jsonaccess_([Head | _], [X | [String]], Result) :-
    Head =.. L,
    [_, X, Y] = L,
    Y = jsonobj(List),
    !,
    jsonaccess_(List, [String], Result).


jsonaccess_([Head | _], [X | [String | Xs]], Result) :-
    Head =.. L,
    [_, X, Y] = L,
    Y = jsonobj(List),
    !,
    jsonaccess_(List, [String | Xs], Result).

jsonaccess_([Head | _], [X | [Number | Xs]], Result) :-
    Head =.. L,
    [_, X, Y] = L,
    Y = jsonarray(List),
    !,
    jsonaccess_(List, [Number | Xs], Result).

jsonaccess_([Head | _], [X], Result) :-
    Head =.. L,
    [_, X, Result] = L,
    !.

jsonaccess_([_ | Tail], L, Result) :-
    !,
    jsonaccess_(Tail, L, Result).


% INPUT/OUTPUT
%

jsonread(FileName, JSON) :-
    open(FileName, read, In),
    read_string(In, _, String),
    jsonparse(String, O),
    JSON = O,
    close(In).

jsondump(JSON, File) :-
    open(File, write, Out),
    JSON =.. ListJSON,
    jsondump_(ListJSON, Out, 0, 0),
    close(Out).

jsondump_([jsonobj, List], Out, Index, _) :-
    !,
    writeln(Out, "{"),
    Index1 is Index + 2,
    jsondumpobj_(List, Out, Index1, IndexLast),
    tab(Out, IndexLast),
    writeln(Out, "}").

jsondump_([jsonarray, List], Out, Index, _) :-
    !,
    writeln(Out, "["),
    Index1 is Index + 2,
    jsondumparray_(List, Out, Index1, IndexLast),
    tab(Out, IndexLast),
    writeln(Out, "]").

%OBJ

jsondumpobj_([',', X, Y], Out, Index, _) :-
    check_value(Y),
    !,
    tab(Out, Index),
    format(Out, '"~w" : "~w"', [X, Y]).

jsondumpobj_([',', X, Y], Out, Index, IndexLast) :-
    !,
    tab(Out, Index),
    format(Out, '"~w" : ', [X]),
    Y =.. ListY,
    jsondump_(ListY, Out, IndexLast, IndexLast).

jsondumpobj_([Head], Out, Index, Index) :-
    !,
    Head =.. ListHead,
    jsondumpobj_(ListHead, Out, Index, Index).

jsondumpobj_([Head | Tail], Out, Index, IndexLast) :-
    !,
    Head =.. ListHead,
    jsondumpobj_(ListHead, Out, Index, IndexLast),
    writeln(Out, ","),
    jsondumpobj_(Tail, Out, Index, IndexLast).

%ARRAY

jsondumparray_([Head], Out, Index, Index) :-
    !,
    jsondumparray_(Head, Out, Index, Index),
    nl(Out).

jsondumparray_([Head | Tail], Out, Index, Index) :-
    !,
    jsondumparray_(Head, Out, Index, Index),
    writeln(Out, ","),
    jsondumparray_(Tail, Out, Index, Index).

jsondumparray_(X, Out, Index, Index) :-
    check_value(X),
    !,
    tab(Out, Index),
    format(Out, '"~w"', [X]).

jsondumparray_(X, Out, Index, Index) :-
    !,
    X =.. List,
    jsondump_(List, Out, Index, Index).

%

:- begin_tests(progetto).

test("Esempio 1", true) :-
    jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', O),
    jsonaccess(O, ["nome"], R),
    O = jsonobj([("nome", "Arthur"), ("cognome", "Dent")]),
    R = "Arthur".

test("Esempio 1 Codes", true) :-
    X = '{"nome" : "Arthur", "cognome" : "Dent"}',
    atom_codes(X, Codes),
    jsonparse(Codes, O),
    jsonaccess(O, ["nome"], R),
    O = jsonobj([("nome", "Arthur"), ("cognome", "Dent")]),
    R = "Arthur".

test("Esempio 1 Access Innestato", true) :-
    jsonparse('{"nome" : {"primonome" : "don rodrigo",
                          "secondonome" : ["giorgino", "usualdo"]},
                "cognome" : ["Dent", "OWO", ["WEY", {"wut" : "EZNO", "XD" : "EZ"}], "RIP"]
               }', O),
    O = jsonobj([("nome",
                  jsonobj([("primonome","don rodrigo"),("secondonome",
                                                        jsonarray(["giorgino","usualdo"]))])),("cognome",
                                                                                               jsonarray(["Dent","OWO",jsonarray(["WEY",jsonobj([("wut","EZNO"),("XD","EZ")])]),"RIP"]))]),
    jsonaccess(O, ["nome", "primonome"], R),
    R = "don rodrigo",
    jsonaccess(O, ["nome", "secondonome", 1], R2),
    R2 = "usualdo",
    jsonaccess(O, ["cognome", 2, 1, "XD"], R3),
    R3 = "EZ".


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
    jsonaccess(O, ["menu", "items", 1, "label"], R),
    R = "Open New".

test("WOT", true) :-
    X = {"web-app": {
  "servlet": [
    {
      "servlet-name": "cofaxCDS",
      "servlet-class": "org.cofax.cds.CDSServlet",
      "init-param": {
        "configGlossary:installationAt": "Philadelphia, PA",
        "configGlossary:adminEmail": "ksm@pobox.com",
        "configGlossary:poweredBy": "Cofax",
        "configGlossary:poweredByIcon": "/images/cofax.gif",
        "configGlossary:staticPath": "/content/static",
        "templateProcessorClass": "org.cofax.WysiwygTemplate",
        "templateLoaderClass": "org.cofax.FilesTemplateLoader",
        "templatePath": "templates",
        "templateOverridePath": "",
        "defaultListTemplate": "listTemplate.htm",
        "defaultFileTemplate": "articleTemplate.htm",
        "useJSP": false,
        "jspListTemplate": "listTemplate.jsp",
        "jspFileTemplate": "articleTemplate.jsp",
        "cachePackageTagsTrack": 200,
        "cachePackageTagsStore": 200,
        "cachePackageTagsRefresh": 60,
        "cacheTemplatesTrack": 100,
        "cacheTemplatesStore": 50,
        "cacheTemplatesRefresh": 15,
        "cachePagesTrack": 200,
        "cachePagesStore": 100,
        "cachePagesRefresh": 10,
        "cachePagesDirtyRead": 10,
        "searchEngineListTemplate": "forSearchEnginesList.htm",
        "searchEngineFileTemplate": "forSearchEngines.htm",
        "searchEngineRobotsDb": "WEB-INF/robots.db",
        "useDataStore": true,
        "dataStoreClass": "org.cofax.SqlDataStore",
        "redirectionClass": "org.cofax.SqlRedirection",
        "dataStoreName": "cofax",
        "dataStoreDriver": "com.microsoft.jdbc.sqlserver.SQLServerDriver",
        "dataStoreUrl": "jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon",
        "dataStoreUser": "sa",
        "dataStorePassword": "dataStoreTestQuery",
        "dataStoreTestQuery": "SET NOCOUNT ON;select test='test';",
        "dataStoreLogFile": "/usr/local/tomcat/logs/datastore.log",
        "dataStoreInitConns": 10,
        "dataStoreMaxConns": 100,
        "dataStoreConnUsageLimit": 100,
        "dataStoreLogLevel": "debug",
        "maxUrlLength": 500}},
    {
      "servlet-name": "cofaxEmail",
      "servlet-class": "org.cofax.cds.EmailServlet",
      "init-param": {
      "mailHost": "mail1",
      "mailHostOverride": "mail2"}},
    {
      "servlet-name": "cofaxAdmin",
      "servlet-class": "org.cofax.cds.AdminServlet"},

    {
      "servlet-name": "fileServlet",
      "servlet-class": "org.cofax.cds.FileServlet"},
    {
      "servlet-name": "cofaxTools",
      "servlet-class": "org.cofax.cms.CofaxToolsServlet",
        "init-param": {
            "templatePath": "toolstemplates/",
            "log": 1,
            "logLocation": "/usr/local/tomcat/logs/CofaxTools.log",
            "logMaxSize": "",
            "dataLog": 1,
            "dataLogLocation": "/usr/local/tomcat/logs/dataLog.log",
            "dataLogMaxSize": "",
          "removePageCache": "/content/admin/remove?cache=pages&id=",
          "removeTemplateCache": "/content/admin/remove?cache=templates&id=",
          "fileTransferFolder": "/usr/local/tomcat/webapps/content/fileTransferFolder",
          "lookInContext": 1,
          "adminGroupID": 4,
          "betaServer": true}}],
              "servlet-mapping": {
      "cofaxCDS": "/",
      "cofaxEmail": "/cofaxutil/aemail/*",
      "cofaxAdmin": "/admin/*",
      "fileServlet": "/static/*",
      "cofaxTools": "/tools/*"},

              "taglib": {
      "taglib-uri": "cofax.tld",
      "taglib-location": "/WEB-INF/tlds/cofax.tld"}}},
        jsonparse(X, JSON),
        jsondump(JSON, 'foo2.json'),
        jsonread('foo2.json', JSON).


test("Esempio I/O", true) :-
    jsonparse('{"nome" : "Zaphod",
                "heads" : ["Head1", "Head2"]}', % Attenzione al newline.
              JSON),
    jsondump(JSON, 'foo.json'),
    jsonread('foo.json', JSON).
:- end_tests(progetto).
