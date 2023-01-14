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

% Reverse

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

%

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

jsonaccess(jsonarray(_), [], _) :- !, fail.

jsonaccess(JSON, Fields, Result) :-
    nonvar(JSON),
    !,
    jsonaccess_(JSON, Fields, Result).

jsonaccess_(jsonobj(X), String, Result) :-
    string(String),
    !,
    member((String , Result), X),
    !.

jsonaccess_(jsonobj(X), Var, Result) :-
    var(Var),
    !,
    member((Var , Result), X).

% caso base in cui Field è un intero (trovo un intero)

jsonaccess_(jsonarray(X), Index, Result) :-
    integer(Index),
    !,
    nth0(Index, X, Result).

jsonaccess_(jsonarray(X), Var, Result) :-
    var(Var),
    !,
    nth0(Var, X, Result).

% caso base (controlla se arriva a lista vuota come field)
jsonaccess_(Obj, [], Obj) :- !.

% richiamo su primo Field, poi sulla coda
jsonaccess_(Obj, [Field | OtherFields], Result) :-
    !,
    jsonaccess_(Obj, Field, TRes),
    jsonaccess_(TRes, OtherFields, Result).

% INPUT/OUTPUT

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

% READ ALTERNATIVO POSSIBILE DA CANCELLARE GESTISCE CASO \/ ERRORE IN
% PROLOG

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

%JSONDUMP

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

%OBJ
printValue(Out, X) :-
    string(X),
    !,
    write(Out, "\""),
    write(Out, X),
    write(Out, "\"").

printValue(Out, X) :-
    !,
    write(Out, X).

jsondumpobj_([',', X, Y], Out, Index) :-
    checkValue(Y, _),
    !,
    tab(Out, Index),
    format(Out, '"~w" : ', [X]),
    printValue(Out, Y).

jsondumpobj_([',', X, Y], Out, Index) :-
    !,
    tab(Out, Index),
    format(Out, '"~w" : ', [X]),
    Y =.. ListY,
    jsondump_(ListY, Out, Index).

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

%ARRAY

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

jsondumparray_(X, Out, _) :-
    checkValue(X, _),
    !,
    format(Out, '"~w"', [X]).

jsondumparray_(X, Out, Index) :-
    !,
    X =.. List,
    jsondump_(List, Out, Index).

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
        jsonparse(Var, JSON),
        Var = X,
        jsondump(JSON, 'foo2.json'),
        jsonread('foo2.json', JSON).


test("Esempio I/O", true) :-
    jsonparse('{"nome" : "Zaphod",
                "heads" : ["Head1", "Head2"]}', % Attenzione al newline.
              JSON),
    jsondump(JSON, 'foo.json'),
    jsonread('foo.json', JSON).
:- end_tests(progetto).
