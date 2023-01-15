La realizzazione in prolog del parser JSON vede come primo predicato jsonparse/2.
jsonparse/2 utilizza l'unificazione per il riconoscimento di Array o Oggetti.
Inoltre viene usato univ, che serve per identificare la presenza di uno o più coppie di chiave valore.
Esso è definito come 'jsonparse(InputType, Result)', dove 'inputType' è diverso a seconda dell'input fornito ed include anche casi base come 

'jsonparse({}, JSON)' che restituirà jsonobj([])

'jsonparse([], JSON)' che restituirà jsonarray([])

Viene controllata la correttezza dell'input e nel caso sia errato, il predicato fallirà.

L'input è organizzato nel seguente modo:

I valori di tipo Oggetto sono nella forma jsonobj(Members) o jsonarray(Elements), dove Members può essere '[]', '[Pair | MoreMembers]'. 
Pair è la coppia (Attributo, Valore), dove Attributo può essere soltanto una stringa, Valore, un Numero o un nuovo Oggetto. Elements può essere vuoto '[]' o '[Valore | Resto]'.

Successivamente è stato implementato il predicato jsonparse_/2, il quale è definito come jsonparse(Input, Result) ed è in grado di smistare input tra jsonparseobj_/2 e jsonparsearray_/2,
i quali si occupano rispettivamente della gestione del parsing di oggetti e array.

Vi sono altri due predicati utilizzati, checkValue/2 e jsonParseRev_/2:

checkValue/2 è definito come checkValue(Input, Compare), checkValue(JSON, Result) e checkValue(Result, JSON), quest'ultimo usato in particolare da jsonParseRev_/2

jsonparseRev_/2 definito come (Result, Input) viene utilizzato di modo che, a partire da un oggetto JSON già parsato, si ottiene come risultato un JSON non parsato come termine. Ovviamente anch'esso dispone di altri 2 predicati a supporto: jsonparseObjRev_/2 e jsonparseArrayRev_/2 che rispettivamente si occupano anch'essi della gestione di oggetti e array.

Per l'accesso all'oggetto JSON parsato viene usato jsonaccess/3, questo predicato è implementato come jsonaccess(JSON, Fields, Result). 
Il predicato inoltre sfrutta jsonaccess_/3 implementato come jsonaccess_(Input, Type, Result).
jsonaccess_/3 a seconda di 'type' gestisce 'input' in modo da fornire il risultato cercato all'interno di Result.

Inoltre il parser fornisce due predicati per lettura e scrittura su file, jsonread/2, jsondump/2.

jsonread/2 è implementato come jsonread(File, JSON) e permette la lettura di un file json, e lo parsa in automatico sfruttando il predicato jsonparse/2. 

E' stato implementato anche un diverso jsonread/2, il quale sfrutta anche jsonread/3, implementato come jsonread(Stream, Char, Chars), 
che consente di gestire anche il caso '\/' in quanto non è gestito da prolog e potrebbe generare errori.

jsondump/2 è implementato come jsondump(JSON, File). Ovvero prendendo in input un oggetto JSON, 
scrive sulla path del file 'File', il quale sarà creato nel caso non fosse già presente o in alternativa sovrascritto.
Anch'esso, sfruttando jsondumpobj_/3, jsondumparray_/3 riconosce e organizza gli elementi in 'JSON' che verranno scritti su file con write/2 e printValue/2.

Di seguito possono essere trovati degli esempi sui quali il programma è stato testato, con i vari tipi di valori

{
    "Example" : {
       "number int" : 42,
       "number float" : 42.0,
       "number exponential" : 4.2e1,
       "string" : "i'm a string and \"42\" best number",
       "special values" : [true, false, null]
    }
}

jsonobj([("Example", jsonobj([("number int", 42), ("number float", 42.0), ("number exponential", 42.0), ("string", "i'm a string and \"42\" best number"), (..., ...)]))])

Nota: a differenza di quanto richiesto, i nomi non sono stati messi all'inizio in quanto per abilitare la modalità corretta di Emacs era necessario usare la prima riga per Mode: Prolog
