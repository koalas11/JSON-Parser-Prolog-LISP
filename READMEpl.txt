La realizzazione in prolog del parser JSON vede come primo predicato jsonparse/2.
Esso è definito come 'jsonparse(inputType, Result)', dove 'inputType' è diverso a seconda dell'input fornito, ed inculde anche casi base come 

'jsonparse({}, JSON)' che restituirà jsonobj([])

'jsonparse([], JSON)' che restituirà jsonarray([])

Successivamente è stato implementato il predicato jsonparse_/2, il quale è definito come jsonparse(input, result) ed è in grado di smistare input tra jsonparseobj_/2 e jsonparsearray_/2. I quali si occupano rispettivamente della gestione del parsing di oggetti e array.

Vi sono altri due predicati utilizzati, checkValue/2 e jsonParseRev_/2

checkValue/2 è definito come checkValue(Input, Compare), checkValue(JSON, Result)

jsonParseRev_/2 definito come (Result, Input) viene utilizzato di modo che, a partire da un oggetto JSON già parsato, si ottiene come risultato un JSON non parsato come termine.

Per l'accesso all'oggetto JSON parsato viene usato jsonaccess/3, questo predicato è implementato come jsonaccess(JSON, Fields, Result). Il predicato inoltre sfrutta jsonaccess_/3 implementato come jsonaccess_(input, type, result).
jsonaccess_/3 a seconda di 'type' gestisce 'input' in modo da fornire il risultato cercato all'interno di input.


Inoltre il parser fornisce due predicati per lettura e scrittura su file, jsonread/2, jsondump/2.

jsonread/2 è implementato come jsonread(File, JSON) e permette la lettura di un file json, e lo parsa in automatico sfruttando il predicato jsonparse/2. 

E' stato implementanto anche un diverso jsonread/2, il quale sfrutta anche jsonparse/3, implementato come jsonparse(Stream, Char, Chars), che consente di gestire anche il caso '\/' in quanto non è gestito da prolog e potrebbe generare errori.

jsondump/2 è implementato come jsondump(JSON, File). Ovvero prendendo in input un oggetto JSON, scrive su un file 'File'. Anch'esso, sfruttando jsondumpobj_/3, jsondumparray_/3 riconosce e organizza gli elementi in 'JSON' che verranno scritti su file con write/2 e printValue/2 

