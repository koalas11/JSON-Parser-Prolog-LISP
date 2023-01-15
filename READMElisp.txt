La realizzazione in lisp di un parser JSON vede come prima funzione (jsonparse (String)) la quale si occupa di eseguire una pulizia della stringa
data in input. 

L'input è organizzato nel seguente modo:
i valori di tipo Oggetto sono nella forma '('jsonobj members ')' o '('jsonarray elements')', dove Members può essere coppia*, coppia '('attributo valore ')',
attributo è una stringa, valore può essere a sua volta una stringa, un numero o un Oggetto. elements può essere zero o più valori.

Successivamente viene richiamata la funzione (stringToJson (String)) la quale determina se siamo in un caso elementare come {} o [].
Con l'uso di un (cond) è in grado di riconoscere l'inizio di un array o di un oggetto.

(jsonparse) sfrutta anche due funzioni ausiliarie: (getMembers (String)) e (getValues (String))

(getMembers) si occupa di cercare i members all'interno di Oggetti, identificati con ':' e ','. Nel caso non siano presenti ',' la funzione restituisce nil

(getValues) invece è la funzione specifica per la gestione di oggetti di tipo array, ricerca infatti tutti gli 'elements' di un array, separati anch'essi da ','.

Entrambe le funzioni per ogni valore chiamano (parseValue (Value)) e (parseMember(Member &optional index))
(parseMember) gestisce i membri di un oggetto definiti come attributo:valore.
(parseValue) invece determina il tipo di valore, e in caso venga trovato un valore di tipo Oggetto, richiama ricorsivamente (jsonparse) che lo restituirà parsato.

Sia (parseValue) che (parseMember) utilizzano la serie di funzioni (hidequotes) (showquotes) e (getquotespos) per la gestione dei casi in cui nelle stringhe siano presenti elementi come gli apici interni '\\\"'
Altri valori come 'false', 'true' e 'null' sono gestiti come simboli

Per accedere all'output di (jsonparse) è stata implementata la funzione (jsonaccess (json &rest fields))
se field è una stringa, chiama (accessField(json field)), se field è un numero chiama (accessNumber(json index))
 
(accessField(json field)) ricerca l'attributo corrispondente nell'oggetto, mentre (accessNumber(json index)) accede all'elemento nell'array

Per le operazioni di lettura e scrittura su file sono state create 2 funzioni (jsondump(json file)) e (jsonread(file))

(jsonread(file)) carica il file indicato nella path 'file', lo trasforma in stringa e richiama (jsonparse) per restituire infine la lista come richiesto.

(jsondump (json file)) invece se 'file' è presente lo sovrascrive, se non c'è, lo crea e stampa all'interno di esso 'json' richiamando a seconda del tipo di valore trovato (jsondumparray) e (jsondumpobj)

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

che risulta in:
(JSONOBJ ("Example" (JSONOBJ ("number int" 42) ("number float" 42.0) ("number exponential" 42.0) ("string" "im a string and \"42\" best number") ("special values" #))))
