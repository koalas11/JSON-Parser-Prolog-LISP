(defun jsonparse(stringaInput)

    ;salvo la lunghezza della stringa in input (non si sa mai)

    (setq lunghezza (length stringaInput))

    ; assegno il tipo alla variabile "tipo" (non lo avresti mai detto eh?)
    ; se le parentesi sono scorrette do semplicemente errore

    (setq tipo "?")

    (if (and (string-equal (char stringaInput 0) "[")
                (string-equal (char stringaInput (1- lunghezza))"]"))
            (progn
                (setq tipo "array")))
                
    (if (and (string-equal (char stringaInput 0) "{")
                (string-equal (char stringaInput (1- lunghezza))"}"))
            (progn 
                (setq tipo "oggetto")))

    (if (string-equal tipo "?")
        (error "syntax error"))

    (if (string-equal tipo "array")
    ;richiamo alla funzione parseArray
        (format t "(JSONARRAY ~a)" (parseArray stringaInput)))
)
(defun parseArray(stringaInput)

    ;uso (aref) per identificare la virgola come carattere perché lisp è brutto
    
    (setq temp (remove (aref "," 0) stringaInput))
    ;so che [] saranno sicuramente all'inizio del mio array, e li tolgo
    ; !! non gestisce sub array, li faccio più tardi 
    (setq temp (string-trim "[]" temp))
    (return-from parseArray temp)
)
(setq input (read-line))
(jsonparse input)

