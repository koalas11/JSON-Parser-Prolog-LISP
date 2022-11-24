(defun jsonparse (input)
    "Faccio il parsing della stringa in input"
    ;verifico la correttezza dell'input
    (setq tipo (controllo input))
    (setq item (builder tipo input))
    (print item))

(defun controllo (input)
    "Controllo la correttezza generale dell'input"
    ;controllo se la stringa sia effettivamente una stringa
    (if (not (typep input 'string))
        (error "L\'input non e\' una stringa"))
    ;salvo la lunghezza
    (setq lunghezza (length input))
    (setq primo (char input 0))
    (setq ultimo (char input (1- lunghezza)))
    (if (and (char-equal primo #\[)(char-equal ultimo #\]))
        (setq tipo "vettore")
         (if (and (char-equal primo #\{)(char-equal ultimo #\}))
            (setq tipo "oggetto")
            (error "sintassi non corretta")))
    (return-from controllo tipo))
(defun builder(tipo input)
    (if(string-equal tipo "vettore")
        (progn 
            (setq item (make-instance 'vettore))
            (setf (elementi item) input))))
(defclass vettore ()
    (
        (elementi 
            :accessor elementi)
        (chiave
            :accessor chiave)))
(setq input (read))
(jsonparse input)