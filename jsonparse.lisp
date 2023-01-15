;;;; -*- Mode: Lisp -*-

;;;; jsonparse.lisp --

;;;; COMPONENTI GRUPPO:
;;;; Sanvito Marco 886493
;;;; Tugrul Emre 886027
;;;; Piccioni Matteo 879377

;;JSONPARSE
;;Prendo la stringa e la pulisco da eventuali spazi, tab e newline
(defun jsonparse (JSONString)
  (stringToJSON (fixString JSONString)))

;;Gestisco la stringa in base a se è un oggetto o un array
(defun stringToJSON (string)
  (cond ((string= string "{}") (list 'JSONOBJ))
        ((string= string "[]") (list 'JSONARRAY))
        ((and (eql #\{ (char string 0))
              (eql #\} 
                   (char string (- (length string) 1))))
         (cons 'JSONOBJ (getMembers (removeBrackets string))))
        ((and (eql #\[ (char string 0))
              (eql #\] 
                   (char string (- (length string) 1))))
         (cons 'JSONARRAY (getValues (removeBrackets string))))
        (t (error "syntax error"))))

;;Gestisco i members definiti come (Pair)*
(defun getMembers (members &optional (index 0))
  (let ((pos (position #\, members :start index)))
    (handler-case (cons (parseMember (subseq members 0 pos))
                        (if (null pos) nil
                          (getMembers (subseq members (+ pos 1)))))
      (error () 
        (if (null pos) 
            (error "syntax error")
          (getMembers members (+ pos 1)))))))

;;Gestisco i valori definiti come (Value)*
(defun getValues (values &optional (index 0))
  (let ((pos (position #\, values :start index)))
    (handler-case (cons (parseValue (fixString (subseq values 0 pos)))
                        (if (null pos) nil
                          (getValues (subseq values (+ pos 1)))))
      (error () 
        (if (null pos)
            (error "syntax error")
          (getValues values (+ pos 1)))))))

;;Parso i member definiti come Chiave : Valore
(defun parseMember (member &optional (index 0))
  (let ((pos (position #\: member :start index)))
    (handler-case
        (if (not (null (getString (subseq member 0 pos))))
            (list (getString (subseq member 0 pos))
                  (parseValue (fixString (subseq member (+ pos 1)))))
          (error "syntax error"))
      (error ()
        (if (null pos) 
            (error "syntax error")
          (parseMember member (+ pos 1)))))))

;;Parso il valore a seconda del tipo
(defun parseValue (value)
  (cond ((string= value "true") 'true)
        ((string= value "false") 'false)
        ((string= value "null") 'null)
        ((not (null (getString value)))
         (getString value))
        ((floatp (read-from-string value))
         (parse-float (fixString value)))
        ((not (null (parse-integer value :junk-allowed t)))
         (parse-integer value))
        (t (jsonparse value))))

;;Estraggo la stringa controllando la correttezza e gestendo
;;i doppi apici interni
(defun getString (string)
  (let* ((fixedString (fixString string))
         (length (length fixedString))
         (hiddenString (HideQuotes fixedString)))
    (if (and (> length 2)
             (not (null (search "\"" fixedString :end2 2)))
             (not (null (search "\"" fixedString :start2 (- length 1))))
             (null 
              (search "\"" (subseq hiddenString 1 
                                   (- (length hiddenString) 1)))))
        (showQuotes 
         (subseq hiddenString 1 (- (length hiddenString) 1))
         (getQuotesPos fixedString))
      nil)))

;;Funzioni di supporto a getString
;;getQuotesPos mette in una lista la posizione dei doppi apici
;;interni alla stringa identificati come \\\"
(defun getQuotesPos (string &optional (index 0))
  (let ((pos (search "\\\"" string :start2 index)))
    (if (null pos) nil
      (cons (- pos 1) (getQuotesPos (subseq string (+ pos 1)))))))

;;hideQuotes crea una stringa con gli apici interni nascosti
;;per controllare poi la correttezza della stringa
(defun hideQuotes (string)
  (let ((index (search "\\\"" String)))
    (if (null index) string
      (concatenate 'string (subseq string 0 index)
                   (hideQuotes (subseq string (+ index 2)))))))

;;showQuotes utilizza la lista di getQuotesPos per riformare
;;la stringa con gli apici interni scritti correttamente
(defun showQuotes (string list)
  (let ((index (first list)))
    (if (null index) string
      (concatenate 'string (subseq string 0 index)
                   "\""
                   (showQuotes (subseq string index) (rest list))))))

;;Funzioni di supporto al jsonparse
;;Elimina spazi newline e tab attorno alla stringa
(defun fixString (String)
  (string-trim '(#\Space #\Tab #\Newline) String))

;;Rimuove le parentesi iniziali e finali
(defun removeBrackets (String)
  (if (< (length String) 2) (error "syntax error")
    (fixString (subseq String 1 (- (length String) 1)))))

;;JSONACCESS
(defun jsonaccess (json &rest fields)
  (if (and (eql (first json) 'JSONARRAY) (null fields)) 
      (error "field empty with starting JSONARRAY")
    (jsonaccess_ json fields)))

;;Gestisco i caso jsonobj e jsonarray
(defun jsonaccess_ (json fields)
  (let ((field (first fields)))
    (cond ((null fields) json)
          ((and (eql (first json) 'JSONOBJ) (stringp field))
           (jsonaccess_ (accessField (rest json) field) (rest fields)))
          ((and (eql (first json) 'JSONARRAY) (numberp field))
           (jsonaccess_ (accessNumber (rest json) field) (rest fields)))
          (T (error "error in fields")))))

;;Accedo al jsonobj tramite una stringa
(defun accessField (json field)
  (cond ((null json) (error "field ~A not found" field))
        ((string= (caar json) field) 
         (second (first json)))
        (T (accessField (rest json) field))))

;;Accedo al jsonarray tramite un index
(defun accessNumber (json index)
  (cond ((not (null (nth index json))) 
         (nth index json))
        (T (error "index out of bounds"))))

;;INPUT/OUTPUT
;;JSONREAD
(defun jsonread (filename)
  (with-open-file (stream filename
                          :direction :input
                          :if-does-not-exist :error)
    (jsonparse (fileToString stream))))

(defun fileToString (stream)
  (let ((riga (read-line stream nil 'eof)))
    (if (eq riga 'eof) ""
      (concatenate 'string riga (fileToString stream)))))

;;JSONDUMP
;;Legge oggetto json parsato e lo stampa in formato json in un file
(defun jsondump (json filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (jsondump_ stream json))
  filename)

;;Gestisce i casi jsonobj e jsonarray
(defun jsondump_ (stream json &optional (tab 0))
  (let ((primo (first json)))
    (cond 
     ((null primo) nil)
     ((eql primo 'JSONOBJ)
      (format stream "{" )
      (jsondumpobj stream (rest json) (+ tab 2))
      (terpri stream)
      (printTab stream tab)
      (format stream "}"))
     ((eql primo 'JSONARRAY)
      (format stream "[" )
      (jsondumparray stream (rest json) (+ tab 2))
      (terpri stream)
      (printTab stream tab)
      (format stream "]"))
     (t (error "dump error")))))

;;Gestisce caso jsonobj
(defun jsondumpobj (stream json tab)
  (if (null json) nil
    (let ((pair (first json)))
      (terpri stream)
      (printTab stream tab)
      (format stream "~S : " (first pair))
      (printValue stream (second pair) tab)
      (if (null (rest json)) nil
        (format stream "," ))
      (jsondumpobj stream (rest json) tab))))

;;Gestisce caso jsonarray
(defun jsondumparray (stream json tab)
  (if (null json) nil
    (progn 
      (terpri stream)
      (printTab stream tab)
      (printValue stream (first json) tab)
      (if (null (rest json)) nil
        (format stream "," ))
      (jsondumparray stream (rest json) tab))))

;;Stampa il valore facendo attenzione ai valori (false null true)
(defun printValue (stream value tab)
  (if (listp value) (jsondump_ stream value tab)
    (cond ((eql value 'true)
           (format stream "true"))
          ((eql value 'false)
           (format stream "false"))
          ((eql value 'null)
           (format stream "null"))
          (t (format stream "~S" value)))))

;;Stampa gli spazi per semplificare la letture del file
(defun printTab (stream tab)
  (if (= tab 0) nil
    (progn 
      (format stream " ")
      (printTab stream (- tab 1)))))

;;;; end of file -- jsonparse.lisp --
