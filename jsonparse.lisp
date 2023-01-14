;;;; -*- Mode: Lisp -*-

;;;; jsonparse.lisp --

;JSONPARSE

(defun jsonparse (JSONString) 
  (stringToJSON (fixString JSONString)))

(defun stringToJSON (String)
  (cond ((string= String "{}") (list 'JSONOBJ))
        ((string= String "[]") (list 'JSONARRAY))
        ((and (eql #\{ (firstChar String))
              (eql #\} (lastChar String)))
         (cons 'JSONOBJ (getMembers (removeBrackets String))))
        ((and (eql #\[ (firstChar String))
              (eql #\] (lastChar String)))
         (cons 'JSONARRAY (getValues (removeBrackets String))))
        (t (error "syntax error"))))

(defun getMembers (Members &optional (index 0))
  (let ((var (position #\, Members :start index)))
    (handler-case (cons (parseMember (subseq Members 0 var))
                        (if (null var) nil
                          (getMembers (subseq Members (+ var 1)))))
      (error () 
        (if (null var) (error "syntax error")
          (getMembers Members (+ var 1)))))))

(defun getValues (Values &optional (index 0))
  (let ((var (position #\, Values :start index)))
    (handler-case (cons (parseValue (subseq Values 0 var))
                        (if (null var) nil
                          (getValues (subseq Values (+ var 1)))))
      (error () 
        (if (null var) (error "syntax error")
          (getValues Values (+ var 1)))))))

(defun parseMember (Member &optional (index 0))
  (let ((var (position #\: Member :start index)))
    (handler-case (if (not (null (getString (subseq Member 0 var))))
                      (list (getString (subseq Member 0 var))
                            (parseValue (fixString (subseq Member (+ var 1)))))
                    (error "syntax error"))
      (error () 
        (if (null var) (error "syntax error")
          (parseMember Member (+ var 1)))))))

(defun parseValue (Value)
  (cond ((string= Value "true") 'true)
        ((string= Value "false") 'false)
        ((string= Value "null") 'null)
        ((not (null (getString Value)))
         (getString Value))
        ((floatp (read-from-string Value))
         (parse-float (fixString Value)))
        ((not (null (parse-integer Value :junk-allowed t)))
         (parse-integer Value))
        (t (jsonparse Value))))

(defun getString (String)
  (let* ((var (fixString String))
         (var2 (length var)))
    (if (< var2 2) nil
      (if (and (not (null (search "\"" var :end2 2))) 
               (not (null (search "\"" var :start2 (- var2 1)))))
          (replaceQuote (subseq var 1 (- var2 1)))
        nil))))

(defun replaceQuote (String)
  (let ((var (search "\\\"" String)))
    (if (null var) String
      (concatenate 'string (subseq String 0 var) "\"" (subseq String (+ var 2))))))
 
(defun fixString (String)
  (string-trim '(#\Space #\Tab #\Newline) String))
  
(defun firstChar (String)
  (char String 0))

(defun lastChar (String)
  (char String (- (length String) 1)))

(defun removeBrackets (String)
  (if (< (length String) 2) (error "syntax error")
    (fixString (subseq String 1 (- (length String) 1)))))

; JSONACCESS
; esiste solo perch� altrimenti viene applicato &rest ad ogni chiamata ricorsiva
(defun jsonaccess (json &rest fields)
  (if (and (eql (first json) 'JSONARRAY) (null fields)) 
      (error "field empty with starting JSONARRAY")
    (jsonaccess_ json fields)))
  
; sto provando a rimuovere &rest, se non va rimettilo sotto!
(defun jsonaccess_ (json fields)
  (let ((field (first fields)))
    (cond ((null fields) json)
          ((and (eql (first json) 'JSONOBJ) (stringp field))
           (jsonaccess_ (accessField (rest json) field) (rest fields)))
          ((and (eql (first json) 'JSONARRAY) (numberp field))
           (jsonaccess_ (accessNumber (rest json) field) (rest fields)))
          (T (error "errore nei fields")))))


; se field � una stringa, accedo all'oggetto
; si potrebbe aggiungere error come access-numer per leggibilita errore
(defun accessField (json field)
  (cond ((null json) (error "field ~A not found" field))
        ((string= (caar json) field) 
         (second (first json)))
        (T (accessField (rest json) field))))


; se field � un numero, accedo al valore dell'array
; il secondo cond si pu� eliminare sostituendo con nth (meglio x leggibilita error)
(defun accessNumber (json index)
  (cond ((not (null (nth index json))) 
         (nth index json))
        (T (error "index out of bounds"))))

; INPUT/OUTPUT

(defun jsonread (filename)
  (with-open-file (stream filename
                          :direction :input
                          :if-does-not-exist :error)
    (fileToString stream)))

(defun fileToString (stream)
  (let ((riga (read-line stream nil 'eof)))
    (if (eq riga 'eof) ""
      (concatenate 'string riga (fileToString stream)))))   				


; legge oggetto json parsato e lo stampa in formato json
(defun jsondump (JSON filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (jsondump_ stream JSON))
  filename)


(defun jsondump_ (stream JSON &optional (tab 0))
  (let ((primo (first JSON)))
    (cond 
     ((null primo) nil)
     ((eql primo 'JSONOBJ)
      (format stream "{" )
      (jsondumpobj stream (rest JSON) (+ tab 2))
      (terpri stream)
      (printTab stream tab)
      (format stream "}"))
     ((eql primo 'JSONARRAY)
      (format stream "[" )
      (jsondumparray stream (rest JSON) (+ tab 2))
      (terpri stream)
      (printTab stream tab)
      (format stream "]"))
     (t (error "dump error")))))


(defun jsondumpobj (stream JSON tab)
  (if (null json) nil
    (let ((pair (first JSON)))
      (terpri stream)
      (printTab stream tab)
      (format stream "~S : " (first pair))
      (printValue stream (second pair) tab)
      (if (null (rest JSON)) nil
        (format stream "," ))
      (jsondumpobj stream (rest JSON) tab))))



(defun jsondumparray (stream JSON tab)
  (if (null json) nil
    (progn 
      (terpri stream)
      (printTab stream tab)
      (printValue stream (first JSON) tab)
      (if (null (rest JSON)) nil
        (format stream "," ))
      (jsondumparray stream (rest JSON) tab))))

(defun printValue (stream value tab)
  (if (listp value) (jsondump_ stream value tab)
    (cond ((eql value 'true)
           (format stream "true"))
          ((eql value 'false)
           (format stream "false"))
          ((eql value 'null)
           (format stream "null"))
          (t (format stream "~S" value)))))

(defun printTab (stream tab)
  (if (= tab 0) nil
    (progn 
      (format stream " ")
      (printTab stream (- tab 1)))))

;;;; end of file -- jsonparse.lisp --