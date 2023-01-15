;;;; Sanvito Marco 886493
;;;; Tugrul Emre 886027
;;;; Piccioni Matteo 879377

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
    (handler-case
        (if (not (null (getString (subseq Member 0 var))))
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

(defun getQuotesPos (string &optional (index 0))
  (let ((pos (search "\\\"" string :start2 index)))
    (if (null pos) nil
      (cons (- pos 1) (getQuotesPos (subseq string (+ pos 1)))))))

(defun hideQuotes (string)
  (let ((index (search "\\\"" String)))
    (if (null index) string
      (concatenate 'string (subseq string 0 index)
                   (hideQuotes (subseq string (+ index 2)))))))

(defun showQuotes (string list)
  (let ((index (first list)))
    (if (null index) string
      (concatenate 'string (subseq string 0 index)
                   "\""
                   (showQuotes (subseq string index) (rest list))))))

(defun getString (String)
  (let* ((var (fixString String))
         (var2 (length var))
         (hiddenstring (HideQuotes var)))
    (if (and (> var2 2)
             (not (null (search "\"" var :end2 2)))
             (not (null (search "\"" var :start2 (- var2 1))))
             (null 
              (search "\"" (subseq hiddenstring 1 
                                   (- (length hiddenstring) 1)))))
        (showQuotes (subseq hiddenstring 1
                            (- (length hiddenstring) 1))
                    (getQuotesPos var))
      nil)))

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
(defun jsonaccess (json &rest fields)
  (if (and (eql (first json) 'JSONARRAY) (null fields)) 
      (error "field empty with starting JSONARRAY")
    (jsonaccess_ json fields)))
  
(defun jsonaccess_ (json fields)
  (let ((field (first fields)))
    (cond ((null fields) json)
          ((and (eql (first json) 'JSONOBJ) (stringp field))
           (jsonaccess_ (accessField (rest json) field) (rest fields)))
          ((and (eql (first json) 'JSONARRAY) (numberp field))
           (jsonaccess_ (accessNumber (rest json) field) (rest fields)))
          (T (error "errore nei fields")))))

(defun accessField (json field)
  (cond ((null json) (error "field ~A not found" field))
        ((string= (caar json) field) 
         (second (first json)))
        (T (accessField (rest json) field))))

(defun accessNumber (json index)
  (cond ((not (null (nth index json))) 
         (nth index json))
        (T (error "index out of bounds"))))

; INPUT/OUTPUT

(defun jsonread (filename)
  (with-open-file (stream filename
                          :direction :input
                          :if-does-not-exist :error)
    (jsonparse (fileToString stream))))

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
