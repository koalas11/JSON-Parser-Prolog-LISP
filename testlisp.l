(defun string-to-json (JSONString)
  (if (eql (char JSONString 0) #\SPACE) (string-to-json (subseq JSONString 1))
    (let ((var (length JSONString)))
      (cond ((= var 0) nil)
            ((and (eql #\{ (char JSONString 0)) (eql #\} (char JSONString (- var 1))))
             (cons 'JSONOBJ (get-members (subseq JSONString 1 (- var 1)))))
            ((and (eql #\[ (char JSONString 0)) (eql #\] (char JSONString (- var 1))))
             (cons 'JSONARRAY (get-values (subseq JSONString 1 (- var 1)))))
            (t (error "WUT"))))))

(defun get-members (Members)
  (if (= (length Members) 0) nil
    (let ((var (position #\, Members)))
      (cons (parse-member (subseq Members 0 var))
            (if (null var) nil (get-members (subseq Members (+ var 1))))))))

(defun get-values (Values)
  (if (= (length Values) 0) nil
    (let* ((var (position #\, Values))
          (var2 (position #\] Values))
          (var3 (position #\} Values)))
      (cond ((eql (char Values 0) #\[)
             (if (null var2) (error "NO U 2")
               (cons (parse-value (subseq Values 0 (+ var2 1)))
                     (get-values (subseq Values (+ var2 1))))))
            ((eql (char Values 0) #\{)
             (if (null var3) (error "NO U 3")
               (cons (parse-value (subseq Values 0 (+ var3 1)))
                     (get-values (subseq Values (+ var3 1))))))
            (t (cons (parse-value (subseq Values 0 var))
                     (if (null var) nil
                       (get-values (subseq Values (+ var 1))))))))))

(defun parse-member (Member)
  (let ((var (position #\: Member)))
    (if (not (null (is-string (subseq Member 0 var))))
        (list (is-string (subseq Member 0 var))
              (parse-value (subseq Member (+ var 1))))
      (error "NOOO"))))

(defun parse-value (Value)
  (cond ((not (null (is-string Value)))
         (is-string Value))
        ((floatp (read-from-string Value))
         (parse-float Value))
        ((not (null (parse-integer Value :junk-allowed t)))
         (parse-integer Value))
        (t (string-to-json Value))))

(defun is-string (String)
  (let* ((var (string-trim '(#\Space #\Tab #\Newline) String))
         (var2 (length var)))
    (if (< var2 2) nil
      (if (and (not (null (search "\"" var :end2 2))) 
               (not (null (search "\"" var :start2 (- var2 1)))))
          (replaceEscapes (subseq var 1 (- var2 1)))
        nil))))

(defun replaceEscapes (String)
  (let ((var (search "\\\"" String)))
    (if (null var) String
      (concatenate 'string (subseq String 0 var) "\"" (subseq String (+ var 2))))))
    
  