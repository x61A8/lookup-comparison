;;;; Comparing 4 methods of checking if a string exists in a dictionary
;;;; (lookup query-string dictionary) -> t or nil

(defpackage :static-lookup
  (:use :cl))

(in-package :static-lookup)

(defparameter *dictionary* (list "False" "await" "else" "import" "pass"
				 "None" "break" "except" "in" "raise"
				 "True" "class" "finally" "is" "return"
				 "and" "continue" "for" "lambda" "try"
				 "as" "def" "from" "nonlocal" "while"
				 "assert" "del" "global" "not" "with"
				 "async" "elif" "if" "or" "yield"

				 "+" "-" "*" "**" "/" "//" "%" "@" "<<"
				 ">>" "&" "|" "^" "~" ":=" "<" ">" "<="
				 ">=" "==" "!="

				 "(" ")" "[" "]" "{" "}" "," ":" "." ";"
				 "=" "->" "+=" "-=" "*=" "/=" "//=" "%="
				 "@=" "&=" "|=" "^=" ">>=" "<<=" "**=")
  "Python keywords, operators, and delimiters.")

;;; 1. Linear scan of a list
(defun list-lookup (query-string list-dict)
  (if (find query-string list-dict :test #'equal)
      t
      nil))

;;; 2. Lookup in hash table
(defun construct-table (list-dict)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (word list-dict table)
      (setf (gethash word table) t))))

(defun table-lookup (query-string table-dict)
  (declare (optimize speed (debug 0) (safety 0)
		     (compilation-speed 0)
		     (space 0)))
  (declare (simple-string query-string))
  (gethash query-string table-dict))

;;; 3. Lookup in trie at runtime
(defstruct trie-node value terminal? children)
(defun trie-add-string (string trie)
  (let ((current-trie trie))
    (loop for c across string
	  do  (let ((next-trie (find c
				     (trie-node-children current-trie)
				     :key #'trie-node-value
				     :test #'char=)))
		(unless next-trie
		  (setf next-trie (make-trie-node :value c
						  :terminal? nil
						  :children nil))
		  (setf (trie-node-children current-trie)
			(cons next-trie (trie-node-children current-trie))))
		(setf current-trie next-trie)))
    (setf (trie-node-terminal? current-trie) t)))

(defun construct-trie (list-dict)
  (let ((trie (make-trie-node :value nil :terminal? nil :children nil)))
    (dolist (word list-dict trie)
      (trie-add-string word trie))))

(defun trie-lookup (query-string trie-dict)
  (declare (optimize speed (debug 0) (safety 0)
		     (compilation-speed 0)
		     (space 0)))
  (declare (simple-string query-string))
  (let ((current-trie trie-dict))
    (loop for c across query-string
	  do (let ((next-trie (find c
				    (trie-node-children current-trie)
				    :key #'trie-node-value
				    :test #'char=)))
	       (unless next-trie
		 (return-from trie-lookup nil))
	       (setf current-trie next-trie)))
    (trie-node-terminal? current-trie)))

;;; 4. Compile trie into lookup function
(defun trie->cond (trie input-symbol depth)
  `(cond ((= (length ,input-symbol) ,depth) ,(if (trie-node-terminal? trie)
						 t
						 nil))
	 ,@(mapcar (lambda (sub-trie)
		     `((char= (char ,input-symbol ,depth)
			      ,(trie-node-value sub-trie))
		       ,(trie->cond sub-trie input-symbol (1+ depth))))
		   (trie-node-children trie))
	 (t nil)))

(defun construct-lookup-func (list-dict)
  (let ((trie (construct-trie list-dict)))
    (compile nil `(lambda (query-string)
		    (declare (optimize speed (debug 0) (safety 0)
				       (compilation-speed 0)
				       (space 0)))
		    (declare (simple-string query-string))
		    ,(trie->cond trie 'query-string 0)))))

;;; Comparisons
(defun generate-random-string (length)
  (let ((chars (loop for char from (char-code #\a) to (char-code #\z)
		     collecting (code-char char)))
	(output (make-string length)))
    (dotimes (i length output)
      (setf (char output i)
	    (nth (random (length chars)) chars)))))

(defun sample-strings (num-samples list-dict)
  (let ((output (make-array num-samples)))
    (dotimes (i num-samples output)
      (setf (aref output i)
	    (if (= (random 2) 0)
		(generate-random-string 5)
		(nth (random (length list-dict)) list-dict))))))

(defun print-result (name start-time end-time hits)
  (format t "~A: ~,3Fs, ~A hits~%"
	  name
	  (/ (* 1.0 (- end-time start-time)) internal-time-units-per-second)
	  hits))

(defun compare (num-samples)
  (let ((list-dict (copy-list *dictionary*))
	(table-dict (construct-table *dictionary*))
	(trie-dict (construct-trie *dictionary*))
	(lookup-func (construct-lookup-func *dictionary*))
	list-start list-end (list-hits 0)
	table-start table-end (table-hits 0)
	trie-start trie-end (trie-hits 0)
	func-start func-end (func-hits 0)
	(sample-inputs (sample-strings num-samples *dictionary*)))
    ;; List
    (setf list-start (get-internal-real-time))
    (loop for sample across sample-inputs
	  do (when (list-lookup sample list-dict)
	       (incf list-hits)))
    (setf list-end (get-internal-real-time))
    (print-result "List" list-start list-end list-hits)

    ;; Table
    (setf table-start (get-internal-real-time))
    (loop for sample across sample-inputs
	  do (when (table-lookup sample table-dict)
	       (incf table-hits)))
    (setf table-end (get-internal-real-time))
    (print-result "Table" table-start table-end table-hits)

    ;; Trie
    (setf trie-start (get-internal-real-time))
    (loop for sample across sample-inputs
	  do (when (trie-lookup sample trie-dict)
	       (incf trie-hits)))
    (setf trie-end (get-internal-real-time))
    (print-result "Trie" trie-start trie-end trie-hits)

    ;; Compiled Function
    (setf func-start (get-internal-real-time))
    (loop for sample across sample-inputs
	  do (when (funcall lookup-func sample)
	       (incf func-hits)))
    (setf func-end (get-internal-real-time))
    (print-result "Func" func-start func-end func-hits)))
