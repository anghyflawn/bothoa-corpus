(in-package #:bothoa-corpus)

;;; Globals

(defparameter *corpus* nil)

(defun prompt-for-new-file ()
  (list (prompt "Input new file name: ")))

(defmacro make-corpus-file (filename corpus-variable)
  "Used to find static files."
   `(let ((corpus-file (cl-fad:file-exists-p (merge-pathnames (make-pathname :directory '(:relative "Dropbox" "alldocs" "thesis" "part2")
									     :name ,filename)
							      (user-homedir-pathname)))))
      (if corpus-file
	  (defparameter ,corpus-variable (namestring corpus-file))
	  (progn (defparameter ,corpus-variable nil)
		 (format t "Sorry, file ~A not found! Please change the path in the definition of make-corpus-file in io.lisp~%" ,filename)))))

(make-corpus-file "cb-corpus" *bothoa-corpus-file*)

;;; Globals that know about phonology

(defparameter *vowels* '(i y u high-e high-o e schwa o low-e low-o a))
(defparameter *consonants* '(b k d f g dz h turned-h j l m n nj ng p r s sh t ts v w z zh hm hn hl hr hj hturned-h))
(defparameter *sonorants* '(l r m n nj ng turned-h w j))
(defparameter *obstruents* (set-difference *consonants* *sonorants*))
(defparameter *ipa-alist*
  '((i . "i")
    (y . "y")
    (u . "u")
    (high-e . "e")
    (high-o . "o")
    (e . "ɛ")
    (o . "ɔ")
    (schwa . "ə")
    (low-e . "æ")
    (low-o . "ɒ")
    (a . "a")
    (b . "b")
    (k . "k")
    (d . "d")
    (f . "f")
    (g . "ɡ")
    (dz . "dʒ")
    (h . "h")
    (turned-h . "ɥ")
    (j . "j")
    (l . "l")
    (m . "m")
    (n . "n")
    (nj . "ɲ")
    (ng . "ŋ")
    (p . "p")
    (r . "r")
    (s . "s")
    (sh . "ʃ")
    (t . "t")
    (ts . "tʃ")
    (v . "v")
    (w . "w")
    (z . "z")
    (zh . "ʒ")
    (hm . "hm")
    (hn . "hn")
    (hl . "hl")
    (hr . "hr")
    (hj . "hj")
    (hturned-h . "hɥ")))

(defun symbols->text (symbol-list &optional (ipa-alist *ipa-alist*))
  (apply #'concatenate 'string (mapcar (lambda (symbol)
					 (symbol->str symbol ipa-alist))
				       symbol-list)))

(defun symbol->str (symbol &optional (ipa-alist *ipa-alist*))
  (cdr (assoc symbol ipa-alist)))

(defun str->symbol (str &optional (ipa-alist *ipa-alist*))
  (car (rassoc str ipa-alist :test #'equalp)))

(defun char->symbol (char &optional (ipa-alist *ipa-alist*))
  (str->symbol (string char) ipa-alist))

(defun parse (input-list &optional (output-list nil) (stress-list nil) (stress nil) (length-list nil))
  (if input-list
      (progn (case (car input-list)
	       (#\Modifier_Letter_Vertical_Line (setq stress 'm))
	       (#\Modifier_Letter_Triangular_Colon (setf (long-p (car output-list)) t))
	       (#\Combining_Tilde (setf (nasal-p (car output-list)) t))
	       (#\Modifier_Letter_Low_Vertical_Line (setq stress 's))
	       (#\Latin_Small_Letter_Esh (if (and (car output-list)
						  (eql (ipa-symbol (car output-list)) 't))
					     (setf (ipa-symbol (car output-list)) 'ts)
					     (push (make-instance 'consonant :symbol 'sh) output-list)))
	       (#\Latin_Small_Letter_Ezh (if (and (car output-list)
						  (eql (ipa-symbol (car output-list)) 'd))
					     (setf (ipa-symbol (car output-list)) 'dz)
					     (push (make-instance 'consonant :symbol 'zh) output-list)))
	       (otherwise (let* ((ipa-symbol (char->symbol (car input-list)))
				 (vowel-p (member ipa-symbol *vowels*)))
			    (push (make-instance (if vowel-p
						     'vowel
						     'consonant)
						 :symbol ipa-symbol)
				  output-list)
			    (when vowel-p
			      (push stress stress-list)
			      (setq stress nil)
			      (case (cadr input-list)
				(#\Modifier_Letter_Triangular_Colon (push 'l length-list))
				(#\Combining_Tilde (if (eql (caddr input-list) #\Modifier_Letter_Triangular_Colon)
						       (push 'l length-list)
						       (push 's length-list)))
				(otherwise (push 's length-list)))))))
	     (parse (cdr input-list) output-list stress-list stress length-list))
     (list :segment-list (nreverse output-list)
	   :stress-pattern (nreverse stress-list)
	   :length-pattern (nreverse length-list))))

(defun str->word (str)
   (parse (coerce str 'list)))

(defun make-word (str)
  (destructuring-bind (&key segment-list stress-pattern length-pattern) (str->word str)
    (make-instance 'word
		   :segments segment-list
		   :stress stress-pattern
		   :lengths length-pattern)))

(defun make-entry (str &optional (page 0))
  (make-instance 'entry
		 :words (mapcar #'make-word
				       (cl-ppcre:split "\\s" str))
		 :page page))

(defun make-corpus (&optional (filename *bothoa-corpus-file*))
  "Just iterate through the file line-by-line and push everything
that's not a page number into the corpus"
  (setq *corpus* nil)
  (let ((page-number nil))
    (iterate (for line in-file filename using #'read-line)
	     (if (every #'digit-char-p line)
		 (setq page-number (parse-integer line :junk-allowed t))
		 (pushnew (make-entry line page-number) *corpus* )))))

(defun print-query (entry-list &key (filename (merge-pathnames #p"result" bothoa-config:*base-directory*)))
  "Take a list of entries and print them out into the REPL and into a
result file"
  (with-open-file (file filename
                        :direction :output
                        :if-exists :supersede)
    (let ((result (with-output-to-string (str)
		    (format str "~{~A~%~}" (reverse (mapcar #'print-entry-with-page-number entry-list))))))
      (format file result)
      (format t result))))

(defmacro q (symbol)
  "I think I'm abusing something here, but it really saves typing"
  `(print-query (,symbol)))

;;; Make the corpus when loaded
(make-corpus)
