(in-package #:bothoa-parser)

(defparameter *test-corpus* nil)

(make-corpus-file "test-corpus" *test-corpus-file*)

(defun make-test-corpus (&optional (filename *test-corpus-file*))
  (setf *test-corpus* nil)
  (iterate (for line in-file filename using #'read-line)
	   (pushnew (make-entry line) *test-corpus*)))

(make-test-corpus)

(define-test make-entry
  (assert-equal "tˈaːɲər" (print-entry (make-entry "ˈtaːɲər")))
  (assert-equal "tʃˈẽːzəɡˌon" (print-entry (make-entry "ˈtʃẽːzəˌɡon"))))

(define-test next-segment
  (assert-equal "e" (segment->string (let ((word (make-word "ten")))
				       (next-element (first (segments word)) word))))
  (assert-equal "ãː" (segment->string (let ((word (make-word "stʃãːs")))
					(next-element (second (segments word)) word))))
  (assert-equal "d" (segment->string (let* ((entry (make-entry "abc def"))
					    (word (first (words entry))))
				       (next-element (third (segments word)) entry)))))

(define-test stress
  (assert-equal 'm (let ((word (make-word "ˈtatəˌti")))
		     (stressed (second (segments word)) word)))
  (assert-equal 's (let ((word (make-word "ˈtatəˌti")))
		     (stressed (sixth (segments word)) word)))
  (assert-equal nil (let ((word (make-word "ˈtatəˌti")))
		      (stressed (fourth (segments word)) word))))

(define-test find-in-entry
  (assert-true (not (null (find-entry-with-sequence (:corpus *test-corpus*) "ar"))))
  (assert-true (not (null (find-entry-with-stress-pattern (:corpus *test-corpus*) '(m nil s)))))
  (assert-true (not (null (find-vowel-sequence (:corpus *test-corpus*) 'a 'a)))))

(define-test corpus-macros
  (assert-expands
   '(remove-duplicates
     (remove-if-not (lambda (word-list)
                      (some (lambda (word)
                              (eql (length (vowels word)) 2))
                            word-list))
      *corpus*
      :key #'words)
     :key #'print-entry)
   (find-in-corpus nil (eql (length (vowels word)) 2))))


(defun test-all ()
  (lisp-unit:run-all-tests :bothoa-parser))
