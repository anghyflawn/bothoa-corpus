;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; bothoa-parser.lisp

(in-package #:bothoa-parser)

;;; "bothoa-parser" goes here. Hacks and glory await!

(defmacro find-in-corpus ((&key (corpus '*corpus*)) &body body)
  "Very simple macro to remove boilerplate for searching in the
   corpus. Takes an expression that is used for the test function on
   words. Available variables: word, word-list"
  `(remove-duplicates
    (remove-if-not (lambda (word-list)
                     (some (lambda (word)
                             ,@body)
                           word-list))
                   ,corpus
                   :key #'words)
    :test (lambda (x y)
	    (string= (print-entry x) (print-entry y)))))

(defmacro with-next (&body body)
  `(let ((next (next-element segment word)))
     (and next
	  ,@body)))


(defmacro with-2nd-next (&body body)
  `(with-next
     (let ((2nd-next (next-element next word)))
       (and 2nd-next
	    ,@body))))

(defmacro with-previous (&body body)
  `(let ((previous (previous-element segment word)))
     (and previous
	  ,@body)))

(defmacro find-entry-with-sequence ((&key (corpus '*corpus*)) sequence)
  `(find-in-corpus (:corpus ,corpus)
     (search (mapcar #'ipa-symbol (getf (parse (coerce ,sequence 'list))
					'segment-list))
	     (mapcar #'ipa-symbol (segments word)))))
		     
		      
(defmacro find-entry-with-stress-pattern ((&key (corpus '*corpus*)) stress-pattern)
  `(find-in-corpus (:corpus ,corpus)
     (equal ,stress-pattern (stress word))))
 
(defmacro find-vowel-sequence ((&key (corpus '*corpus*)) sym1 sym2)
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (vowel)
	     (and (equal (ipa-symbol vowel) ,sym1)
		  (equal (ipa-symbol (next-vowel vowel word)) ,sym2)))
	   (vowels word))))

(defmacro find-medial-vowels (&key (corpus '*corpus*))
  "This gives a list of all trisyllabic words with initial stress
   where the second vowel is not schwa or [i]. Changing the second
   form can be used to narrow down the search"
  `(find-in-corpus (:corpus ,corpus)
     (and (eql (length (vowels word)) 3)
	  (not (is-a (second (vowels word)) 'schwa 'i)) ; Comment this out...
;	  (is-a (second (vowels word)) 'o) ; ... to narrow down the search here
	  (equal (first (stress word)) 'm))))

(defmacro find-weak-vowels (vowel &key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
	     (and (not (stressed segment word))
		  (not (eq segment (car (last (vowels word)))))
		  (is-a segment ,vowel)))
	   (vowels word))))

(defmacro sequences-after-long-oe (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (vowel)
	     (and (equal (ipa-symbol vowel) 'schwa)
		  (stressed vowel word)
		  (long-p vowel)
		  (not (nasal-p vowel))
		  (not (member (ipa-symbol (next-element vowel word)) '(w j turned-h s)))
		  (consonant-p (next-element vowel word))
		  (consonant-p (next-element (next-element vowel word) word))))
	   (vowels word))))
     
;; FIXME: The next function is kind of ugly because not all methods
;; are set up to deal with null arguments.

(defmacro long-before-muta-cum-liquida (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
	     (with-2nd-next
	       (and (long-p segment) ; Loads of clauses here.
		    (not (nasal-p segment))
		    (consonant-p next)
		    (stressed segment word)
		    (is-a 2nd-next 'r 'l 'n 'm))))
	   (vowels word))))

(defmacro trisyllabic-final-stress (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (and (eql (length (vowels word)) 3)
	  (equal (stressed (third (vowels word)) word) 'm)
	  (not (long-p (third (vowels word)))))))

(defmacro disyllabic-final-stress (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (and (eql (length (vowels word)) 2)
	  (not (long-p (second (vowels word))))
	  (equal (stressed (second (vowels word)) word) 'm))))

(defmacro obstruent-sonorant-sequences (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
             (and (consonant-p segment)
                  (member (ipa-symbol segment) '(s z sh zh))
                  (not (null (next-element segment word)))
                  (consonant-p (next-element segment word))
                  (not (obstruent-p (next-element segment word)))))
           (segments word))))

(defmacro ns-clusters (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
	     (and (member (ipa-symbol segment) '(n m ng))
		  (member (ipa-symbol (next-element segment word)) '(f v s z sh zh h))))
;		  (null (next-element (next-element segment word) word))))
	   (segments word))))

(defmacro stop-sequences (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
	     (let ((stops '(p t k b d g ts dz))
		   (next (next-element segment word)))
	       (and (member (ipa-symbol segment) stops)
		    (member (ipa-symbol next) stops))))
	   (segments word))))

(defmacro fricative-sequences (&key (corpus '*corpus*))
    `(find-in-corpus (:corpus ,corpus)
       (some (lambda (segment)
  	       (let ((fricatives '(v s z sh zh h))
		     (next (next-element segment word)))
		 (and (member (ipa-symbol segment) fricatives)
		      (member (ipa-symbol next) fricatives))))
	   (segments word))))

(defmacro stop-fricative-sequences (&key (corpus '*corpus*))
    `(find-in-corpus (:corpus ,corpus)
       (some (lambda (segment)
  	       (let ((fricatives '(v s z sh zh h))
;		     (fricatives '(f v s z sh zh h)) ; commented out, because a lof of this is noise due to the conditional in -f
		     (stops '(p t k b d g ts dz))
		     (next (next-element segment word)))
		 (and (member (ipa-symbol segment) stops)
		      (member (ipa-symbol next) fricatives))))
	   (segments word))))

(defmacro final-v (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
	     (and (last-p segment word)
		  (is-a segment 'v)))
	   (segments word))))

(defmacro long-before-voiceless (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
		   (some (lambda (segment)
			   (and (vowel-p segment)
				(not (long-p segment))
				(not (nasal-p segment))
				(next-element segment word)
				(not (last-p (next-element segment word) word))
				(is-a (next-element segment word) 'h)))
			 (segments word))))

(defmacro short-nasal-vowels (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
		   (some (lambda (segment)
			   (and (vowel-p segment)
				(not (long-p segment))
				(nasal-p segment)
				(not (is-a segment 'a))))
			 (segments word))))

(defmacro liquids+voiceless-fricatives (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
		   (some (lambda (segment)
			   (and (is-a segment 'l 'r)
				(is-a (next-element segment word) 'f 's 'sh)))
			 (segments word))))

;; FIXME: ugly

(defun find-stress-patterns-all-lights (&key (corpus *corpus*))
  (let ((patterns (mapcar (lambda (word-list)
			    (mapcar #'stress (remove-if #'null word-list)))
			  (mapcar (lambda (entry)
				    (remove-if (lambda (word)
							 (member 'l (length-pattern word)))
						       (words entry)))
				  corpus)))
	(acc nil))
    (dolist (entry patterns)
      (dolist (pattern entry)
	(pushnew pattern acc :test #'equal)))
    acc))

(defmacro hh-words (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (> (count 'l (length-pattern word)) 1)))

(defmacro unstressed-diphthongs (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
	     (with-next
	       (and (not (stressed segment word))
		    (or (and (is-a segment 'e)
			     (is-a next 'j))
			(and (is-a segment 'a)
			     (is-a next 'w))))))
	   (vowels word))))

(defmacro schwa-second-closed-syllable (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (let ((vowels (vowels word)))
       (and (> (length vowels) 2)
	    (let* ((3rd-vowel-from-end (nth (- (length vowels) 3) vowels))
		   (2nd-vowel-from-end (nth (- (length vowels) 2) vowels))
		   (next (next-element 2nd-vowel-from-end word))
		   (2nd-next (next-element next word)))
	      (and (stressed 3rd-vowel-from-end word)
		   (is-a 2nd-vowel-from-end 'schwa)
		   (consonant-p next)
		   (consonant-p 2nd-next)))))))

(defmacro unstressed-nasal-vowels (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
	     (and (nasal-p segment)
		  (not (stressed segment word))))
	   (vowels word))))

(defmacro vowel-final-monosyllables (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (and (= 1 (length (vowels word)))
	  (let ((vowel (car (vowels word))))
	    (and (stressed vowel word)
		 (last-p vowel word))))))

(defmacro find-hiatus (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
	     (with-next
	       (and (is-a segment 'i 'y 'u)
		    (not (long-p segment))
		    (vowel-p next))))
	   (vowels word))))

(defmacro find-preconsonantal-glides (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
	     (with-next
	       (let ((previous  (previous-element segment word)))
		 (and (is-a segment 'w 'j 'turned-h)
		      (consonant-p next)
		      (or (not previous)
			  (not (vowel-p previous)))))))
	     (segments word))))

(defmacro find-prevocalic-y (&key (corpus '*corpus*))
  `(find-in-corpus (:corpus ,corpus)
     (some (lambda (segment)
	     (with-next
	       (with-previous
		 (and (is-a segment 'turned-h)
		      (not (long-p segment))
		      (vowel-p next)
		      (not (is-a previous 'dz 'ts))))))
	     (segments word))))