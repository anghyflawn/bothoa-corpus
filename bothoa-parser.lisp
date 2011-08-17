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
    :key #'print-entry))

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
	  (not (member (ipa-symbol (second (vowels word))) '(schwa i))) ; Comment this out...
;	  (equal (ipa-symbol (second (vowels word))) 'low-e) ; ... to narrow down the search here
	  (equal (first (stress word)) 'm))))

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
     (some (lambda (vowel)
	     (let* ((next (next-element vowel word))
		    (2nd-next (next-element next word)))
	       (and (long-p vowel) ; Loads of clauses here.
		    (not (nasal-p vowel))
		    (not (null next))
		    (not (null 2nd-next))
		    (consonant-p next)
		    (equal (stressed vowel word) 's)
		    (member (ipa-symbol 2nd-next) '(r l n m)))))
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
		  (member (ipa-symbol (next-element segment word)) '(f v s z sh zh h))
		  (null (next-element (next-element segment word) word))))
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