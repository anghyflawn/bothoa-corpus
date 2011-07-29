(in-package #:bothoa-parser)

;;; Classes

(defclass word ()
  ((segment-list
    :initarg :segments
    :accessor segments
    :documentation "The list of segments, implemented as instance of the 'segment class, in the word")
   (stress-pattern
    :initarg :stress
    :accessor stress
    :documentation "A list of the symbols U, S, and M, for unstressed, secondary, and main, respectively")
   (length-pattern
    :initarg :lengths
    :accessor length-pattern
    :documentation "A list of the symbols L and H, for short and long vowels, respectively")))

(defclass segment ()
  ((ipa-symbol
    :initarg :symbol
    :accessor ipa-symbol
    :documentation "The Lisp symbol representing the segment")))

(defclass consonant (segment)
  ((obstruent
    :initarg :obs
    :initform nil
    :accessor obstruent-p
    :documentation "T if the consonant is an obstruent, nil if it is a sonorant")))

(defclass vowel (segment)
  ((long
    :initarg :long
    :initform nil
    :accessor long-p
    :documentation "T if the vowel is long, nil if it is short")
   (nasal
    :initarg :nasal
    :initform 'nil
    :accessor nasal-p
    :documentation "T if the vowel is nasal, nil if it is short")))

(defclass entry ()
  ((words
    :initarg :words
    :accessor words
    :documentation "List of word objects")))


;;; Methods

(defgeneric vowel-p (segment)
  (:documentation "Is the segment a vowel?"))

(defmethod vowel-p ((segment segment))
  (equal (type-of segment) 'vowel))

(defgeneric consonant-p (segment)
  (:documentation "Is the segment a consonant?"))

(defmethod consonant-p ((segment segment))
  (equal (type-of segment) 'consonant))

(defgeneric (setf ipa-symbol) (segment symbol)
  (:documentation "Automatic accessors choke on null inputs, so we roll our own"))

(defmethod (setf ipa-symbol) ((segment segment) symbol)
  (setf (slot-value segment 'ipa-symbol) symbol))

(defgeneric ipa-symbol (segment)
  (:documentation "Automatic accessors choke on null inputs, so we roll our own"))

(defmethod ipa-symbol ((segment segment))
  (slot-value segment 'ipa-symbol))

(defmethod ipa-symbol ((segment null))
  (declare (ignore segment))
  nil)

(defgeneric ipa (segment)
  (:documentation "Print the IPA representation of the segment"))

(defmethod ipa (segment)
  (symbol->str (ipa-symbol segment)))

(defgeneric segment->string (segment)
  (:documentation "Print the string representation of the segment"))

(defmethod segment->string ((segment consonant))
  (ipa segment))

(defmethod segment->string ((segment vowel))
  (concatenate 'string
	       (ipa segment)
	       (when (nasal-p segment)
		 "̃")
	       (when (long-p segment)
		 "ː")))

(defgeneric word->string (word)
  (:documentation "Print the string representation of the word"))


(defmethod word->string ((word word))
  (labels ((print-segment (segment-list stress-pattern output-list)
	     (if segment-list
		 (progn
		   (when (member (ipa-symbol (car segment-list)) *vowels*)
		     (case (car stress-pattern)
		       ('m (push (string #\Modifier_Letter_Vertical_Line) output-list))
		       ('s (push (string #\Modifier_Letter_Low_Vertical_Line) output-list)))
		     (setq stress-pattern (cdr stress-pattern)))
		   (push (segment->string (car segment-list)) output-list)
		   (print-segment (cdr segment-list) stress-pattern output-list))
		 (nreverse output-list))))
    (apply #'concatenate 'string (print-segment (segments word) (stress word) nil))))
		   
(defgeneric print-entry (entry &key strict)
  (:documentation "Pretty-print an entry"))

(defmethod print-entry ((entry entry) &key (strict nil))
  (let ((format-string (if strict
			   "~{~A~}"
			   "~{~A~^ ~}")))
    (with-output-to-string (stream)
      (format stream format-string (mapcar #'word->string (words entry))))))

(defgeneric next-element (element higher-element)
  (:documentation
   "Get the next element within the higher element,
e.g. the next segment in a word etc."))

(defmethod next-element ((segment segment) (word word))
  (let ((segments (segments word)))
    (nth (1+ (position segment segments)) segments)))

(defmethod next-element ((word word) (entry entry))
  (let ((words (words entry)))
    (nth (1+ (position word words)) words)))

(defmethod next-element ((segment segment) (entry entry))
  (let ((segment-list (entry-segment-list entry)))
    (nth (1+ (position segment segment-list)) segment-list)))

(defmethod next-element ((segment null) word)
  nil)

(defgeneric next-vowel (vowel word)
  (:documentation "Get the vowel of the next syllable"))

(defmethod next-vowel ((vowel vowel) (word word))
  (let ((vowel-list (vowels word)))
    (nth (1+ (position vowel vowel-list)) vowel-list)))

(defgeneric entry-segment-list (entry)
  (:documentation "The list of all segments in an entry"))

(defmethod entry-segment-list ((entry entry))
  (apply #'append (mapcar #'segments (words entry))))

(defgeneric stressed (segment word)
  (:documentation "Returns M, S or NIL depending on whether the segment is stressed"))

(defmethod stressed ((segment vowel) (word word))
  (nth (position segment (vowels word)) (stress word)))

(defgeneric vowels (word)
  (:documentation "Get the list of vowels in a word"))

(defmethod vowels ((word word))
  (remove-if-not #'vowel-p (segments word)))

