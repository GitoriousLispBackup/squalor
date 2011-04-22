(in-package #:squalor)

(defvar *string-quote* #\'
  "Character used to quote strings in SQL")

(defvar *string-quote-escape* "'"
  "Escape sequence for quote characters within strings")

(defvar *search-metachars* "_%"
  "List of search pattern metacharacters")

(defvar *search-metachar-escape* "/"
  "Escape sequence for pattern metacharacters")

(defgeneric interpolate-scalar (value))

(defmethod interpolate-scalar ((value float))
  (princ-to-string value))

(defmethod interpolate-scalar ((value integer))
  (princ-to-string value))

(defmethod interpolate-scalar ((value ratio))
  (interpolate-scalar (float value)))

(defmethod interpolate-scalar ((value string))
  (with-output-to-string (s)
    (write-char *string-quote* s)
    (escape value (list *string-quote*) *string-quote-escape* s)
    (write-char *string-quote* s)))

(defun interpolate-sequence (seq)
  (format nil "~{~a~^,~}" (map 'list 'interpolate-scalar seq)))

(defun interpolate-relation (seq)
  (format nil "~{(~a)~^,~}" (map 'list 'interpolate-sequence seq)))

(defun interpolate-search-string (string)
  (with-output-to-string (s)
    (escape string *search-metachars* *search-metachar-escape* s)))

(defun escape (string escaped-chars escape-string stream)
  (loop for c across string do
       (when (find c escaped-chars)
         (write-string escape-string stream))
       (write-char c stream)))
