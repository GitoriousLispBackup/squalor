(in-package #:squalor)

(defvar *open-delimiter* #\{
  "Character used to start interpolation")

(defvar *close-delimiter* #\}
  "Character used to end interpolation")

(defvar *string-quote* #\'
  "Character used to quote strings in SQL")

(defvar *string-quote-escape* "'"
  "Escape sequence for quote characters within strings")

(defvar *search-metachars* "_%"
  "List of search pattern metacharacters")

(defvar *search-metachar-escape* "/"
  "Escape sequence for pattern metacharacters")

(defmacro sql (str)
  `(concatenate
    'string
    ,@(let ((start 0))
           (nconc
            (mapcan
             (lambda (pos)
               (prog1
                   (list (subseq str start (car pos))
                         (interpolate (subseq str (1+ (car pos)) (cdr pos))))
                 (setf start (1+ (cdr pos)))))
             (get-delimiter-positions str))
            (list (subseq str start))))))

(defun get-delimiter-positions (str)
  (let ((current-start nil)
        (positions ()))
    (loop for c across str for i from 0 do
         (if current-start
             (when (eql c *close-delimiter*)
               (push (cons current-start i) positions)
               (setf current-start nil))
             (when (eql c *open-delimiter*)
               (setf current-start i))))
    (nreverse positions)))

(defun interpolate (str)
  (list
   (ecase (aref str 0)
     (#\: 'interpolate-scalar)
     (#\@ 'interpolate-sequence)
     (#\# 'interpolate-relation)
     (#\$ 'interpolate-search-string))
   (read-from-string (subseq str 1))))

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
