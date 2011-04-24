(defpackage #:squalor
  (:use #:cl)
  (:export #:*open-delimiter*
           #:*close-delimiter*
           #:*string-quote*
           #:*string-quote-escape*
           #:*search-metachars*
           #:*search-metachar-escape*
           #:enable-squalor-syntax
           #:sql))
