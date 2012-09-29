(in-package :ts)

(defun parse (stream)
  (ts.parser:parse stream))

(defun parse-file (filepath)
  (with-open-file (stream filepath 
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (ts.parser:parse stream)))
