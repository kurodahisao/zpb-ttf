;;; Copyright (c) 2016 KURODA Hisao, All Rights Reserved

(in-package "ZPB-TTF")

(defclass cvt--table ()
  ((binary-data :initarg :binary-data :accessor binary-data)))

(defmethod load-cvt--info ((font-loader font-loader))
  (seek-to-table "cvt " font-loader)
  (with-slots (input-stream cvt--table)
      font-loader
    (let* ((table-size (table-size "cvt " font-loader))
           (binary-data (make-array table-size :element-type '(vector (unsigned-byte 8)))))
      (read-sequence binary-data input-stream)
      (setf cvt--table (make-instance 'cvt--table :binary-data binary-data)))))

(defmethod dump-cvt--info ((font-loader font-loader) output-stream)
  (with-slots (input-stream cvt--table)
      font-loader
    (write-sequence (binary-data cvt--table) output-stream))
  (align-file-position output-stream))
