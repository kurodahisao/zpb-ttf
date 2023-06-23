;;; Copyright (c) 2016 KURODA Hisao, All Rights Reserved

(in-package "ZPB-TTF")

(defclass fpgm-table ()
  ((binary-data :initarg :binary-data :accessor binary-data)))

(defmethod load-fpgm-info ((font-loader font-loader))
  (seek-to-table "fpgm" font-loader)
  (with-slots (input-stream fpgm-table)
      font-loader
    (let* ((table-size (table-size "fpgm" font-loader))
           (binary-data (make-array table-size :element-type '(vector (unsigned-byte 8)))))
      (read-sequence binary-data input-stream)
      (setf fpgm-table (make-instance 'fpgm-table :binary-data binary-data)))))

(defmethod dump-fpgm-info ((font-loader font-loader) output-stream)
  ;; (seek-to-position "fpgm" font-loader output-stream)
  (with-slots (input-stream fpgm-table)
      font-loader
    (write-sequence (binary-data fpgm-table) output-stream))
  (align-file-position output-stream))
