;;; Copyright (c) 2016 KURODA Hisao, All Rights Reserved

(in-package "ZPB-TTF")

(defclass gdef-table ()
  ((binary-data :initarg :binary-data :accessor binary-data)))

(defmethod load-gdef-info ((font-loader font-loader))
  (unless (null (table-info "GDEF" font-loader))
    (seek-to-table "GDEF" font-loader)
    (with-slots (input-stream gdef-table)
        font-loader
      (let* ((table-size (table-size "GDEF" font-loader))
             (binary-data (make-array table-size :element-type '(vector (unsigned-byte 8)))))
        (read-sequence binary-data input-stream)
        (setf gdef-table (make-instance 'gdef-table :binary-data binary-data))))))

(defmethod dump-gdef-info ((font-loader font-loader) output-stream)
  ;; (seek-to-position "GDEF" font-loader output-stream)
  (with-slots (input-stream gdef-table)
      font-loader
    (write-sequence (binary-data gdef-table) output-stream))
  (align-file-position output-stream))
