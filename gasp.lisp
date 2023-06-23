;;; Copyright (c) 2016 KURODA Hisao, All Rights Reserved

(in-package "ZPB-TTF2")

(defclass gasp-table ()
  ((binary-data :initarg :binary-data :accessor binary-data)))

(defmethod load-gasp-info ((font-loader font-loader))
  (unless (null (table-info "gasp" font-loader))
    (seek-to-table "gasp" font-loader)
    (with-slots (input-stream gasp-table)
        font-loader
      (let* ((table-size (table-size "gasp" font-loader))
             (binary-data (make-array table-size :element-type '(vector (unsigned-byte 8)))))
        (read-sequence binary-data input-stream)
        (setf gasp-table (make-instance 'gasp-table :binary-data binary-data))))))

(defmethod dump-gasp-info ((font-loader font-loader) output-stream)
  ;; (seek-to-position "gasp" font-loader output-stream)
  (with-slots (input-stream gasp-table)
      font-loader
    (write-sequence (binary-data gasp-table) output-stream))
  (align-file-position output-stream))
