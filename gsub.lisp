;;; Copyright (c) 2016 KURODA Hisao, All Rights Reserved

(in-package "ZPB-TTF2")

(defclass gsub-table ()
  ((binary-data :initarg :binary-data :accessor binary-data)))

(defmethod load-gsub-info ((font-loader font-loader))
  (unless (null (table-info "GSUB" font-loader))
    (seek-to-table "GSUB" font-loader)
    (with-slots (input-stream gsub-table)
        font-loader
      (let* ((table-size (table-size "GSUB" font-loader))
             (binary-data (make-array table-size :element-type '(vector (unsigned-byte 8)))))
        (read-sequence binary-data input-stream)
        (setf gsub-table (make-instance 'gsub-table :binary-data binary-data))))))

(defmethod dump-gsub-info ((font-loader font-loader) output-stream)
  ;; (seek-to-position "GSUB" font-loader output-stream)
  (with-slots (input-stream gsub-table)
      font-loader
    (write-sequence (binary-data gsub-table) output-stream))
  (align-file-position output-stream))
