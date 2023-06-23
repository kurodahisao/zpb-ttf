;;; Copyright (c) 2016 KURODA Hisao, All Rights Reserved

(in-package "ZPB-TTF2")

(defclass prep-table ()
  ((binary-data :initarg :binary-data :accessor binary-data)))

(defmethod load-prep-info ((font-loader font-loader))
  (seek-to-table "prep" font-loader)
  (with-slots (input-stream prep-table)
      font-loader
    (let* ((table-size (table-size "prep" font-loader))
           (binary-data (make-array table-size :element-type '(vector (unsigned-byte 8)))))
      (read-sequence binary-data input-stream)
      (setf prep-table (make-instance 'prep-table :binary-data binary-data)))))

(defmethod dump-prep-info ((font-loader font-loader) output-stream)
  (let ((table-position (table-position "prep" font-loader))
        (file-position (file-position output-stream)))
    (unless (= table-position file-position)
      (warn "Table `prep' position is missing ~A (~A)." table-position file-position)
      (seek-to-table "prep" font-loader)))
  (with-slots (input-stream prep-table)
      font-loader
    (write-sequence (binary-data prep-table) output-stream))
  (align-file-position output-stream))
