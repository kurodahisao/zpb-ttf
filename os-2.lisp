;;; Copyright (c) 2016 KURODA Hisao, All Rights Reserved

(in-package "ZPB-TTF2")

(defclass os/2-table ()
  ((binary-data :initarg :binary-data :accessor binary-data)))

(defmethod load-os/2-info ((font-loader font-loader))
  (unless (null (table-info "OS/2" font-loader))
    (seek-to-table "OS/2" font-loader)
    (with-slots (input-stream os/2-table)
        font-loader
      (let* ((table-size (table-size "OS/2" font-loader))
             (binary-data (make-array table-size :element-type '(vector (unsigned-byte 8)))))
        (read-sequence binary-data input-stream)
        (setf os/2-table (make-instance 'os/2-table :binary-data binary-data))))))

(defmethod dump-os/2-info ((font-loader font-loader) output-stream)
  ;; (seek-to-position "OS/2" font-loader output-stream)
  (with-slots (input-stream os/2-table)
      font-loader
    (write-sequence (binary-data os/2-table) output-stream))
  (align-file-position output-stream))
