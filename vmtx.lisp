;;; Copyright (c) 2016 KURODA Hisao, All Rights Reserved

(in-package "ZPB-TTF")

(defclass vertical-metrics-entry ()
  ((advance-height :initarg :advance-height :accessor advance-height) ; The advance height of the glyph. Signed integer in FUnits.
   (top-side-bearing :initarg :top-side-bearing :accessor top-side-bearing))) ; The top side bearing of the glyph. Signed integer in FUnits

(defclass vmtx-table ()
  ((v-metrics :initarg :v-metrics :accessor v-metrics) ; [numOfLongHorMetrics] The value numOfLongHorMetrics comes from the 'hhea' table. If the font is monospaced, only one entry need be in the array but that entry is required.
   (top-side-bearing :initarg :top-side-bearing :accessor top-side-bearing))) ; The top side bearing of the glyph. Signed integer in FUnits

(defmethod load-vmtx-info ((font-loader font-loader))
  (seek-to-table "vmtx" font-loader)
  (with-slots (input-stream vmtx-table)
      font-loader
    (let ((vertical-metrics-count (vertical-metrics-count font-loader))
          #+ignore (top-side-bearing (- (glyph-count font-loader) vertical-metrics-count))) ; ??
      (with-slots (input-stream vmtx-table)
          font-loader
        (let ((start-pos (file-position input-stream))
              (table-size (table-size "vmtx" font-loader)))
          (setf vmtx-table (make-instance 'vmtx-table
                             :v-metrics (make-array vertical-metrics-count)
                             :top-side-bearing (make-array 0 :fill-pointer t :adjustable t)))
          (loop with v-metrics = (v-metrics vmtx-table)
              for i from 0 below (length v-metrics)
              for vertical-metrics-entry = (make-instance 'vertical-metrics-entry
                                             :advance-height (read-uint16 input-stream)
                                             :top-side-bearing (read-int16 input-stream))
              do (setf (svref v-metrics i) vertical-metrics-entry))
          ;; ?? for i from 0 below (length left-side-bearing) ??
          (loop with max-size = (- table-size (- (file-position input-stream) start-pos))
              with top-side-bearing = (top-side-bearing vmtx-table)
              for size from 0 by 2 below max-size
              do (vector-push-extend (read-int16 input-stream) top-side-bearing)))))))

(defmethod dump-vmtx-info ((font-loader font-loader) output-stream)
  (let ((table-position (table-position "vmtx" font-loader))
        (file-position (file-position output-stream)))
    (unless (= table-position file-position)
      (warn "Table `vmtx' position is missing ~A (~A)." table-position file-position)
      (seek-to-table "vmtx" font-loader)))
  (let ((start-pos (file-position output-stream)))
    (with-slots (vmtx-table)
        font-loader
      (loop for i from 0 below (vertical-metrics-count font-loader)
          for vertical-metrics-entry across (v-metrics vmtx-table)
          do (write-uint16 (advance-height vertical-metrics-entry) output-stream)
             (write-uint16 (top-side-bearing vertical-metrics-entry) output-stream))
      #+ignore ;; ??
      (loop for top-side-bearing across (top-side-bearing vmtx-table)
          do (write-fword top-side-bearing output-stream)))
    (let ((end-pos (align-file-position output-stream)))
      (prog1
          end-pos
        (change-table-size "vmtx" (- end-pos start-pos) font-loader)))))
