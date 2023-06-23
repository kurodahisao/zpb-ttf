;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;; Copyright (c) 2016 KURODA Hisao, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; Loading data from the "hmtx" table.
;;;
;;;  https://docs.microsoft.com/en-us/typography/opentype/spec/hmtx
;;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6hmtx.html
;;;
;;; $Id: hmtx.lisp,v 1.3 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf)

;;; A longHorMetric is defined by the C structure shown here:
;;; struct {
;;;     uint16 advanceWidth;
;;;     int16 leftSideBearing;
;;; }
(defclass long-hor-metric ()
  ((advance-width :initarg :advance-width :accessor advance-width)
   (left-side-bearing :initarg :left-side-bearing :accessor left-side-bearing)))

(defclass hmtx-table ()
  ((h-metrics :initarg :h-metrics :accessor h-metrics) ; [numOfLongHorMetrics] The value numOfLongHorMetrics comes from the 'hhea' table. If the font is monospaced, only one entry need be in the array but that entry is required.
   (left-side-bearing :initarg :left-side-bearing :accessor left-side-bearing))) ; [] Here the advanceWidth is assumed to be the same as the advanceWidth for the last entry above. The number of entries in this array is derived from the total number of glyphs minus numOfLongHorMetrics. This generally is used with a run of monospaced glyphs (e.g. Kanji fonts or Courier fonts). Only one run is allowed and it must be at the end.

(defmethod load-hmtx-info ((font-loader font-loader))
  (seek-to-table "hmtx" font-loader)
  (let ((horizontal-metrics-count (horizontal-metrics-count font-loader))
        #+ignore (left-side-bearing-count (- (glyph-count font-loader) horizontal-metrics-count))) ; ??
    (with-slots (input-stream hmtx-table)
        font-loader
      (let ((start-pos (file-position input-stream))
            (table-size (table-size "hmtx" font-loader)))
        (setf hmtx-table (make-instance 'hmtx-table
                           :h-metrics (make-array horizontal-metrics-count)
                           :left-side-bearing (make-array 0 :fill-pointer t :adjustable t)))
        (loop with h-metrics = (h-metrics hmtx-table)
            for i from 0 below (length h-metrics)
            for long-hor-metric = (make-instance 'long-hor-metric
                                    :advance-width (read-uint16 input-stream)
                                    :left-side-bearing (read-int16 input-stream))
            do (setf (svref h-metrics i) long-hor-metric))
        ;; ?? for i from 0 below (length left-side-bearing) ??
        (loop with max-size = (- table-size (- (file-position input-stream) start-pos))
            with left-side-bearing = (left-side-bearing hmtx-table)
            for size from 0 by 2 below max-size
            do (vector-push-extend (read-fword input-stream) left-side-bearing))))))

(defmethod dump-hmtx-info ((font-loader font-loader) output-stream)
  (let ((table-position (table-position "hmtx" font-loader))
        (file-position (file-position output-stream)))
    (unless (= table-position file-position)
      (warn "Table `hmtx' position is missing ~A (~A)." table-position file-position)
      (seek-to-table "hmtx" font-loader)))
  (let ((start-pos (file-position output-stream)))
    (with-slots (hmtx-table)
        font-loader
      (loop for i from 0 below (horizontal-metrics-count font-loader)
          for long-hor-metric across (h-metrics hmtx-table)
          do (write-uint16 (advance-width long-hor-metric) output-stream)
             (write-uint16 (left-side-bearing long-hor-metric) output-stream))
      #+ignore ;; ??
      (loop for left-side-bearing across (left-side-bearing hmtx-table)
          do (write-fword left-side-bearing output-stream)))
    (let ((end-pos (align-file-position output-stream)))
      (prog1
          end-pos
        (change-table-size "hmtx" (- end-pos start-pos) font-loader)))))
