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
;;; Loading data from the "hhea" table.
;;;
;;;  https://docs.microsoft.com/en-us/typography/opentype/spec/hhea
;;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6hhea.html
;;;
;;; $Id: hhea.lisp,v 1.4 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf2)

(defclass hhea-table ()
  ((version :initarg :version :reader version) ; 0x00010000 (1.0)
   (ascent :initarg :ascent :reader ascent) ; Distance from baseline of highest ascender
   (descent :initarg :descent :reader descent) ; Distance from baseline of lowest descender
   (line-gap :initarg :line-gap :reader line-gap) ; typographic line gap
   (advance-width-max :initarg :advance-width-max :reader advance-width-max) ; must be consistent with horizontal metrics
   (min-left-side-bearing :initarg :min-left-side-bearing :reader min-left-side-bearing) ; must be consistent with horizontal metrics
   (min-right-side-bearing :initarg :min-right-side-bearing :reader min-right-side-bearing) ; must be consistent with horizontal metrics
   (x-max-extent :initarg :x-max-extent :reader x-max-extent) ; max(lsb + (xMax-xMin))
   (caret-slope-rise :initarg :caret-slope-rise :reader caret-slope-rise) ; used to calculate the slope of the caret (rise/run) set to 1 for vertical caret
   (caret-slope-run :initarg :caret-slope-run :reader caret-slope-run) ; 0 for vertical
   (caret-offset :initarg :caret-offset :reader caret-offset) ; set value to 0 for non-slanted fonts
   (reserved1 :initarg :reserved1 :reader reserved1) ; set value to 0
   (reserved2 :initarg :reserved2 :reader reserved2) ; set value to 0
   (reserved3 :initarg :reserved3 :reader reserved3) ; set value to 0
   (reserved4 :initarg :reserved4 :reader reserved4) ; set value to 0
   (metric-data-format :initarg :metric-data-format :reader metric-data-format) ; 0 for current format
   (num-of-long-hor-metrics :initarg :num-of-long-hor-metrics :accessor num-of-long-hor-metrics))) ; number of advance widths in metrics table

(defmethod load-hhea-info ((font-loader font-loader))
  (seek-to-table "hhea" font-loader)
  (with-slots (input-stream hhea-table)
      font-loader
    (let ((version (read-fixed input-stream)))
      (setf hhea-table
        (make-instance 'hhea-table
          :version (prog1 version
                     (check-version "\"hhea\" table" version #x00010000))
          :ascent (read-fword input-stream)
          :descent (read-fword input-stream)
          :line-gap (read-fword input-stream)
          :advance-width-max (read-ufword input-stream)
          :min-left-side-bearing (read-fword input-stream)
          :min-right-side-bearing (read-fword input-stream)
          :x-max-extent (read-fword input-stream)
          :caret-slope-rise (read-int16 input-stream)
          :caret-slope-run (read-int16 input-stream)
          :caret-offset (read-fword input-stream)
          :reserved1 (read-int16 input-stream)
          :reserved2 (read-int16 input-stream)
          :reserved3 (read-int16 input-stream)
          :reserved4 (read-int16 input-stream)
          :metric-data-format (read-int16 input-stream)
          :num-of-long-hor-metrics (read-uint16 input-stream))))))


(defmethod ascender ((font-loader font-loader))
  (ascent (hhea-table font-loader)))

(defmethod descender ((font-loader font-loader))
  (descent (hhea-table font-loader)))

(defmethod line-gap ((font-loader font-loader))
  (line-gap (hhea-table font-loader)))

(defmethod horizontal-metrics-count ((font-loader font-loader))
  (with-slots (input-stream hhea-table)
      font-loader
    (num-of-long-hor-metrics hhea-table)))

(defmethod (setf horizontal-metrics-count) (value (font-loader font-loader))
  (with-slots (input-stream hhea-table)
      font-loader
    (setf (num-of-long-hor-metrics hhea-table) value)))

(defmethod dump-hhea-info ((font-loader font-loader) output-stream)
  (let ((table-position (table-position "hhea" font-loader))
        (file-position (file-position output-stream)))
    (unless (= table-position file-position)
      (warn "Table `hhea' position is missing ~A (~A)." table-position file-position)
      (seek-to-table "hhea" font-loader)))
  (when (boundp '*dump-character-list*)
    (setf (horizontal-metrics-count font-loader)
      (1+ (loop for index in (character-to-glyph-indexes *dump-character-list* font-loader)
              maximize index))))
  (with-slots (hhea-table)
      font-loader
    (write-fixed (version hhea-table) output-stream)
    (write-fword (ascent hhea-table) output-stream)
    (write-fword (descent hhea-table) output-stream)
    (write-fword (line-gap hhea-table) output-stream)
    (write-ufword (advance-width-max hhea-table) output-stream)
    (write-fword (min-left-side-bearing hhea-table) output-stream)
    (write-fword (min-right-side-bearing hhea-table) output-stream)
    (write-fword (x-max-extent hhea-table) output-stream)
    (write-int16 (caret-slope-rise hhea-table) output-stream)
    (write-int16 (caret-slope-run hhea-table) output-stream)
    (write-fword (caret-offset hhea-table) output-stream)
    (write-int16 (reserved1 hhea-table) output-stream)
    (write-int16 (reserved2 hhea-table) output-stream)
    (write-int16 (reserved3 hhea-table) output-stream)
    (write-int16 (reserved4 hhea-table) output-stream)
    (write-int16 (metric-data-format hhea-table) output-stream)
    (write-uint16 (num-of-long-hor-metrics hhea-table) output-stream)))
