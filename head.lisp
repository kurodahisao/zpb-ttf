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
;;; Loading data from the "head" table.
;;;
;;;  https://docs.microsoft.com/en-us/typography/opentype/spec/head
;;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6head.html
;;;
;;; $Id: head.lisp,v 1.5 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf2)

(defclass head-table ()
  ((version :initarg :version :accessor version) ; 0x00010000 if (version 1.0)
   (font-revision :initarg :font-revision :accessor font-revision) ; set by font manufacturer
   (check-sum-adjustment :initarg :check-sum-adjustment :accessor check-sum-adjustment) ; To compute: set it to 0, calculate the checksum for the 'head' table and put it in the table directory, sum the entire font as a uint32_t, then store 0xB1B0AFBA - sum. (The checksum for the 'head' table will be wrong as a result. That is OK ; do not reset it.)
   (magic-number :initarg :magic-number :accessor magic-number) ; set to 0x5F0F3CF5
   (flags :initarg :flags :accessor flags) ; bit 0 - y value of 0 specifies baseline
                                        ; bit 1 - x position of left most black bit is LSB
                                        ; bit 2 - scaled point size and actual point size will differ (i.e. 24 point glyph differs from 12 point glyph scaled by factor of 2)
                                        ; bit 3 - use integer scaling instead of fractional
                                        ; bit 4 - (used by the Microsoft implementation of the TrueType scaler)
                                        ; bit 5 - This bit should be set in fonts that are intended to e laid out vertically, and in which the glyphs have been drawn such that an x-coordinate of 0 corresponds to the desired vertical baseline.
                                        ; bit 6 - This bit must be set to zero.
                                        ; bit 7 - This bit should be set if the font requires layout for correct linguistic rendering (e.g. Arabic fonts).
                                        ; bit 8 - This bit should be set for an AAT font which has one or more metamorphosis effects designated as happening by default.
                                        ; bit 9 - This bit should be set if the font contains any strong right-to-left glyphs.
                                        ; bit 10 - This bit should be set if the font contains Indic-style rearrangement effects.
                                        ; bits 11-13 - Defined by Adobe.
                                        ; bit 14 - This bit should be set if the glyphs in the font are simply generic symbols for code point ranges, such as for a last resort font.
   (units-perem :initarg :units-perem :accessor units-perem) ; range from 64 to 16384
   (created :initarg :created :accessor created) ; created international date
   (modified :initarg :modified :accessor modified) ; modified international date
   (xmin :initarg :xmin :accessor xmin) ; for all glyph bounding boxes
   (ymin :initarg :ymin :accessor ymin) ; for all glyph bounding boxes
   (xmax :initarg :xmax :accessor xmax) ; for all glyph bounding boxes
   (ymax :initarg :ymax :accessor ymax) ; for all glyph bounding boxes
   (mac-style :initarg :mac-style :accessor mac-style) ; bit 0 bold
                                        ; bit 1 italic
                                        ; bit 2 underline
                                        ; bit 3 outline
                                        ; bit 4 shadow
                                        ; bit 5 condensed (narrow)
                                        ; bit 6 extended
   (lowest-rec-ppem :initarg :lowest-rec-ppem :accessor lowest-rec-ppem) ; smallest readable size in pixels
   (font-direction-hint :initarg :font-direction-hint :accessor font-direction-hint) ; 0 Mixed directional glyphs
                                        ; 1 Only strongly left to right glyphs
                                        ; 2 Like 1 but also contains neutrals
                                        ; -1 Only strongly right to left glyphs
                                        ; -2 Like -1 but also contains neutrals
   (index-to-loc-format :initarg :index-to-loc-format :accessor index-to-loc-format) ; 0 for short offsets, 1 for long
   (glyph-data-format :initarg :glyph-data-format :accessor glyph-data-format))) ; 0 for current format

(defmethod load-head-info ((font-loader font-loader))
  (seek-to-table "head" font-loader)
  (with-slots (input-stream head-table)
      font-loader
    ;; version font-revision check-sum-adjustment magic-number
    ;; flags created modified xmin ymin xmax ymax flags mac-style
    ;; lowest-rec-ppem font-direction-hint index-to-loc-format glyph-data-format
    (setf head-table (make-instance 'head-table
                       :version (let ((version (read-uint32 input-stream)))
                                  (prog1 version
                                    (check-version "\"head\" table" version #x00010000)))
                       :font-revision (read-uint32 input-stream)
                       :check-sum-adjustment (read-uint32 input-stream)
                       :magic-number (let ((magic-number (read-uint32 input-stream)))
                                       (prog1 magic-number
                                         (when (/= magic-number #x5F0F3CF5)
                                           (error 'bad-magic
                                                  :location "\"head\" table"
                                                  :expected-values (list #x5F0F3CF5)
                                                  :actual-value magic-number))))
                       :flags (read-uint16 input-stream)
                       :units-perem (read-uint16 input-stream)
                       :created (read-int64 input-stream)
                       :modified (read-int64 input-stream)
                       :xmin (read-int16 input-stream)
                       :ymin (read-int16 input-stream)
                       :xmax (read-int16 input-stream)
                       :ymax (read-int16 input-stream)
                       :mac-style (read-int16 input-stream)
                       :lowest-rec-ppem (read-int16 input-stream)
                       :font-direction-hint (read-int16 input-stream)
                       :index-to-loc-format (read-int16 input-stream)
                       :glyph-data-format (read-int16 input-stream)))))

(defmethod units/em ((font-loader font-loader))
  (units-perem (head-table font-loader)))

(defmethod bounding-box ((font-loader font-loader))
  (with-slots (xmin ymin xmax ymax)
      (head-table font-loader)
    (vector xmin ymin xmax ymax)))

(defmethod loca-offset-format ((font-loader font-loader))
  (with-slots (index-to-loc-format)
      (head-table font-loader)
    (if (zerop index-to-loc-format) :short :long)))

(defmethod (setf loca-offset-format) (value (font-loader font-loader))
  (with-slots (index-to-loc-format)
      (head-table font-loader)
    (if (eq value :short)
        (setf index-to-loc-format 0)
      (setf index-to-loc-format 1))))

(defmethod dump-head-info ((font-loader font-loader) output-stream)
  (let ((table-position (table-position "head" font-loader))
        (file-position (file-position output-stream)))
    (unless (= table-position file-position)
      (warn "Table `head' position is missing ~A (~A)." table-position file-position)
      (seek-to-table "head" font-loader)))
  (setf (loca-offset-format font-loader) :short) ; to make font file smaller
  (with-slots (head-table)
      font-loader
    ;; version font-revision check-sum-adjustment magic-number
    ;; flags created modified xmin ymin xmax ymax flags mac-style
    ;; lowest-rec-ppem font-direction-hint index-to-loc-format glyph-data-format
    (write-uint32 (version head-table) output-stream)
    (write-uint32 (font-revision head-table) output-stream)
    (write-uint32 (check-sum-adjustment head-table) output-stream)
    (write-uint32 (magic-number head-table) output-stream)
    (write-uint16 (flags head-table) output-stream)
    (write-uint16 (units-perem head-table) output-stream)
    (write-int64 (created head-table) output-stream)
    (write-int64 (modified head-table) output-stream)
    (write-int16 (xmin head-table) output-stream)
    (write-int16 (ymin head-table) output-stream)
    (write-int16 (xmax head-table) output-stream)
    (write-int16 (ymax head-table) output-stream)
    (write-int16 (mac-style head-table) output-stream)
    (write-int16 (lowest-rec-ppem head-table) output-stream)
    (write-int16 (font-direction-hint head-table) output-stream)
    (write-int16 (index-to-loc-format head-table) output-stream)
    (write-int16 (glyph-data-format head-table) output-stream))
  (align-file-position output-stream))
