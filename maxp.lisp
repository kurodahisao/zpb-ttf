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
;;; Loading data from the "maxp" table.
;;;
;;;  https://docs.microsoft.com/en-us/typography/opentype/spec/maxp
;;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6maxp.html
;;;
;;; $Id: maxp.lisp,v 1.3 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf)

(defclass maxp-table ()
  ((version :initarg :version :reader version) ; 0x00010000 (1.0)
   (num-glyphs :initarg :num-glyphs :accessor num-glyphs) ; the number of glyphs in the font
   (max-points :initarg :max-points :reader max-points) ; points in non-compound glyph
   (max-contours :initarg :max-contours :reader max-contours) ; contours in non-compound glyph
   (max-componentpoints :initarg :max-componentpoints :reader max-componentpoints) ; points in compound glyph
   (max-componentcontours :initarg :max-componentcontours :reader max-componentcontours) ; contours in compound glyph
   (max-zones :initarg :max-zones :reader max-zones) ; set to 2
   (max-twilightpoints :initarg :max-twilightpoints :reader max-twilightpoints) ; points used in Twilight Zone (Z0)
   (max-storage :initarg :max-storage :reader max-storage) ; number of Storage Area locations
   (max-functiondefs :initarg :max-functiondefs :reader max-functiondefs) ; number of FDEFs
   (max-instructiondefs :initarg :max-instructiondefs :reader max-instructiondefs) ; number of IDEFs
   (max-stackelements :initarg :max-stackelements :reader max-stackelements) ; maximum stack depth
   (max-sizeofinstructions :initarg :max-sizeofinstructions :reader max-sizeofinstructions) ; byte count for glyph instructions
   (max-componentelements :initarg :max-componentelements :reader max-componentelements) ; number of glyphs referenced at top level
   (max-componentdepth :initarg :max-componentdepth :reader max-componentdepth))) ; levels of recursion, set to 0 if font has only simple glyphs

(defmethod load-maxp-info ((font-loader font-loader))
  (seek-to-table "maxp" font-loader)
  (with-slots (input-stream maxp-table) font-loader
    (let ((version (read-uint32 input-stream)))
      (check-version "\"maxp\" table" version #x00010000)
      (setf maxp-table (make-instance 'maxp-table
                         :version version
                         :num-glyphs (read-uint16 input-stream)
                         :max-points (read-uint16 input-stream)
                         :max-contours (read-uint16 input-stream)
                         :max-componentpoints (read-uint16 input-stream)
                         :max-componentcontours (read-uint16 input-stream)
                         :max-zones (read-uint16 input-stream)
                         :max-twilightpoints (read-uint16 input-stream)
                         :max-storage (read-uint16 input-stream)
                         :max-functiondefs (read-uint16 input-stream)
                         :max-instructiondefs (read-uint16 input-stream)
                         :max-stackelements (read-uint16 input-stream)
                         :max-sizeofinstructions (read-uint16 input-stream)
                         :max-componentelements (read-uint16 input-stream)
                         :max-componentdepth (read-uint16 input-stream))))))

(defmethod glyph-count ((font-loader font-loader))
  (num-glyphs (maxp-table font-loader)))

(defmethod (setf glyph-count) (value (font-loader font-loader))
  (setf (num-glyphs (maxp-table font-loader)) value))

(defmethod dump-maxp-info ((font-loader font-loader) output-stream)
  (let ((table-position (table-position "maxp" font-loader))
        (file-position (file-position output-stream)))
    (unless (= table-position file-position)
      (warn "Table `maxp' position is missing ~A (~A)." table-position file-position)
      (seek-to-table "maxp" font-loader)))
  (with-slots (maxp-table)
      font-loader
    (write-uint32 (version maxp-table) output-stream)
    (write-uint16 (num-glyphs maxp-table) output-stream)
    (write-uint16 (max-points maxp-table) output-stream)
    (write-uint16 (max-contours maxp-table) output-stream)
    (write-uint16 (max-componentpoints maxp-table) output-stream)
    (write-uint16 (max-componentcontours maxp-table) output-stream)
    (write-uint16 (max-zones maxp-table) output-stream)
    (write-uint16 (max-twilightpoints maxp-table) output-stream)
    (write-uint16 (max-storage maxp-table) output-stream)
    (write-uint16 (max-functiondefs maxp-table) output-stream)
    (write-uint16 (max-instructiondefs maxp-table) output-stream)
    (write-uint16 (max-stackelements maxp-table) output-stream)
    (write-uint16 (max-sizeofinstructions maxp-table) output-stream)
    (write-uint16 (max-componentelements maxp-table) output-stream)
    (write-uint16 (max-componentdepth maxp-table) output-stream))
  (align-file-position output-stream))
