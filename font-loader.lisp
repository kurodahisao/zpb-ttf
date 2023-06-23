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
;;; The font-loader object, which is the primary interface for
;;; getting glyph and metrics info.
;;;
;;; $Id: font-loader.lisp,v 1.26 2006/03/23 22:21:40 xach Exp $

(in-package #:zpb-ttf2)

(defvar *dump-character-list*)

(defclass font-loader ()
  ((tables :initform (make-hash-table) :reader tables)
   (input-stream :initarg :input-stream :accessor input-stream
          :documentation "The stream from which things are loaded.")
   (scaler-type :initarg :scaler-type :reader scaler-type)
   (table-count :initarg :table-count :reader table-count)
   (search-range :initarg :search-range :reader search-range)
   (entry-selector :initarg :entry-selector :reader entry-selector)
   (range-shift :initarg :range-shift :reader range-shift)
   ;; maxp table
   (maxp-table :initarg :maxp-table :reader maxp-table)
   ;; from the 'head' table ; definitions were moved to head.lisp
   ;; (units/em :accessor units/em)
   ;; (bounding-box :accessor bounding-box)
   ;; (loca-offset-format :accessor loca-offset-format)
   (head-table :initarg :head-table :accessor head-table)
   (gsub-table :initarg :gsub-table :accessor gsub-table)
   (gdef-table :initarg :gdef-table :accessor gdef-table)
   (os/2-table :initarg :os/2-table :accessor os/2-table)
   (cvt--table :initarg :cvt--table :accessor cvt--table)
   (fpgm-table :initarg :fpgm-table :accessor fpgm-table)
   (gasp-table :initarg :gasp-table :accessor gasp-table)
   (prep-table :initarg :prep-table :accessor prep-table)
   (vhea-table :initarg :vhea-table :accessor vhea-table)
   (vmtx-table :initarg :vmtx-table :accessor vmtx-table)
   (hmtx-table :initarg :hmtx-table :accessor hmtx-table)
   ;; from the 'loca' table ; definitions was moved to loca.lisp
   ;; (glyph-locations :accessor glyph-locations)
   (loca-table :initarg :loca-table :accessor loca-table)
   ;; from the 'cmap' table ; definitions was moved to loca.lisp
   ;; (character-map :accessor character-map)
   ;; (inverse-character-map :accessor inverse-character-map)
   (cmap-table :initarg :cmap-table :accessor cmap-table)
   ;; from the 'maxp' table ; definition was moved to maxp.lisp
   ;; (glyph-count :accessor glyph-count)
   ;; from the 'hhea' table  ; definition was moved to hhea.lisp
   ;; (ascender :accessor ascender)
   ;; (descender :accessor descender)
   ;; (line-gap :accessor line-gap)
   (hhea-table :initarg :hhea-table :accessor hhea-table)
   ;; from the 'hmtx' table
   (advance-widths :accessor advance-widths)
   (left-side-bearings :accessor left-side-bearings)
   ;; from the 'kern' table
   (kerning-table :initform (make-hash-table) :accessor kerning-table)
   ;; from the 'name' table ; definition was moved to name.lisp
   ;; (name-entries :initform nil :accessor name-entries)
   (name-table :initarg :name-table :accessor name-table)
   ;; from the 'post' table ; definitions were moved to post.lisp
   ;; (italic-angle :accessor italic-angle :initform 0)
   ;; (fixed-pitch-p :accessor fixed-pitch-p :initform nil)
   ;; (underline-position :accessor underline-position :initform 0)
   ;; (underline-thickness :accessor underline-thickness :initform 0)
   ;; (postscript-glyph-names :accessor postscript-glyph-names)
   (post-table :initarg :post-table :accessor post-table)
   (glyf-table :initarg :glyf-table :accessor glyf-table)
   ;; misc
   (glyph-cache :accessor glyph-cache)
   ;; # of fonts in collection, if loaded from a ttc file
   (collection-font-count :reader collection-font-count :initform nil
                          :initarg :collection-font-cont)
   ;; index of font in collection, if loaded from a ttc file
   (collection-font-index :reader collection-font-index :initform nil
                          :initarg :collection-font-index)))

(defclass table-info ()
  ((name :initarg :name :reader name)
   (checksum :initarg :checksum :accessor checksum)
   (offset :initarg :offset :reader offset)
   (size :initarg :size :reader size)))

(defmethod print-object ((object table-info) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "\"~A\"" (name object))))


;;; tag integers to strings and back

(defun number->tag (number)
  "Convert the 32-bit NUMBER to a string of four characters based on
the CODE-CHAR of each octet in the number."
  (let ((tag (make-string 4)))
    (loop for i below 4
          for offset from 24 downto 0 by 8
          do (setf (schar tag i)
                   (code-char (ldb (byte 8 offset) number))))
    tag))

(defun tag->number (tag)
  "Convert the four-character string TAG to a 32-bit number based on
the CHAR-CODE of each character."
  (declare (simple-string tag))
  (loop for char across tag
        for offset from 24 downto 0 by 8
        summing (ash (char-code char) offset)))


;;; Getting table info out of the loader

(defmethod table-info ((tag string) (font-loader font-loader))
  (gethash (tag->number tag) (tables font-loader)))

(defmethod table-exists-p (tag font-loader)
  (nth-value 1 (table-info tag font-loader)))

(defmethod table-position ((tag string) (font-loader font-loader))
  "Return the byte position in the font-loader's stream for the table
named by TAG."
  (let ((table-info (table-info tag font-loader)))
    (if table-info
        (offset table-info)
        (error "No such table -- ~A" tag))))

(defmethod table-size ((tag string) (font-loader font-loader))
  (let ((table-info (table-info tag font-loader)))
    (if table-info
        (size table-info)
        (error "No such table -- ~A" tag))))

(defmethod seek-to-table ((tag string) (font-loader font-loader))
  "Move FONT-LOADER's input stream to the start of the table named by TAG."
  (let ((table-info (table-info tag font-loader)))
    (if table-info
        (seek-to-table table-info font-loader)
        (error "No such table -- ~A" tag))))

(defmethod seek-to-table ((table table-info) (font-loader font-loader))
  "Move FONT-LOADER's input stream to the start of TABLE."
  (file-position (input-stream font-loader) (offset table)))
