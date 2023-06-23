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
;;; "post" table functions
;;;
;;;   https://docs.microsoft.com/en-us/typography/opentype/spec/post
;;;   http://developer.apple.com/fonts/TTRefMan/RM06/Chap6post.html
;;;
;;; $Id: post.lisp,v 1.7 2006/11/09 15:06:16 xach Exp $

(in-package #:zpb-ttf2)

(defvar *standard-mac-glyph-names*
  #(".notdef"
    ".null"
    "nonmarkingreturn"
    "space"
    "exclam"
    "quotedbl"
    "numbersign"
    "dollar"
    "percent"
    "ampersand"
    "quotesingle"
    "parenleft"
    "parenright"
    "asterisk"
    "plus"
    "comma"
    "hyphen"
    "period"
    "slash"
    "zero" "one" "two" "three" "four"
    "five" "six" "seven" "eight" "nine"
    "colon"
    "semicolon"
    "less"
    "equal"
    "greater"
    "question"
    "at"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
    "bracketleft"
    "backslash"
    "bracketright"
    "asciicircum"
    "underscore"
    "grave"
    "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "braceleft"
    "bar"
    "braceright"
    "asciitilde"
    "Adieresis"
    "Aring"
    "Ccedilla"
    "Eacute"
    "Ntilde"
    "Odieresis"
    "Udieresis"
    "aacute"
    "agrave"
    "acircumflex"
    "adieresis"
    "atilde"
    "aring"
    "ccedilla"
    "eacute"
    "egrave"
    "ecircumflex"
    "edieresis"
    "iacute"
    "igrave"
    "icircumflex"
    "idieresis"
    "ntilde"
    "oacute"
    "ograve"
    "ocircumflex"
    "odieresis"
    "otilde"
    "uacute"
    "ugrave"
    "ucircumflex"
    "udieresis"
    "dagger"
    "degree"
    "cent"
    "sterling"
    "section"
    "bullet"
    "paragraph"
    "germandbls"
    "registered"
    "copyright"
    "trademark"
    "acute"
    "dieresis"
    "notequal"
    "AE"
    "Oslash"
    "infinity"
    "plusminus"
    "lessequal"
    "greaterequal"
    "yen"
    "mu"
    "partialdiff"
    "summation"
    "product"
    "pi"
    "integral"
    "ordfeminine"
    "ordmasculine"
    "Omega"
    "ae"
    "oslash"
    "questiondown"
    "exclamdown"
    "logicalnot"
    "radical"
    "florin"
    "approxequal"
    "Delta"
    "guillemotleft"
    "guillemotright"
    "ellipsis"
    "nonbreakingspace"
    "Agrave"
    "Atilde"
    "Otilde"
    "OE"
    "oe"
    "endash"
    "emdash"
    "quotedblleft"
    "quotedblright"
    "quoteleft"
    "quoteright"
    "divide"
    "lozenge"
    "ydieresis"
    "Ydieresis"
    "fraction"
    "currency"
    "guilsinglleft"
    "guilsinglright"
    "fi"
    "fl"
    "daggerdbl"
    "periodcentered"
    "quotesinglbase"
    "quotedblbase"
    "perthousand"
    "Acircumflex"
    "Ecircumflex"
    "Aacute"
    "Edieresis"
    "Egrave"
    "Iacute"
    "Icircumflex"
    "Idieresis"
    "Igrave"
    "Oacute"
    "Ocircumflex"
    "apple"
    "Ograve"
    "Uacute"
    "Ucircumflex"
    "Ugrave"
    "dotlessi"
    "circumflex"
    "tilde"
    "macron"
    "breve"
    "dotaccent"
    "ring"
    "cedilla"
    "hungarumlaut"
    "ogonek"
    "caron"
    "Lslash"
    "lslash"
    "Scaron"
    "scaron"
    "Zcaron"
    "zcaron"
    "brokenbar"
    "Eth"
    "eth"
    "Yacute"
    "yacute"
    "Thorn"
    "thorn"
    "minus"
    "multiply"
    "onesuperior"
    "twosuperior"
    "threesuperior"
    "onehalf"
    "onequarter"
    "threequarters"
    "franc"
    "Gbreve"
    "gbreve"
    "Idotaccent"
    "Scedilla"
    "scedilla"
    "Cacute"
    "cacute"
    "Ccaron"
    "ccaron"
    "dcroat"))

(defclass post-table ()
  ((post-format :initarg :post-format :accessor post-format) ; Format of this table
   (italic-angle :initarg :italic-angle :accessor italic-angle) ; Italic angle in degrees
   (underline-position :initarg :underline-position :accessor underline-position) ; Underline position
   (underline-thickness :initarg :underline-thickness :accessor underline-thickness) ; Underline thickness
   (is-fixed-pitch :initarg :is-fixed-pitch :accessor is-fixed-pitch) ; Font is monospaced      ; set to 1 if the font is monospaced and 0 otherwise (N.B., to maintain compatibility with older versions of the TrueType spec, accept any non-zero value as meaning that the font is monospaced)
   (min-mem-type-42 :initarg :min-mem-type-42 :accessor min-mem-type-42) ; Minimum memory usage when a TrueType font is downloaded as a Type 42 font
   (max-mem-type-42 :initarg :max-mem-type-42 :accessor max-mem-type-42) ; Maximum memory usage when a TrueType font is downloaded as a Type 42 font
   (min-mem-type-1 :initarg :min-mem-type-1 :accessor min-mem-type-1) ; Minimum memory usage when a TrueType font is downloaded as a Type 1 font
   (max-mem-type-1 :initarg :max-mem-type-1 :accessor max-mem-type-1) ; Maximum memory usage when a TrueType font is downloaded as a Type 1 font
   (post-format-object :initarg :post-format-object :accessor post-format-object)))

(defclass post-format-2 ()
  ((number-of-glyphs :initarg :number-of-glyphs :accessor number-of-glyphs) ; number of glyphs
   (glyph-name-index :initarg :glyph-name-index :accessor glyph-name-index) ; Ordinal number of this glyph in 'post' string tables. This is not an offset.
   (names :initarg :names :accessor names) ; glyph names with length bytes [variable] (a Pascal string)
   (glyph-names :initarg :glyph-names :accessor glyph-names))) ; the indexes are replaced with names.

(defmethod load-post-info ((font-loader font-loader))
  (let ((names (make-array (glyph-count font-loader) :initial-element 0))
        (stream (input-stream font-loader)))
    (seek-to-table "post" font-loader)
    (let ((format (read-fixed stream)))
      (when (/= format #x00020000 #x00030000)
        (error 'unsupported-format
               :location "\"post\" table"
               :expected-values (list #x00020000 #x00030000)
               :actual-value format))
      (setf (post-table font-loader)
        (make-instance 'post-table
          :post-format format
          :italic-angle (read-fixed stream)
          :underline-position (read-fword stream)
          :underline-thickness (read-fword stream)
          :is-fixed-pitch (read-uint32 stream)
          :min-mem-type-42 (read-uint32 stream)
          :max-mem-type-42 (read-uint32 stream)
          :min-mem-type-1 (read-uint32 stream)
          :max-mem-type-1 (read-uint32 stream)
          :post-format-object (case format
                                (#x00020000 (load-post-format-2 names stream))
                                (#x00030000 (load-post-format-3 names stream))))))))

(defmethod italic-angle ((font-loader font-loader))
  (italic-angle (post-table font-loader)))

(defmethod underline-position ((font-loader font-loader))
  (underline-position (post-table font-loader)))

(defmethod underline-thickness ((font-loader font-loader))
  (underline-thickness (post-table font-loader)))

(defmethod fixed-pitch-p ((font-loader font-loader))
  (plusp (is-fixed-pitch (post-table font-loader))))

(defmethod postscript-glyph-names ((font-loader font-loader))
  (let ((post-format-object (post-format-object (post-table font-loader))))
    (if (typep post-format-object 'post-format-2)
        (glyph-names post-format-object)
      post-format-object)))

(defun load-post-format-2 (names stream)
  (let* ((glyph-count (read-uint16 stream))
         (new-count glyph-count))
    (when (/= glyph-count (length names))
      (warn "Glyph count in \"post\" table (~D) ~
             does not match glyph count in \"maxp\" table (~D). ~
             This font may be broken."
            glyph-count (length names))
      (setf glyph-count (length names)
            new-count (length names)))
    ;; This is done in a couple passes. First, initialize the names
    ;; tables with indexes into either the standard table or the
    ;; pstring table. Next, read the pstring table into a vector.
    ;; Finally, replace the indexes with names.
    (dotimes (i glyph-count)
      (let ((name-index (read-uint16 stream)))
        (when (< name-index 258)
          (decf new-count))
        (setf (aref names i) name-index)))
    (let* ((pstrings (make-array new-count))
           (post-format-2 (make-instance 'post-format-2
                            :number-of-glyphs glyph-count
                            :glyph-name-index (copy-seq names)
                            :names pstrings
                            :glyph-names names)))
      (dotimes (i new-count)
        (setf (aref pstrings i) (read-pstring stream)))
      (loop for i below glyph-count
          for j across names
          do
            (cond ((< j 258)
                   (setf (aref names i)
                     (aref *standard-mac-glyph-names* j)))
                  (t
                   (setf (aref names i)
                     (aref pstrings (- j 258))))))
      post-format-2)))

(defun load-post-format-3 (names stream)
  (declare (ignore stream))
  (fill names nil))

(defun postscript-uni-name-p (name)
  (let ((end (or (position #\. name) (length name))))
    (and (= end 7)
         (= (mismatch "uni" name) 3)
         (loop for i from 3 below end
               always (digit-char-p (char name i) 16)))))

(defun postscript-name-code-point (name)
  "Returns, if available, the interpretation of the PostScript name NAME as a Unicode code point specifier.
Ref: http://partners.adobe.com/public/developer/opentype/index_glyph.html"
  (when (postscript-uni-name-p name)
    (parse-integer name :start 3 :end 7 :radix 16)))

(defmethod dump-post-info ((font-loader font-loader) output-stream)
  (let ((table-position (table-position "post" font-loader))
        (file-position (file-position output-stream)))
    (unless (= table-position file-position)
      (warn "Table `post' position is missing ~A (~A)." table-position file-position)
      (seek-to-table "post" font-loader)))
  (with-slots (post-table)
      font-loader
    (let ((format (post-format post-table)))
      (write-fixed format output-stream)
      (write-fixed (italic-angle post-table) output-stream)
      (write-fword (underline-position post-table) output-stream)
      (write-fword (underline-thickness post-table) output-stream)
      (write-uint32 (is-fixed-pitch post-table) output-stream)
      (write-uint32 (min-mem-type-42 post-table) output-stream)
      (write-uint32 (max-mem-type-42 post-table) output-stream)
      (write-uint32 (min-mem-type-1 post-table) output-stream)
      (write-uint32 (max-mem-type-1 post-table) output-stream)
      (let ((object (post-format-object post-table)))
        (case format
          (#x00020000 (dump-post-format-2 object output-stream))
          (#x00030000 (dump-post-format-3 object output-stream))))))
  (align-file-position output-stream))

(defun dump-post-format-2 (format-2 stream)
  (write-uint16 (number-of-glyphs format-2) stream)
  (loop for index across (glyph-name-index format-2)
      do (write-uint16 index stream))
  (loop for pstring across (names format-2)
      do (write-pstring pstring stream)))

(defun dump-post-format-3 (format-3 stream)
  (declare (ignore format-3 stream))
  (warn "Apple recommends against using 'post' table format 3 under most circumstances, as it can create problems with some printer drivers and PDF documents. The savings in disk space usually does not justify the potential loss in functionality."))
