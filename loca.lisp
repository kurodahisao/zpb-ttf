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
;;; Loading data from the "loca" table.
;;;
;;;  https://docs.microsoft.com/en-us/typography/opentype/spec/loca
;;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6loca.html
;;;
;;; $Id: loca.lisp,v 1.3 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf)

(defmethod load-loca-info ((font-loader font-loader))
  (seek-to-table "loca" font-loader)
  (with-slots (input-stream glyph-locations glyph-count loca-offset-format loca-table)
      font-loader
    (setf glyph-locations (make-array (1+ glyph-count)))
    (dotimes (i (1+ glyph-count))
      (setf (svref glyph-locations i)
            (if (eql loca-offset-format :short)
                (* (read-uint16 input-stream) 2)
                (read-uint32 input-stream))))
    (setf loca-table glyph-locations)))

(defmethod glyph-locations ((font-loader font-loader))
  (loca-table font-loader))

(defmethod (setf glyph-locations) (value (font-loader font-loader))
  (setf (loca-table font-loader) value))

(defmethod glyph-location (index (font-loader font-loader))
  (aref (glyph-locations font-loader) index))

(defmethod glyph-length (index (font-loader font-loader))
  (with-slots (glyph-locations)
      font-loader
    (- (aref glyph-locations (1+ index))
       (aref glyph-locations index))))

(defmethod dump-loca-info ((font-loader font-loader) output-stream)
  (let ((table-position (table-position "loca" font-loader))
        (file-position (file-position output-stream)))
    (unless (= table-position file-position)
      (warn "Table `loca' position is missing ~A (~A)." table-position file-position)
      (seek-to-table "loca" font-loader)))
  (let ((start-pos (file-position output-stream)))
    (with-slots (loca-table)
        font-loader
      (loop with loca-offset-format = (loca-offset-format font-loader)
          for location across loca-table
          do (if (eql loca-offset-format :short)
                 (write-uint16 (/ location 2) output-stream)
               (write-uint32 location output-stream))))
    (let ((end-pos (align-file-position output-stream)))
      (prog1
          end-pos
        (change-table-size "loca" (- end-pos start-pos) font-loader)))))
