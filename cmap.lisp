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
;;; Loading data from the "cmap" table.
;;;
;;;  https://docs.microsoft.com/en-us/typography/opentype/spec/cmap
;;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6cmap.html
;;;
;;; $Id: cmap.lisp,v 1.15 2006/03/23 22:23:32 xach Exp $

(in-package #:zpb-ttf2)

(deftype cmap-value-table ()
  `(array (unsigned-byte 16) (*)))

;;; FIXME: "unicode-cmap" is actually a format 4 character map that
;;; happens to currently be loaded from a Unicode-compatible
;;; subtable. However, other character maps (like Microsoft's Symbol
;;; encoding) also use format 4 and could be loaded with these
;;; "unicode" objects and functions.

(defclass cmap-table ()
  ((version :initarg :version :reader version) ; Version number (Set to zero)
   (number-subtables :initarg :number-subtables :reader number-subtables) ; Number of encoding subtables
   (subtables :initarg :subtables :accessor subtables)
   (unicode-cmap :initarg :unicode-cmap :accessor unicode-cmap)
   (inverse-character-map :initarg :inverse-character-map :accessor inverse-character-map)))

(defclass cmap-subtable ()
  ((platform-id :initarg :platform-id :accessor platform-id) ; Platform identifier
   (platform-specific-id :initarg :platform-specific-id :accessor platform-specific-id) ; Platform-specific encoding identifier
   (offset :initarg :offset :accessor offset))) ; Offset of the mapping tabl

;;; UInt16	format	Format number is set to 4
;;; UInt16	length	Length of subtable in bytes
;;; UInt16	language	Language code (see above)
;;; UInt16	segCountX2	2 * segCount
;;; UInt16	searchRange	2 * (2**FLOOR(log2(segCount)))
;;; UInt16	entrySelector	log2(searchRange/2)
;;; UInt16	rangeShift	(2 * segCount) - searchRange
;;; UInt16	endCode[segCount]	Ending character code for each segment, last = 0xFFFF.
;;; UInt16	reservedPad	This value should be zero
;;; UInt16	startCode[segCount]	Starting character code for each segment
;;; UInt16	idDelta[segCount]	Delta for all character codes in segment
;;; UInt16	idRangeOffset[segCount]	Offset in bytes to glyph indexArray, or 0
;;; UInt16	glyphIndexArray[variable]	Glyph index array
(defclass unicode-cmap ()
  ((cmap-format :initarg :cmap-format :reader cmap-format)
   (cmap-length :initarg :cmap-length :reader cmap-length)
   (cmap-language :initarg :cmap-language :reader cmap-language)
   (segment-count :initarg :segment-count :reader segment-count)
   (search-range :initarg :search-range :reader search-range)
   (entry-selector :initarg :entry-selector :reader entry-selector)
   (range-shift :initarg :range-shift :reader range-shift)
   (end-codes :initarg :end-codes :reader end-codes)
   (reserved-pad :initarg :reserved-pad :reader reserved-pad)
   (start-codes :initarg :start-codes :reader start-codes)
   (id-deltas :initarg :id-deltas :reader id-deltas)
   (id-range-offsets :initarg :id-range-offsets :reader id-range-offsets)
   (glyph-indexes :initarg :glyph-indexes :accessor glyph-indexes)))

(defun load-unicode-cmap (stream)
  "Load a Unicode character map of type 4 from STREAM starting at the
current offset."
  (let ((format (read-uint16 stream)))
    (when (/= format 4)
      (error 'unsupported-format
             :location "\"cmap\" subtable"
             :actual-value format
             :expected-values (list 4)))
    (let ((table-start (- (file-position stream) 2))
          (subtable-length (read-uint16 stream))
          (language-code (read-uint16 stream))
          (segment-count (/ (read-uint16 stream) 2))
          (search-range (read-uint16 stream))
          (entry-selector (read-uint16 stream))
          (range-shift (read-uint16 stream)))
      (flet ((make-and-load-array (&optional (size segment-count))
               (loop with array = (make-array size
                                              :element-type '(unsigned-byte 16)
                                              :initial-element 0)
                   for i below size
                   do (setf (aref array i) (read-uint16 stream))
                   finally (return array)))
             (make-signed (i)
               (if (logbitp 15 i)
                   (1- (- (logandc2 #xFFFF i)))
                 i)))
        (let ((end-codes (make-and-load-array))
              (pad (read-uint16 stream))
              (start-codes (make-and-load-array))
              (id-deltas (make-and-load-array))
              (id-range-offsets (make-and-load-array))
              (glyph-index-array-size (/ (- subtable-length
                                            (- (file-position stream)
                                               table-start))
                                         2)))
          (make-instance 'unicode-cmap
                         :cmap-format format
                         :cmap-length subtable-length
                         :cmap-language language-code
                         :segment-count segment-count
                         :search-range search-range
                         :entry-selector entry-selector
                         :range-shift range-shift
                         :end-codes end-codes
                         :reserved-pad pad
                         :start-codes start-codes
                         ;; these are really signed, so sign them
                         :id-deltas (map 'vector #'make-signed id-deltas)
                         :id-range-offsets id-range-offsets
                         :glyph-indexes (make-and-load-array glyph-index-array-size)))))))


(defmethod invert-character-map (font-loader)
  "Return a vector mapping font indexes to code points."
  (with-slots (start-codes end-codes)
      (character-map font-loader)
    (declare (type cmap-value-table start-codes end-codes))
    (let ((points (make-array (glyph-count font-loader) :initial-element -1)))
      (dotimes (i (1- (length end-codes)) points)
        (loop for j from (aref start-codes i) to (aref end-codes i)
              for font-index = (code-point-font-index j font-loader)
              when (minusp (svref points font-index)) do
              (setf (svref points font-index) j))))))


(defgeneric code-point-font-index (code-point font-loader)
  (:documentation "Return the index of the Unicode CODE-POINT in
FONT-LOADER, if present, otherwise NIL.")
  (:method (code-point font-loader)
    (let ((cmap (character-map font-loader)))
      (with-slots (end-codes start-codes
                   id-deltas id-range-offsets
                   glyph-indexes)
          cmap
        (declare (type cmap-value-table
                       end-codes start-codes
                       id-range-offsets
                       glyph-indexes))
        (dotimes (i (segment-count cmap) 1)
          (when (<= code-point (aref end-codes i))
            (return
              (let ((start-code (aref start-codes i))
                    (id-range-offset (aref id-range-offsets i))
                    (id-delta (aref id-deltas i)))
                (cond ((< code-point start-code)
                       0)
                      ((zerop id-range-offset)
                       (logand #xFFFF (+ code-point id-delta)))
                      (t
                       (let* ((glyph-index-offset (- (+ i
                                                        (ash id-range-offset -1)
                                                        (- code-point start-code))
                                                     (segment-count cmap)))
                              (glyph-index (aref (glyph-indexes cmap)
                                                 glyph-index-offset)))
                         (logand #xFFFF
                                 (+ glyph-index id-delta)))))))))))))

(defgeneric font-index-code-point (glyph-index font-loader)
  (:documentation "Return the code-point for a given glyph index.")
  (:method (glyph-index font-loader)
    (let ((point (aref (inverse-character-map font-loader) glyph-index)))
      (if (plusp point)
          point
          0))))

(defmethod load-cmap-info ((font-loader font-loader))
  (unless (null (table-info "cmap" font-loader))
    (seek-to-table "cmap" font-loader)
    (with-slots (input-stream cmap-table)
        font-loader
      (let* ((start-pos (file-position input-stream))
             (version-number (read-uint16 input-stream))
             (subtable-count (read-uint16 input-stream)) ; only unicode cmap
             (subtables (make-array subtable-count))
             (foundp nil))
        (setf cmap-table (make-instance 'cmap-table
                           :version version-number
                           :number-subtables subtable-count
                           :subtables subtables))
        (loop for i from 0 below subtable-count
            for platform-id = (read-uint16 input-stream)
            for platform-specific-id = (read-uint16 input-stream)
            for offset = (read-uint32 input-stream)
            do (setf (aref subtables i) (make-instance 'cmap-subtable
                                          :platform-id platform-id
                                          :platform-specific-id platform-specific-id
                                          :offset offset)))
        (loop for i from 0 below subtable-count
            for subtable = (aref subtables i)
            for platform-id = (platform-id subtable)
            for platform-specific-id = (platform-specific-id subtable)
            for offset = (offset subtable)
            if (and (= platform-id
                       +microsoft-platform-id+)
                    (= platform-specific-id
                       +microsoft-unicode-bmp-encoding-id+))
            do (file-position input-stream (+ start-pos offset))
               (setf (unicode-cmap cmap-table) (load-unicode-cmap input-stream)
                     (inverse-character-map cmap-table) (invert-character-map font-loader)
                     foundp t)
            else if (and (= platform-id 0)
                         (= platform-specific-id 3))
            do
              (file-position input-stream (+ start-pos offset))
            else do
                 (let ((format1 (read-uint16 input-stream)))
                   (case format1
                     ;; Format 12.0 is required for Unicode fonts covering characters above U+FFFF on Windows. It is the most useful of the cmap formats with 32-bit support.
                     (12 (let ((format2 (read-uint16 input-stream))
                               (length (read-uint32 input-stream))
                               (language (read-uint32 input-stream))
                               (ngroups (read-uint32 input-stream)))
                           (declare (ignore format2 length language))
                           (loop repeat ngroups
                               do (read-uint32 input-stream) (read-uint32 input-stream) (read-uint32 input-stream))))
                     ;; Subtable format 14 specifies the Unicode Variation Sequences (UVSes) supported by the font. A Variation Sequence, according to the Unicode Standard, comprises a base character followed by a variation selector; e.g. <U+82A6, U+E0101>.
                     (14 (let ((length (read-uint32 input-stream))
                               (num-var-selector-records (read-uint32 input-stream))
                               (var-selector (read-uint24 input-stream))
                               (default-uvs-offset (read-uint32 input-stream))
                               (non-default-uvs-offset (read-uint32 input-stream)))
                           (warn "Format=~A,length=~A,records=~A,selector=~X,uvs-offset=~A,non-uvs-offset=~A~%" format1 length num-var-selector-records var-selector default-uvs-offset non-default-uvs-offset)))
                     (t (warn "Non Supported CMAP Format=~A." format1)))))
        (unless foundp
          (error "Could not find supported character map in font file"))))))

(defmethod character-map ((font-loader font-loader))
  (unicode-cmap (cmap-table font-loader)))

(defmethod inverse-character-map ((font-loader font-loader))
  (inverse-character-map (cmap-table font-loader)))




(defun available-character-maps (loader)
  (seek-to-table "cmap" loader)
  (let ((stream (input-stream loader)))
    (let ((start-pos (file-position stream))
          (version-number (read-uint16 stream))
          (subtable-count (read-uint16 stream)))
      (declare (ignore start-pos))
      (assert (zerop version-number))
      (dotimes (i subtable-count)
        (let ((platform-id (read-uint16 stream))
              (encoding-id (read-uint16 stream))
              (offset (read-uint32 stream)))
          (declare (ignore offset))
          (format t "~D (~A) - ~D (~A)~%"
                  platform-id (platform-id-name platform-id)
                  encoding-id (encoding-id-name platform-id encoding-id)))))))

(defmethod dump-cmap-info ((font-loader font-loader) output-stream)
  (with-slots (cmap-table)
      font-loader
    (let ((start-pos (file-position output-stream))
          (number-subtables 1)          ; Only UCS-2 table will be dumped
          (offset 12))                  ; version+numberSubtables+platformID+platformSpecificID+offset)
      (write-uint16 (version cmap-table) output-stream)
      (write-uint16 number-subtables output-stream) ; (number-subtables cmap-table)
      (loop for subtable across (subtables cmap-table)
          for platform-id = (platform-id subtable)
          for platform-specific-id = (platform-specific-id subtable)
          when (and (= platform-id
                       +microsoft-platform-id+)
                    (= platform-specific-id
                       +microsoft-unicode-bmp-encoding-id+))
          do (write-uint16 (platform-id subtable) output-stream)
             (write-uint16 (platform-specific-id subtable) output-stream)
             (write-uint32 offset output-stream) ; (offset subtable)
             ;; (file-position output-stream (+ start-pos offset))
             (let ((unicode-cmap (unicode-cmap cmap-table)))
               (dump-unicode-cmap unicode-cmap output-stream)))
      (let ((end-pos (align-file-position output-stream)))
        (prog1
            end-pos
          (change-table-size "cmap" (- end-pos start-pos) font-loader))))))

(defun dump-unicode-cmap (unicode-cmap output-stream)
  (write-uint16 (cmap-format unicode-cmap) output-stream)
  (write-uint16 (cmap-length unicode-cmap) output-stream)
  (write-uint16 (cmap-language unicode-cmap) output-stream)
  (write-uint16 (* (segment-count unicode-cmap) 2) output-stream)
  (write-uint16 (search-range unicode-cmap) output-stream)
  (write-uint16 (entry-selector unicode-cmap) output-stream)
  (write-uint16 (range-shift unicode-cmap) output-stream)
  (flet ((dump-array (array stream)
           (loop for elm across array
               do (write-uint16 elm stream))))
    (dump-array (end-codes unicode-cmap) output-stream)
    (write-uint16 (reserved-pad unicode-cmap) output-stream)
    (dump-array (start-codes unicode-cmap) output-stream)
    (dump-array (id-deltas unicode-cmap) output-stream)
    (dump-array (id-range-offsets unicode-cmap) output-stream)
    (dump-array (glyph-indexes unicode-cmap) output-stream)))
