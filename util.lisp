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
;;; Utility functions, mostly for reading data out of the input-stream
;;; of a font-loader.
;;;
;;; $Id: util.lisp,v 1.9 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf2)

;;; Reading compound MSB values from an '(unsigned-byte 8) stream

(defun read-uint32 (stream)
  (loop repeat 4
        for value = (read-byte stream)
        then (logior (ash value 8) (read-byte stream))
        finally (return value)))

(defun read-uint16 (stream)
  (loop repeat 2
        for value = (read-byte stream)
          then (logior (ash value 8) (read-byte stream))
        finally (return value)))


(defun read-uint8 (stream)
  (read-byte stream))

(defun read-int8 (stream)
  (let ((result (read-byte stream)))
    (if (logbitp 7 result)
        (1- (- (logandc2 #xFF result)))
        result)))

(defun read-int16 (stream)
  (let ((result (read-uint16 stream)))
    (if (logbitp 15 result)
        (1- (- (logandc2 #xFFFF result)))
        result)))

(defun read-fixed (stream)
  (read-uint32 stream))

(defun read-fword (stream)
  (read-int16 stream))

(defun read-fixed2.14 (stream)
  (let ((value (read-uint16 stream)))
    (let ((integer (ash value -14))
          (fraction (logand #x3FFF value)))
      (when (logbitp 1 integer)
        (setf integer (1- (- (logandc2 #b11 integer)))))
      (+ integer (float (/ fraction #x3FFF))))))

(defun read-pstring (stream)
  "Read a Pascal-style length-prefixed string."
  (let* ((length (read-uint8 stream))
         (buf (make-array length :element-type '(unsigned-byte 8)))
         (string (make-string length)))
    (read-sequence buf stream)
    ;; The following could be (map 'string #'code-char buf), but that
    ;; form benchmarked poorly
    (dotimes (i length string)
      (setf (schar string i) (code-char (aref buf i))))))

(defun bounded-aref (vector index)
  "Some TrueType data vectors are truncated, and any references beyond
the end of the vector should be treated as a reference to the last
element in the vector."
  (aref vector (min (1- (length vector)) index)))

(defun (setf bounded-aref) (new-value vector index)
  (setf (aref vector (min (1- (length vector)) index)) new-value))

(defun read-uint64 (stream)
  (let ((word0 (read-uint32 stream))
        (word1 (read-uint32 stream)))
    (dpb word0 (byte 64 32) word1)))

(defun read-int64 (stream)
  (let ((word (read-uint64 stream)))
    (if (logbitp 63 word)
        (1- (- (logandc2 #xFFFFFFFFFFFFFFFF word)))
      word)))

(defun read-int32 (stream)
  (let ((word (read-uint32 stream)))
    (if (logbitp 31 word)
        (1- (- (logandc2 #xFFFFFFFF word)))
      word)))

(defun read-uint24 (stream)
  (let ((byte0 (read-byte stream))
        (byte1 (read-byte stream))
        (byte2 (read-byte stream)))
    (dpb byte0 (byte 24 16) (dpb byte1 (byte 24 8) byte2))))

(defun write-uint24 (uint24 stream)
  (write-byte (ldb (byte 8 16) uint24) stream)
  (write-byte (ldb (byte 8  8) uint24) stream)
  (write-byte (ldb (byte 8  0) uint24) stream))

(defun read-ufword (stream)
  (read-uint16 stream))

(defun write-int16 (int16 stream)
  (write-uint16 int16 stream))

(defun write-uint16 (uint16 stream)
  (write-byte (ldb (byte 8 8) uint16) stream)
  (write-byte (ldb (byte 8 0) uint16) stream))

(defun write-int32 (int32 stream)
  (write-uint32 int32 stream))

(defun write-uint32 (uint32 stream)
  (write-byte (ldb (byte 8 24) uint32) stream)
  (write-byte (ldb (byte 8 16) uint32) stream)
  (write-byte (ldb (byte 8  8) uint32) stream)
  (write-byte (ldb (byte 8  0) uint32) stream))

(defun write-int64 (int64 stream)
  (write-uint64 int64 stream))

(defun write-uint64 (uint64 stream)
  (write-byte (ldb (byte 8 56) uint64) stream)
  (write-byte (ldb (byte 8 48) uint64) stream)
  (write-byte (ldb (byte 8 40) uint64) stream)
  (write-byte (ldb (byte 8 32) uint64) stream)
  (write-byte (ldb (byte 8 24) uint64) stream)
  (write-byte (ldb (byte 8 16) uint64) stream)
  (write-byte (ldb (byte 8  8) uint64) stream)
  (write-byte (ldb (byte 8  0) uint64) stream))

(defun write-ufword (ufword stream)
  (write-uint16 ufword stream))

(defun write-fword (fword stream)
  (write-int16 fword stream))

(defun write-fixed (fixed stream)
  (write-uint32 fixed stream))

(defun write-uint8 (uint8 stream)
  (write-byte uint8 stream))

(defun write-pstring (string stream)
  "Read a Pascal-style length-prefixed string."
  (write-uint8 (length string) stream)
  (loop for char across string
      do (write-byte (char-code char) stream)))

(defun align-position (position boundary)
  (multiple-value-bind (q r)
      (floor position boundary)
    (if (zerop r)
        position
      (* (1+ q) boundary))))

(defun align-file-position (stream &optional (boundary 4))
  (let* ((position (file-position stream))
         (new-position (align-position position boundary)))
    (if (= position new-position)
        position
      (if (output-stream-p stream)
          (loop repeat (- new-position position)
              do (write-byte 0 stream)
              finally (return (file-position stream)))
        (prog1 new-position
          (file-position stream new-position))))))

(defun change-table-size (name newsize font-loader &optional (boundary 4))
  (let* ((name-table (gethash (tag->number name) (tables font-loader)))
         (a-oldsize (align-position (size name-table) boundary))
         (a-newsize (align-position newsize boundary))
         (diff (- a-oldsize a-newsize))
         (name-offset (offset name-table)))
    (setf (size name-table) newsize)
    (loop for table-info being the hash-values of (tables font-loader)
        when (> (offset table-info) name-offset) do
          (setf (offset table-info) (- (offset table-info) diff)))))

;;; uint32 CalcTableChecksum(uint32 *table, uint32 numberOfBytesInTable)
;;;     {
;;;     uint32 sum = 0;
;;;     uint32 nLongs = (numberOfBytesInTable + 3) / 4;
;;;     while (nLongs-- > 0)
;;;         sum += *table++;
;;;     return sum;
;;;     }
(defun calc-table-checksum (offset size stream)
  (file-position stream offset)
  (let ((sum (loop repeat (floor (+ size 3) 4)
                 sum (read-uint32 stream))))
    (ldb (byte 32 0) sum)))

(defun advance-file-position (stream n &optional fill)
  "Move the file position of STREAM ahead by N bytes."
  (if (null fill)
      (let ((pos (file-position stream)))
        (file-position stream (+ pos n)))
    (loop repeat n
        do (write-byte 0 stream))))

(defun char-to-charname (font-loader character-name-list)
  (mapcar #'(lambda (c) (postscript-name (find-glyph c font-loader))) character-name-list))

(defparameter +octets+ #+allegro :octets #-allegro :latin-1)
