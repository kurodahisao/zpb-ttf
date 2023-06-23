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
;;; Loading data from the 'glyf' table.
;;;
;;; $Id: glyf.lisp,v 1.13 2006/03/23 22:22:01 xach Exp $

(in-package #:zpb-ttf2)

(defclass control-point ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (on-curve-p :initarg :on-curve-p :reader on-curve-p)))

(defun make-control-point (x y on-curve-p)
  (make-instance 'control-point
                 :x x
                 :y y
                 :on-curve-p on-curve-p))

(defmethod print-object ((control-point control-point) stream)
  (print-unreadable-object (control-point stream :type t)
    (format stream "~D,~D~:[~;*~]"
            (x control-point) (y control-point) (on-curve-p control-point))))

(defmacro do-contour-segments* ((p1 p2) contour &body body)
  (let ((length (gensym))
        (i (gensym))
        (stack (gensym))
        (next (gensym))
        (next-point (gensym "NEXT-POINT"))
        (midpoint (gensym "MIDPOINT"))
        (contour* (gensym))
        (loop (gensym "LOOP"))
        (body-tag (gensym "BODY"))
        (mid p1)
        (end p2))
    `(let* ((,i 1)
            (,contour* ,contour)
            (,length (length ,contour*))
            ,stack ,next ,mid ,end)
       (unless (zerop ,length)
         (flet ((,next-point ()
                  (when (< ,i ,length)
                    (prog1 (aref ,contour* ,i) (incf ,i))))
                (,midpoint (p0 p1)
                  (make-control-point (/ (+ (x p0) (x p1)) 2)
                                      (/ (+ (y p0) (y p1)) 2)
                                      t)))
           (tagbody
              ,loop
              (setf ,mid nil
                    ,next (,next-point))
              (unless ,next
                (setf ,mid ,stack
                      ,end (aref ,contour* 0))
                (go ,body-tag))
              (if (on-curve-p ,next)
                  (setf ,end ,next
                        ,mid ,stack
                        ,stack nil)
                  (cond (,stack
                         (setf ,mid ,stack
                               ,end (,midpoint ,stack ,next)
                               ,stack ,next))
                        (t
                         (setf ,stack ,next)
                         (go ,loop))))
              ,body-tag
              ,@body
              (when ,next
                (go ,loop))))))))
                               
(defmacro do-contour-segments ((p0 p1 p2) contour &body body)
    "A contour is made up of segments. A segment may be a straight line
or a curve. For each segment, bind the P0 and P2 variables to the
start and end points of the segment. If the segment is a curve, set P1
to the control point of the curve, otherwise set P1 to NIL."
    ;; This macro started out life as a function and was converted.
    (let ((start p0)
          (contour* (gensym "CONTOUR")))
      `(let ((,contour* ,contour))
         (when (plusp (length ,contour*))
           (let ((,start (aref ,contour* 0)))
             (do-contour-segments* (,p1 ,p2)
                 ,contour*
               (progn ,@body)
               (setf ,start ,p2)))))))

(defun explicit-contour-points (contour)
  (let ((new-contour (make-array (length contour)
                                 :adjustable t
                                 :fill-pointer 0)))
    (when (plusp (length contour))
      (vector-push-extend (aref contour 0) new-contour))
    (do-contour-segments* (p1 p2)
        contour
      (when p1
        (vector-push-extend p1 new-contour))
      (vector-push-extend p2 new-contour))
    new-contour))


;;; Locating a glyph's contours and bounding box in the font loader's
;;; stream, and loading them

(defparameter *empty-contours*
  (make-array 0 :element-type '(signed-byte 16)))

(defparameter *empty-bounding-box*
  (make-array 4
              :initial-element 0
              :element-type '(signed-byte 16)))

(defun empty-bounding-box ()
  (copy-seq *empty-bounding-box*))

(defun empty-contours ()
  (copy-seq *empty-contours*))

(defun dump-compound-flags (flags)
  (format t "XXX flags=~16,'0B~%" flags)
  (let ((meanings '((0 . ARG_1_AND_2_ARE_WORDS)
                       (1 . ARGS_ARE_XY_VALUES)
                       (2 . ROUND_XY_TO_GRID)
                       (3 . WE_HAVE_A_SCALE)
                       (4 . OBSOLETE)
                       (5 . MORE_COMPONENTS)
                       (6 . WE_HAVE_AN_X_AND_Y_SCALE)
                       (7 . WE_HAVE_A_TWO_BY_TWO)
                       (8 . WE_HAVE_INSTRUCTIONS)
                       (9 . USE_MY_METRICS)
                       (10 . OVERLAP_COMPOUND))))
       (loop for ((bit . meaning)) on meanings
             do (when (logbitp bit flags)
                  (format t "...~A~%" meaning)))))

(defun transform-option-count (flags)
  (let ((scale-p 3)
        (xy-scale-p 6)
        (2*2-scale-p 7))
    (cond ((logbitp scale-p flags) 1)
          ((logbitp xy-scale-p flags) 2)
          ((logbitp 2*2-scale-p flags) 4)
          (t 0))))

(defun make-transformer (a b c d e f)
  "Given the elements of the transformation matrix specified by A, B,
C, D, E, and F, return a function of two arguments that returns the
arguments transformed as multiple values.
Ref: http://developer.apple.com/fonts/TTRefMan/RM06/Chap6glyf.html"
  (let ((m (max (abs a) (abs b)))
        (n (max (abs c) (abs d))))
    (when (<= (abs (- (abs a) (abs b))) 33/65536)
      (setf m (* m 2)))
    (when (<= (abs (- (abs c) (abs d))) 33/65536)
      (setf n (* n 2)))
    (lambda (x y)
      (values (* m (+ (* (/ a m) x)
                      (* (/ c m) y)
                      e))
              (* n (+ (* (/ b n) x)
                      (* (/ d n) y)
                      f))))))

(defun transform-contours (fn contours)
  "Call FN with the X and Y coordinates of each point of each contour
in the vector CONTOURS. FN should return two values, which are used to
update the X and Y values of each point."
  (loop for contour across contours do
        (loop for p across contour do
              (setf (values (x p) (y p))
                    (funcall fn (x p) (y p))))))

(defun merge-contours (contours-list)
  (let* ((total-contours (loop for contours in contours-list
                               summing (length contours)))
         (merged (make-array total-contours))
         (i 0))
    (dolist (contours contours-list merged)
      (loop for contour across contours do
            (setf (aref merged i) contour)
            (incf i)))))

(defun read-compound-contours (loader)
  (let ((contours-list '())
        (stream (input-stream loader)))
    (loop
     (let ((flags (read-uint16 stream))
           (font-index (read-uint16 stream)))
       (let ((position (file-position stream))
             (contours (read-contours-at-index font-index loader)))
         (push contours contours-list)
         (file-position stream position)
         (let ((args-words-p (logbitp 0 flags))
               (args-xy-values-p (logbitp 1 flags))
               (more-components-p (logbitp 5 flags))
               arg1 arg2)
           (cond ((and args-words-p args-xy-values-p)
                  (setf arg1 (read-int16 stream)
                        arg2 (read-int16 stream)))
                 (args-words-p
                  (setf arg1 (read-uint16 stream)
                        arg2 (read-uint16 stream))
                  (error "Compound glyphs relative to indexes not yet supported"))
                 (args-xy-values-p
                  (setf arg1 (read-int8 stream)
                        arg2 (read-int8 stream)))
                 (t
                  (setf arg1 (read-uint8 stream)
                        arg2 (read-uint8 stream))
                  (error "Compound glyphs relative to indexes not yet supported")))
           ;; Transform according to the transformation matrix
           (let ((a 1.0) (b 0.0) (c 0.0) (d 1.0)
                 (e arg1) (f arg2))
             (ecase (transform-option-count flags)
               (0)
               (1
                (setf a (setf d (read-fixed2.14 stream))))
               (2
                (setf a (read-fixed2.14 stream)
                      d (read-fixed2.14 stream)))
               (4
                (setf a (read-fixed2.14 stream)
                      b (read-fixed2.14 stream)
                      c (read-fixed2.14 stream)
                      d (read-fixed2.14 stream))))
             (let ((transform-fn (make-transformer a b c d e f)))
               (transform-contours transform-fn contours)))
           (unless more-components-p
             (return (merge-contours contours-list)))))))))
     
(defun read-points-vector (stream flags count axis)
  (let ((points (make-array count :fill-pointer 0))
        (short-index (if (eql axis :x) 1 2))
        (same-index (if (eql axis :x) 4 5)))
    (flet ((save-point (point)
             (vector-push point points)))
      (loop for flag across flags
            for short-p = (logbitp short-index flag)
            for same-p = (logbitp same-index flag)
            do (cond (short-p
                      (let ((new-point (read-uint8 stream)))
                        (save-point (if same-p new-point (- new-point)))))
                     (t
                      (if same-p
                          (save-point 0)
                          (save-point (read-int16 stream)))))))
    points))

(defun read-simple-contours (contour-count stream)
  "With the stream positioned immediately after the glyph bounding
box, read the contours data from STREAM and return it as a vector."
  (let ((contour-endpoint-indexes (make-array contour-count)))
    (loop for i below contour-count
          for endpoint-index = (read-uint16 stream)
          do (setf (svref contour-endpoint-indexes i) endpoint-index))
    ;; instructions
    (let ((n-points (1+ (svref contour-endpoint-indexes
                               (1- contour-count))))
          (instruction-length (read-uint16 stream)))
      (loop for i below instruction-length
            do (read-byte stream))
      ;; read the flags
      (let ((flags (make-array n-points)))
        (loop with i = 0
              while (< i n-points) do
              (let ((flag-byte (read-uint8 stream)))
                (setf (svref flags i) flag-byte)
                (incf i)
                (when (logbitp 3 flag-byte)
                  (let ((n-repeats (read-uint8 stream)))
                    (loop repeat n-repeats do
                          (setf (svref flags i) flag-byte)
                          (incf i))))))
        (let ((x-points (read-points-vector stream flags n-points :x ))
              (y-points (read-points-vector stream flags n-points :y))
              (control-points (make-array n-points :fill-pointer 0))
              (contours (make-array contour-count)))
          (loop for x-point across x-points
                for y-point across y-points
                for flag across flags
                for x = x-point then (+ x x-point)
                for y = y-point then (+ y y-point)
                do
                (vector-push-extend (make-control-point x y
                                                        (logbitp 0 flag))
                                    control-points))
          (loop for start = 0 then (1+ end)
                for end across contour-endpoint-indexes
                for i from 0
                do (setf (svref contours i)
                         (subseq control-points start (1+ end))))
          contours)))))

(defun read-contours-at-index (index loader)
  "Read the contours at glyph index INDEX, discarding bounding box
information."
  (let ((stream (input-stream loader)))
    (file-position stream (+ (table-position "glyf" loader)
                             (glyph-location index loader)))
    (let ((contour-count (read-int16 stream))
          (xmin (read-int16 stream))
          (ymin (read-int16 stream))
          (xmax (read-int16 stream))
          (ymax (read-int16 stream)))
      (declare (ignore xmin ymin xmax ymax))
      (if (= contour-count -1)
          (read-compound-contours loader)
          (read-simple-contours contour-count stream)))))

(defclass glyf-table ()
  ((number-of-contours :initarg :number-of-contours :accessor number-of-contours) ; If the number of contours is positive or zero, it is a single glyph;
                                        ; If the number of contours less than zero, the glyph is compound
   (xmin :initarg :xmin :accessor xmin) ; Minimum x for coordinate data
   (ymin :initarg :ymin :accessor ymin) ; Minimum y for coordinate data
   (xmax :initarg :xmax :accessor xmax) ; Maximum x for coordinate data
   (ymax :initarg :ymax :accessor ymax) ; Maximum y for coordinate data
   (glyph :initarg :glyph :accessor glyph))) ; (here follow the data for the simple or compound glyph)

(defclass simple-glyph ()
  ((end-pts-of-contours :initarg :end-pts-of-contours :accessor end-pts-of-contours) ; Array of last points of each contour ; n is the number of contours; array entries are point indices
   (instruction-length :initarg :instruction-length :accessor instruction-length) ; Total number of bytes needed for instructions
   (instructions :initarg :instructions :accessor instructions) ; Array of instructions for this glyph
   (flags :initarg :flags :accessor flags) ; Array of flags
   (xcoordinates :initarg :xcoordinates :accessor xcoordinates) ; Array of x-coordinates ; the first is relative to (0,0), others are relative to previous point
   (ycoordinates :initarg :ycoordinates :accessor ycoordinates))) ; Array of y-coordinates ; the first is relative to (0,0), others are relative to previous point

(defmethod load-glyf-info ((font-loader font-loader))
  (seek-to-table "glyf" font-loader)
  (with-slots (input-stream glyf-table)
      font-loader
    (loop with start-pos = (file-position input-stream)
        with glyphs = (make-array 0 :fill-pointer t :adjustable t)
        with last-location = (aref (glyph-locations font-loader)
                                   (glyph-count font-loader))
        for i from 0
        for location across (glyph-locations font-loader)
        when (< location last-location)
        do (file-position input-stream (+ start-pos location))
           (let ((number-of-contours (read-int16 input-stream))
                 (xmin (read-fword input-stream))
                 (ymin (read-fword input-stream))
                 (xmax (read-fword input-stream))
                 (ymax (read-fword input-stream)))
             (unless (>= number-of-contours 0)
               (error "Coumound Contours (contour-count=~A) not yet supported." number-of-contours))
             (when (= location          ; if location = nextlocation, it has no glyph.
                      (aref (glyph-locations font-loader) (1+ i)))
               ;; (format t "Glyph ~Ath and ~Ath have the same location ~A.~%" i (1+ i) location)
               (setq number-of-contours 0))
             (let* ((end-pts-of-contours (read-simple-vector input-stream number-of-contours '(unsigned-byte 16) #'read-uint16))
                    (instruction-length (if (= 0 number-of-contours)
                                            0
                                          (read-fword input-stream)))
                    (instructions (read-simple-vector input-stream instruction-length '(unsigned-byte 8) #'read-uint8))
                    (n-points (if (= 0 number-of-contours)
                                  0
                                (1+ (aref end-pts-of-contours (1- number-of-contours)))))
                    (flags (read-flag-vector input-stream n-points))
                    (xcoordinates (read-points-vector input-stream flags n-points :x))
                    (ycoordinates (read-points-vector input-stream flags n-points :y)))
               (when (and (= number-of-contours 0) (/= instruction-length 0))
                 (error "Contradicted number-of-contours & instruction-length ~A,~A" number-of-contours instruction-length))
               (vector-push-extend (make-instance 'glyf-table
                                     :number-of-contours number-of-contours
                                     :xmin xmin
                                     :ymin ymin
                                     :xmax xmax
                                     :ymax ymax
                                     :glyph (make-instance 'simple-glyph
                                              :end-pts-of-contours end-pts-of-contours
                                              :instruction-length instruction-length
                                              :instructions instructions
                                              :flags flags
                                              :xcoordinates xcoordinates
                                              :ycoordinates ycoordinates))
                                   glyphs)))
        finally (progn
                  (setf glyf-table glyphs)
                  (let ((end-pos (align-file-position input-stream))
                        (end-pos-expected (+ start-pos (table-size "glyf" font-loader))))
                    (when (/= end-pos end-pos-expected)
                      (error "Something wrong with Glyf Load ~A /= ~A expected." end-pos end-pos-expected)))))))

(defun read-simple-vector (stream n element-type read-function)
  (loop with vector = (make-array n :element-type element-type :fill-pointer 0)
      repeat n
      do (vector-push (funcall read-function stream) vector)
      finally (return vector)))

(defun read-flag-vector (stream n-points)
  (loop with vector = (make-array n-points :element-type '(unsigned-byte 8))
      for i from 0 below n-points
      for flag-byte = (setf (aref vector i) (read-uint8 stream))
      when (logbitp 3 flag-byte) do
        (loop repeat (read-uint8 stream)
            do (setf (aref vector (incf i)) flag-byte))
      finally (return vector)))


(defmethod dump-glyf-info ((font-loader font-loader) output-stream)
  (let ((table-position (table-position "glyf" font-loader))
        (file-position (file-position output-stream)))
    (unless (= table-position file-position)
      (warn "Table `glyf' position is missing ~A (~A)." table-position file-position)
      (seek-to-table "glyf" font-loader)))
  (with-slots (glyf-table)
      font-loader
    (let ((start-pos (file-position output-stream))
          (locations (glyph-locations font-loader)))
      (if (boundp '*dump-character-list*)
          (loop with indexes = (cons 0 (character-to-glyph-indexes *dump-character-list* font-loader))
              for i from 0
              for location = (align-position (file-position output-stream) 4)
              for glyph across glyf-table
              do (setf (aref locations i) (- location start-pos))
                 (when (member i indexes)
                   (dump-glyph-location glyph location output-stream))
              finally (setf (aref locations i) (- location start-pos)))
        (loop for location across locations
            for glyph across glyf-table
            do (dump-glyph-location glyph (+ start-pos location) output-stream)))
      (let ((end-pos (align-file-position output-stream))
            (end-pos-expected (+ start-pos (table-size "glyf" font-loader))))
        (when (/= end-pos end-pos-expected)
          (warn "Glyf Dump size differ ~A /= ~A expected." end-pos end-pos-expected))
        (prog1
            end-pos
          (change-table-size "glyf" (- end-pos start-pos) font-loader))))))

(defun dump-glyph-location (glyph location output-stream)
  (unless (= (number-of-contours glyph) 0)
    (file-position output-stream location)
    (write-int16 (number-of-contours glyph) output-stream)
    (write-fword (xmin glyph) output-stream)
    (write-fword (ymin glyph) output-stream)
    (write-fword (xmax glyph) output-stream)
    (write-fword (ymax glyph) output-stream)
    (let ((simple-glyph (glyph glyph)))
      (dump-simple-vector output-stream (end-pts-of-contours simple-glyph) #'write-uint16)
      (write-fword (instruction-length simple-glyph) output-stream)
      (dump-simple-vector output-stream (instructions simple-glyph) #'write-uint8)
      (dump-flag-vector output-stream (flags simple-glyph))
      (dump-points-vector output-stream (xcoordinates simple-glyph) (flags simple-glyph) :x)
      (dump-points-vector output-stream (ycoordinates simple-glyph) (flags simple-glyph) :y))))

#+ignore
(defun dump-null-glyph (glyph location output-stream)
  (declare (ignore glyph))
  (file-position output-stream location)
  (write-int16 0 output-stream)
  (write-fword 0 output-stream)
  (write-fword 0 output-stream)
  (write-fword 0 output-stream)
  (write-fword 0 output-stream))

(defun character-to-glyph-indexes (char-list font-loader)
  (sort (mapcar #'(lambda (char)
                    (font-index (find-glyph char font-loader)))
                char-list)
        #'<))

(defun dump-simple-vector (stream vector write-function)
  (loop for elm across vector
      do (funcall write-function elm stream)))

(defun dump-flag-vector (stream flags)
  (loop with max-index = (length flags)
      for i from 0 below max-index
      for flag-byte = (aref flags i)
      do (write-uint8 flag-byte stream)
         (when (logbitp 3 flag-byte)
           (loop for repeat from 0
               for j = (incf i)
               for next = (and (< j max-index) (aref flags j))
               while (eql flag-byte next)
               finally (progn
                         (decf i)
                         (write-uint8 repeat stream))))))

(defun dump-points-vector (stream points flags axis)
  (let ((short-index (if (eql axis :x) 1 2))
        (same-index (if (eql axis :x) 4 5)))
    (loop for flag across flags
        for point across points
        for short-p = (logbitp short-index flag)
        for same-p = (logbitp same-index flag)
        do (cond (short-p
                  (write-uint8 (if same-p point (- point)) stream))
                 (t
                  (if same-p
                      (assert (= point 0))
                    (write-int16 point stream)))))))
