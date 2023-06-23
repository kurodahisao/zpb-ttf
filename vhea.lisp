;;; Copyright (c) 2016 KURODA Hisao, All Rights Reserved

(in-package "ZPB-TTF")

(defclass vhea-table ()
  ((version :initarg :version :accessor version) ; Version number of the Vertical Header Table (0x00011000 for the current version).
   (vert-typo-ascender :initarg :vert-typo-ascender :accessor vert-typo-ascender) ; The vertical typographic ascender for this font. It is the distance in FUnits from the vertical center baseline to the right of the design space. This will usually be set to half the horizontal advance of full-width glyphs. For example, if the full width is 1000 FUnits, this field will be set to 500.
   (vert-typo-descender :initarg :vert-typo-descender :accessor vert-typo-descender) ; The vertical typographic descender for this font. It is the distance in FUnits from the vertical center baseline to the left of the design space. This will usually be set to half the horizontal advance of full-width glyphs. For example, if the full width is 1000 FUnits, this field will be set to -500.
   (vert-typo-line-gap :initarg :vert-typo-line-gap :accessor vert-typo-line-gap) ; The vertical typographic line gap for this font.
   (advance-height-max :initarg :advance-height-max :accessor advance-height-max) ; The maximum advance height measurement in FUnits found in the font. This value must be consistent with the entries in the vertical metrics table.
   (min-top-side-bearing :initarg :min-top-side-bearing :accessor min-top-side-bearing) ; The minimum top side bearing measurement in FUnits found in the font, in FUnits. This value must be consistent with the entries in the vertical metrics table.
   (min-bottom-side-bearing :initarg :min-bottom-side-bearing :accessor min-bottom-side-bearing) ; The minimum bottom side bearing measurement in FUnits found in the font, in FUnits. This value must be consistent with the entries in the vertical metrics table.
   (y-max-extent :initarg :y-max-extent :accessor y-max-extent) ; This is defined as the value of the minTopSideBearing field added to the result of the value of the yMin field subtracted from the value of the yMax field.
   (caret-slope-rise :initarg :caret-slope-rise :accessor caret-slope-rise) ; The value of the caretSlopeRise field divided by the value of the caretSlopeRun field determines the slope of the caret. A value of 0 for the rise and a value of 1 for the run specifies a horizontal caret. A value of 1 for the rise and a value of 0 for the run specifies a vertical caret. A value between 0 for the rise and 1 for the run is desirable for fonts whose glyphs are oblique or italic. For a vertical font, a horizontal caret is best.
   (caret-slope-run :initarg :caret-slope-run :accessor caret-slope-run) ; See the caretSlopeRise field. Value = 0 for non-slanted fonts.
   (caret-offset :initarg :caret-offset :accessor caret-offset) ; The amount by which the highlight on a slanted glyph needs to be shifted away from the glyph in order to produce the best appearance. Set value equal to 0 for non-slanted fonts.
   (reserved1 :initarg :reserved1 :accessor reserved1) ; Set to 0.
   (reserved2 :initarg :reserved2 :accessor reserved2) ; Set to 0.
   (reserved3 :initarg :reserved3 :accessor reserved3) ; Set to 0.
   (reserved4 :initarg :reserved4 :accessor reserved4) ; Set to 0.
   (metric-data-format :initarg :metric-data-format :accessor metric-data-format) ; set to 0.
   (num-of-long-ver-metrics :initarg :num-of-long-ver-metrics :accessor num-of-long-ver-metrics))) ; Number of advance heights in the Vertical Metrics table.

(defmethod vertical-metrics-count ((font-loader font-loader))
  (with-slots (vhea-table)
      font-loader
    (num-of-long-ver-metrics vhea-table)))

(defmethod (setf vertical-metrics-count) (value (font-loader font-loader))
  (with-slots (vhea-table)
      font-loader
    (setf (num-of-long-ver-metrics vhea-table) value)))

(defmethod load-vhea-info ((font-loader font-loader))
  (seek-to-table "vhea" font-loader)
  (with-slots (input-stream vhea-table)
      font-loader
    (let ((version (read-fixed input-stream)))
      (setf vhea-table (make-instance 'vhea-table
                         :version (prog1 version
                                    (check-version "\"vhea\" table" version #x00010000))
                         :vert-typo-ascender (read-int16 input-stream)
                         :vert-typo-descender (read-int16 input-stream)
                         :vert-typo-line-gap (read-int16 input-stream)
                         :advance-height-max (read-int16 input-stream)
                         :min-top-side-bearing (read-int16 input-stream)
                         :min-bottom-side-bearing (read-int16 input-stream)
                         :y-max-extent (read-int16 input-stream)
                         :caret-slope-rise (read-int16 input-stream)
                         :caret-slope-run (read-int16 input-stream)
                         :caret-offset (read-int16 input-stream)
                         :reserved1 (read-int16 input-stream)
                         :reserved2 (read-int16 input-stream)
                         :reserved3 (read-int16 input-stream)
                         :reserved4 (read-int16 input-stream)
                         :metric-data-format (read-int16 input-stream)
                         :num-of-long-ver-metrics (read-uint16 input-stream))))))

(defmethod dump-vhea-info ((font-loader font-loader) output-stream)
  (let ((table-position (table-position "vhea" font-loader))
        (file-position (file-position output-stream)))
    (unless (= table-position file-position)
      (warn "Table `vhea' position is missing ~A (~A)." table-position file-position)
      (seek-to-table "vhea" font-loader)))
  (when (boundp '*dump-character-list*)
    (setf (vertical-metrics-count font-loader)
      (1+ (loop for index in (character-to-glyph-indexes *dump-character-list* font-loader)
              maximize index))))
  (with-slots (input-stream vhea-table)
      font-loader
    (write-fixed (version vhea-table) output-stream)
    (write-int16 (vert-typo-ascender vhea-table) output-stream)
    (write-int16 (vert-typo-descender vhea-table) output-stream)
    (write-int16 (vert-typo-line-gap vhea-table) output-stream)
    (write-int16 (advance-height-max vhea-table) output-stream)
    (write-int16 (min-top-side-bearing vhea-table) output-stream)
    (write-int16 (min-bottom-side-bearing vhea-table) output-stream)
    (write-int16 (y-max-extent vhea-table) output-stream)
    (write-int16 (caret-slope-rise vhea-table) output-stream)
    (write-int16 (caret-slope-run vhea-table) output-stream)
    (write-int16 (caret-offset vhea-table) output-stream)
    (write-int16 (reserved1 vhea-table) output-stream)
    (write-int16 (reserved2 vhea-table) output-stream)
    (write-int16 (reserved3 vhea-table) output-stream)
    (write-int16 (reserved4 vhea-table) output-stream)
    (write-int16 (metric-data-format vhea-table) output-stream)
    (write-uint16 (num-of-long-ver-metrics vhea-table) output-stream))
  (align-file-position output-stream))
