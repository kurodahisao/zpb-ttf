;; $Id: zpb-ttf.asd,v 1.5 2006/03/24 20:47:27 xach Exp $

(defpackage #:zpb-ttf-system
  (:use #:cl #:asdf))

(in-package #:zpb-ttf-system)

(defsystem #:zpb-ttf
  :version "1.0.4"
  :author "Zach Beane <xach@xach.com>"
  :description "Access & Dump TrueType font metrics and outlines from Common Lisp"
  :license "BSD"
  :components ((:file "package")
               (:file "util"
                      :depends-on ("package"))
               (:file "conditions"
                      :depends-on ("package"))
               (:file "bounding-box"
                      :depends-on ("package"))
               (:file "font-loader"
                      :depends-on ("package"
                                   "util"
                                   "bounding-box"))
               (:file "maxp"
                      :depends-on ("package"
                                   "util"
                                   "font-loader"))
               (:file "head"
                      :depends-on ("package"
                                   "util"
                                   "conditions"
                                   "font-loader"))
               (:file "kern"
                      :depends-on ("package"
                                   "util"
                                   "conditions"
                                   "font-loader"))
               (:file "loca"
                      :depends-on ("package"
                                   "util"
                                   "font-loader"))
               (:file "name"
                      :depends-on ("package"
                                   "util"
                                   "conditions"
                                   "font-loader"))
               (:file "cmap"
                      :depends-on ("package"
                                   "util"
                                   "name"
                                   "font-loader"))
               (:file "post"
                      :depends-on ("package"
                                   "util"
                                   "conditions"
                                   "font-loader"))
               (:file "hhea"
                      :depends-on ("package"
                                   "util"
                                   "font-loader"))
               (:file "hmtx"
                      :depends-on ("package"
                                   "util"
                                   "font-loader"
                                   "hhea"))
               (:file "glyf"
                      :depends-on ("package"
                                   "util"
                                   "loca"
                                   "font-loader"))
               (:file "glyph"
                      :depends-on ("package"
                                   "util"
                                   "font-loader"
                                   "bounding-box"
                                   "glyf"
                                   "kern"
                                   "loca"))
               (:file "cvt"
                      :depends-on ("package"
                                   "util"
                                   "loca"
                                   "font-loader"))
               (:file "fpgm"
                      :depends-on ("package"
                                   "loca"
                                   "util"
                                   "font-loader"))
               (:file "gasp"
                      :depends-on ("package"
                                   "util"
                                   "loca"
                                   "font-loader"))
               (:file "gdef"
                      :depends-on ("package"
                                   "util"
                                   "loca"
                                   "font-loader"))
               (:file "gsub"
                      :depends-on ("package"
                                   "loca"
                                   "util"
                                   "font-loader"))
               (:file "os-2"
                      :depends-on ("package"
                                   "util"
                                   "loca"
                                   "font-loader"))
               (:file "prep"
                      :depends-on ("package"
                                   "util"
                                   "loca"
                                   "font-loader"))
               (:file "vhea"
                      :depends-on ("package"

                                   "loca""util"
                                   "font-loader"))
               (:file "vmtx"
                      :depends-on ("package"
                                   "util"
                                   "loca"
                                   "font-loader"))
               (:file "font-loader-interface"
                      :depends-on ("package"
                                   "util"
                                   "conditions"
                                   "font-loader"
                                   "maxp"
                                   "head"
                                   "kern"
                                   "loca"
                                   "name"
                                   "cmap"
                                   "post"
                                   "hhea"
                                   "hmtx"
                                   "cvt"
                                   "fpgm"
                                   "gasp"
                                   "gdef"
                                   "gsub"
                                   "os-2"
                                   "prep"
                                   "vhea"
                                   "vmtx"
                                   "glyf"))))


