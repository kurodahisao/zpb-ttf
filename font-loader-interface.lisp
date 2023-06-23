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
;;; Interface functions for creating, initializing, and closing a
;;; FONT-LOADER object.
;;;
;;; $Id: font-loader-interface.lisp,v 1.6 2006/03/23 22:20:35 xach Exp $

(in-package #:zpb-ttf)

(defun arrange-finalization (object stream)
  (flet ((quietly-close (&optional object)
           (declare (ignore object))
           (ignore-errors (close stream))))
    #+sbcl
    (sb-ext:finalize object #'quietly-close)
    #+cmucl
    (ext:finalize object #'quietly-close)
    #+clisp
    (ext:finalize object #'quietly-close)
    #+allegro
    (excl:schedule-finalization object #'quietly-close)))
    

#||
;;; cmap encoding subtable: platformID, platformSpecificID and offset
;;; 吐出すときは UCS-2 のテーブルだけで好い?
(0 3 3976) ; Default semantics & PRC
(3 1 60042) ; Unicode 2.0 or later semantics (BMP only) & UCS-2
(3 10 116108) ; Unicode 2.0 or later semantics (BMP only) & UCS-4

;;; Glyph Internal
(with-open-file (input "/usr/share/fonts/ipam.ttf" :external-format :latin-1 :element-type 'unsigned-byte)
  (loop with fontloader = (open-font-loader-from-stream input)
      for char in '(#\ぁ #\あ #\ぃ #\い #\壽)
      do (file-position input
                        (zpb-ttf::location (zpb-ttf:find-glyph char fontloader)))
         (print (list char 
                      (zpb-ttf::read-int16 input) ; number-of-contours
                      (zpb-ttf::read-int16 input) ; xmin
                      (zpb-ttf::read-int16 input) ; ymin
                      (zpb-ttf::read-int16 input) ; xmax
                      (zpb-ttf::read-int16 input) ; ymax
                      ))))
(#\HIRAGANA_LETTER_SMALL_A 3 369 -115 1717 1360) 
(#\HIRAGANA_LETTER_A 3 244 -74 1825 1661) 
(#\HIRAGANA_LETTER_SMALL_I 2 324 23 1786 1135) 
(#\HIRAGANA_LETTER_I 2 170 82 1862 1374) 
(#\U58FD 7 121 -148 1928 1698) 

;;;
(with-open-file (input "/usr/share/fonts/ipam.ttf" :external-format :latin-1 :element-type 'unsigned-byte)
  (let ((loader (open-font-loader-from-stream input)))
    (loop for char in '(#\1 #\a #\ぁ #\あ #\ぃ #\い #\壽)
        for glyph = (find-glyph char loader)
        for index = (font-index glyph)
        collect (list char (read-contours-at-index index loader)))))
((#\1
  #(#(#<CONTROL-POINT 283,111*> #<CONTROL-POINT 283,158*>
      #<CONTROL-POINT 413,164> #<CONTROL-POINT 443,209*>
      #<CONTROL-POINT 461,240> #<CONTROL-POINT 461,314*>
      #<CONTROL-POINT 461,1338*> #<CONTROL-POINT 461,1410>
      #<CONTROL-POINT 416,1428*> #<CONTROL-POINT 388,1439>
      #<CONTROL-POINT 283,1442*> #<CONTROL-POINT 283,1491*>
      #<CONTROL-POINT 449,1506> #<CONTROL-POINT 586,1586*>
      #<CONTROL-POINT 586,314*> #<CONTROL-POINT 586,211>
      #<CONTROL-POINT 633,183*> #<CONTROL-POINT 670,161>
      #<CONTROL-POINT 766,158*> #<CONTROL-POINT 766,111*>)))
 (#\a
  #(#(#<CONTROL-POINT 675,711*> #<CONTROL-POINT 675,741*>
      #<CONTROL-POINT 675,1051> #<CONTROL-POINT 485,1051*>
      #<CONTROL-POINT 336,1051> #<CONTROL-POINT 308,942*>
      #<CONTROL-POINT 353,915> #<CONTROL-POINT 353,862*>
      #<CONTROL-POINT 353,778> #<CONTROL-POINT 265,778*>
      #<CONTROL-POINT 173,778> #<CONTROL-POINT 173,881*>
      #<CONTROL-POINT 173,949> #<CONTROL-POINT 234,1014*>
      #<CONTROL-POINT 331,1110> #<CONTROL-POINT 484,1110*>
      #<CONTROL-POINT 788,1110> #<CONTROL-POINT 788,721*>
      #<CONTROL-POINT 788,273*> #<CONTROL-POINT 788,168>
      #<CONTROL-POINT 831,168*> #<CONTROL-POINT 889,168>
      #<CONTROL-POINT 929,277*> #<CONTROL-POINT 970,254*>
      #<CONTROL-POINT 912,74> #<CONTROL-POINT 806,74*> #<CONTROL-POINT 702,74>
      #<CONTROL-POINT 681,205*> #<CONTROL-POINT 553,74>
      #<CONTROL-POINT 396,74*> #<CONTROL-POINT 306,74>
      #<CONTROL-POINT 243,113*> #<CONTROL-POINT 132,177>
      #<CONTROL-POINT 132,320*> #<CONTROL-POINT 132,653>)
    #(#<CONTROL-POINT 675,653*> #<CONTROL-POINT 259,589>
      #<CONTROL-POINT 259,336*> #<CONTROL-POINT 259,156>
      #<CONTROL-POINT 419,156*> #<CONTROL-POINT 565,156>
      #<CONTROL-POINT 675,285*>)))
 (#\HIRAGANA_LETTER_SMALL_A
  #(#(#<CONTROL-POINT 1168,805*> #<CONTROL-POINT 1159,842>
      #<CONTROL-POINT 1112,883*> #<CONTROL-POINT 1145,911*>
      #<CONTROL-POINT 1249,853> #<CONTROL-POINT 1276,805*>
      #<CONTROL-POINT 1451,786> #<CONTROL-POINT 1576,690*>
      #<CONTROL-POINT 1717,580> #<CONTROL-POINT 1717,418*>
      #<CONTROL-POINT 1717,139> #<CONTROL-POINT 1434,-8*>
      #<CONTROL-POINT 1279,-90> #<CONTROL-POINT 1049,-115*>
      #<CONTROL-POINT 1031,-68*> #<CONTROL-POINT 1253,-28>
      #<CONTROL-POINT 1381,59*> #<CONTROL-POINT 1592,203>
      #<CONTROL-POINT 1592,414*> #<CONTROL-POINT 1592,560>
      #<CONTROL-POINT 1473,657*> #<CONTROL-POINT 1400,719>
      #<CONTROL-POINT 1287,741*> #<CONTROL-POINT 1281,691>
      #<CONTROL-POINT 1239,618*> #<CONTROL-POINT 1136,435>
      #<CONTROL-POINT 936,242*> #<CONTROL-POINT 946,196>
      #<CONTROL-POINT 949,180*> #<CONTROL-POINT 957,144>
      #<CONTROL-POINT 957,111*> #<CONTROL-POINT 957,57>
      #<CONTROL-POINT 920,10*> #<CONTROL-POINT 905,-8> #<CONTROL-POINT 889,-8*>
      #<CONTROL-POINT 845,-8> #<CONTROL-POINT 836,156*> #<CONTROL-POINT 644,0>
      #<CONTROL-POINT 506,0*> #<CONTROL-POINT 369,0> #<CONTROL-POINT 369,168*>
      #<CONTROL-POINT 369,385> #<CONTROL-POINT 605,584*>
      #<CONTROL-POINT 705,666> #<CONTROL-POINT 842,725*>
      #<CONTROL-POINT 856,934*> #<CONTROL-POINT 799,930>
      #<CONTROL-POINT 768,930*> #<CONTROL-POINT 598,930>
      #<CONTROL-POINT 514,1075*> #<CONTROL-POINT 549,1106*>
      #<CONTROL-POINT 623,1008> #<CONTROL-POINT 760,1008*>
      #<CONTROL-POINT 798,1008> #<CONTROL-POINT 863,1014*>
      #<CONTROL-POINT 871,1083*> #<CONTROL-POINT 873,1114*>
      #<CONTROL-POINT 879,1172> #<CONTROL-POINT 879,1196*>
      #<CONTROL-POINT 879,1278> #<CONTROL-POINT 801,1325*>
      #<CONTROL-POINT 828,1360*> #<CONTROL-POINT 1006,1332>
      #<CONTROL-POINT 1006,1237*> #<CONTROL-POINT 1006,1211>
      #<CONTROL-POINT 985,1143*> #<CONTROL-POINT 970,1089>
      #<CONTROL-POINT 961,1028*> #<CONTROL-POINT 1141,1065>
      #<CONTROL-POINT 1231,1128*> #<CONTROL-POINT 1258,1149>
      #<CONTROL-POINT 1285,1149*> #<CONTROL-POINT 1314,1149>
      #<CONTROL-POINT 1340,1135*> #<CONTROL-POINT 1377,1113>
      #<CONTROL-POINT 1377,1094*> #<CONTROL-POINT 1377,1009>
      #<CONTROL-POINT 949,944*> #<CONTROL-POINT 934,841>
      #<CONTROL-POINT 928,758*> #<CONTROL-POINT 1039,796>)
    #(#<CONTROL-POINT 1170,745*> #<CONTROL-POINT 1044,737>
      #<CONTROL-POINT 924,688*> #<CONTROL-POINT 920,585>
      #<CONTROL-POINT 920,512*> #<CONTROL-POINT 920,434>
      #<CONTROL-POINT 926,338*> #<CONTROL-POINT 1140,576>)
    #(#<CONTROL-POINT 832,248*> #<CONTROL-POINT 832,279*>
      #<CONTROL-POINT 832,315*> #<CONTROL-POINT 832,461>
      #<CONTROL-POINT 840,647*> #<CONTROL-POINT 751,594>
      #<CONTROL-POINT 676,532*> #<CONTROL-POINT 482,357>
      #<CONTROL-POINT 482,186*> #<CONTROL-POINT 482,92>
      #<CONTROL-POINT 547,92*> #<CONTROL-POINT 652,92>)))
 (#\HIRAGANA_LETTER_A
  #(#(#<CONTROL-POINT 1190,1008*> #<CONTROL-POINT 1178,1052>
      #<CONTROL-POINT 1121,1094*> #<CONTROL-POINT 1153,1130*>
      #<CONTROL-POINT 1280,1078> #<CONTROL-POINT 1319,1006*>
      #<CONTROL-POINT 1473,987> #<CONTROL-POINT 1581,926*>
      #<CONTROL-POINT 1825,790> #<CONTROL-POINT 1825,541*>
      #<CONTROL-POINT 1825,252> #<CONTROL-POINT 1567,86*>
      #<CONTROL-POINT 1376,-34> #<CONTROL-POINT 1061,-74*>
      #<CONTROL-POINT 1045,-27*> #<CONTROL-POINT 1692,108>
      #<CONTROL-POINT 1692,541*> #<CONTROL-POINT 1692,731>
      #<CONTROL-POINT 1547,846*> #<CONTROL-POINT 1458,916>
      #<CONTROL-POINT 1323,938*> #<CONTROL-POINT 1302,821>
      #<CONTROL-POINT 1180,639*> #<CONTROL-POINT 1060,464>
      #<CONTROL-POINT 910,332*> #<CONTROL-POINT 913,313>
      #<CONTROL-POINT 920,286*> #<CONTROL-POINT 929,254>
      #<CONTROL-POINT 930,250*> #<CONTROL-POINT 942,210>
      #<CONTROL-POINT 942,174*> #<CONTROL-POINT 942,106>
      #<CONTROL-POINT 899,57*> #<CONTROL-POINT 877,35> #<CONTROL-POINT 860,35*>
      #<CONTROL-POINT 809,35> #<CONTROL-POINT 797,240*> #<CONTROL-POINT 559,63>
      #<CONTROL-POINT 410,63*> #<CONTROL-POINT 244,63>
      #<CONTROL-POINT 244,268*> #<CONTROL-POINT 244,520>
      #<CONTROL-POINT 510,749*> #<CONTROL-POINT 630,846>
      #<CONTROL-POINT 797,913*> #<CONTROL-POINT 801,981>
      #<CONTROL-POINT 806,1054*> #<CONTROL-POINT 811,1121>
      #<CONTROL-POINT 811,1124*> #<CONTROL-POINT 815,1176*>
      #<CONTROL-POINT 748,1171> #<CONTROL-POINT 717,1171*>
      #<CONTROL-POINT 486,1171> #<CONTROL-POINT 404,1333*>
      #<CONTROL-POINT 443,1366*> #<CONTROL-POINT 520,1251>
      #<CONTROL-POINT 672,1251*> #<CONTROL-POINT 731,1251>
      #<CONTROL-POINT 824,1262*> #<CONTROL-POINT 828,1307*>
      #<CONTROL-POINT 830,1343*> #<CONTROL-POINT 838,1424>
      #<CONTROL-POINT 838,1462*> #<CONTROL-POINT 838,1572>
      #<CONTROL-POINT 738,1618*> #<CONTROL-POINT 766,1661*>
      #<CONTROL-POINT 981,1621> #<CONTROL-POINT 981,1522*>
      #<CONTROL-POINT 981,1498> #<CONTROL-POINT 961,1421*>
      #<CONTROL-POINT 947,1361> #<CONTROL-POINT 930,1280*>
      #<CONTROL-POINT 1087,1307> #<CONTROL-POINT 1182,1356*>
      #<CONTROL-POINT 1184,1358> #<CONTROL-POINT 1186,1358*>
      #<CONTROL-POINT 1215,1372> #<CONTROL-POINT 1243,1393*>
      #<CONTROL-POINT 1282,1419> #<CONTROL-POINT 1321,1419*>
      #<CONTROL-POINT 1367,1419> #<CONTROL-POINT 1405,1399*>
      #<CONTROL-POINT 1434,1383> #<CONTROL-POINT 1434,1366*>
      #<CONTROL-POINT 1434,1343> #<CONTROL-POINT 1393,1321*>
      #<CONTROL-POINT 1216,1232> #<CONTROL-POINT 916,1186*>
      #<CONTROL-POINT 899,1067> #<CONTROL-POINT 893,948*>
      #<CONTROL-POINT 1052,996>)
    #(#<CONTROL-POINT 1194,942*> #<CONTROL-POINT 1043,933>
      #<CONTROL-POINT 889,874*> #<CONTROL-POINT 887,836>
      #<CONTROL-POINT 887,680*> #<CONTROL-POINT 887,524>
      #<CONTROL-POINT 895,432*> #<CONTROL-POINT 1033,576>
      #<CONTROL-POINT 1137,786*> #<CONTROL-POINT 1186,888>)
    #(#<CONTROL-POINT 793,334*> #<CONTROL-POINT 789,502>
      #<CONTROL-POINT 789,561*> #<CONTROL-POINT 789,578>
      #<CONTROL-POINT 790,663*> #<CONTROL-POINT 792,761>
      #<CONTROL-POINT 793,831*> #<CONTROL-POINT 712,790>
      #<CONTROL-POINT 621,719*> #<CONTROL-POINT 371,506>
      #<CONTROL-POINT 371,272*> #<CONTROL-POINT 371,163>
      #<CONTROL-POINT 459,163*> #<CONTROL-POINT 583,163>)))
 (#\HIRAGANA_LETTER_SMALL_I
  #(#(#<CONTROL-POINT 908,602*> #<CONTROL-POINT 883,543*>
      #<CONTROL-POINT 762,258> #<CONTROL-POINT 762,170*>
      #<CONTROL-POINT 762,161> #<CONTROL-POINT 766,123*>
      #<CONTROL-POINT 770,74> #<CONTROL-POINT 770,70*> #<CONTROL-POINT 770,23>
      #<CONTROL-POINT 735,23*> #<CONTROL-POINT 709,23> #<CONTROL-POINT 643,70*>
      #<CONTROL-POINT 496,176> #<CONTROL-POINT 445,367*>
      #<CONTROL-POINT 404,521> #<CONTROL-POINT 404,832*>
      #<CONTROL-POINT 404,975> #<CONTROL-POINT 390,1018*>
      #<CONTROL-POINT 373,1068> #<CONTROL-POINT 324,1094*>
      #<CONTROL-POINT 353,1135*> #<CONTROL-POINT 545,1089>
      #<CONTROL-POINT 545,954*> #<CONTROL-POINT 545,926>
      #<CONTROL-POINT 529,844*> #<CONTROL-POINT 502,731>
      #<CONTROL-POINT 502,621*> #<CONTROL-POINT 502,396>
      #<CONTROL-POINT 586,281*> #<CONTROL-POINT 608,252>
      #<CONTROL-POINT 623,252*> #<CONTROL-POINT 664,252>
      #<CONTROL-POINT 869,629*>)
    #(#<CONTROL-POINT 1315,1079*> #<CONTROL-POINT 1522,966>
      #<CONTROL-POINT 1673,784*> #<CONTROL-POINT 1786,650>
      #<CONTROL-POINT 1786,520*> #<CONTROL-POINT 1786,456>
      #<CONTROL-POINT 1743,419*> #<CONTROL-POINT 1720,399>
      #<CONTROL-POINT 1698,399*> #<CONTROL-POINT 1648,399>
      #<CONTROL-POINT 1633,489*> #<CONTROL-POINT 1562,853>
      #<CONTROL-POINT 1287,1040*>)))
 (#\HIRAGANA_LETTER_I
  #(#(#<CONTROL-POINT 844,743*> #<CONTROL-POINT 682,395>
      #<CONTROL-POINT 682,244*> #<CONTROL-POINT 682,204>
      #<CONTROL-POINT 690,166*> #<CONTROL-POINT 692,156>
      #<CONTROL-POINT 692,135*> #<CONTROL-POINT 692,82>
      #<CONTROL-POINT 647,82*> #<CONTROL-POINT 585,82>
      #<CONTROL-POINT 483,180*> #<CONTROL-POINT 361,303>
      #<CONTROL-POINT 315,500*> #<CONTROL-POINT 271,676>
      #<CONTROL-POINT 268,1049*> #<CONTROL-POINT 268,1175>
      #<CONTROL-POINT 252,1237*> #<CONTROL-POINT 233,1298>
      #<CONTROL-POINT 170,1331*> #<CONTROL-POINT 197,1374*>
      #<CONTROL-POINT 420,1347> #<CONTROL-POINT 420,1190*>
      #<CONTROL-POINT 420,1161> #<CONTROL-POINT 410,1110*>
      #<CONTROL-POINT 377,961> #<CONTROL-POINT 377,807*>
      #<CONTROL-POINT 377,490> #<CONTROL-POINT 496,340*>
      #<CONTROL-POINT 512,324> #<CONTROL-POINT 520,324*>
      #<CONTROL-POINT 548,324> #<CONTROL-POINT 766,711*>
      #<CONTROL-POINT 801,772*>)
    #(#<CONTROL-POINT 1313,1317*> #<CONTROL-POINT 1593,1160>
      #<CONTROL-POINT 1745,944*> #<CONTROL-POINT 1862,778>
      #<CONTROL-POINT 1862,643*> #<CONTROL-POINT 1862,522>
      #<CONTROL-POINT 1772,522*> #<CONTROL-POINT 1721,522>
      #<CONTROL-POINT 1702,618*> #<CONTROL-POINT 1616,1053>
      #<CONTROL-POINT 1280,1278*>)))
 (#\U58FD
  #(#(#<CONTROL-POINT 447,393*> #<CONTROL-POINT 728,393*>
      #<CONTROL-POINT 793,471*> #<CONTROL-POINT 910,382>
      #<CONTROL-POINT 910,354*> #<CONTROL-POINT 910,345>
      #<CONTROL-POINT 898,336*> #<CONTROL-POINT 855,305*>
      #<CONTROL-POINT 855,-27*> #<CONTROL-POINT 742,-27*>
      #<CONTROL-POINT 742,57*> #<CONTROL-POINT 422,57*>
      #<CONTROL-POINT 422,-55*> #<CONTROL-POINT 307,-55*>
      #<CONTROL-POINT 307,450*> #<CONTROL-POINT 378,426>)
    #(#<CONTROL-POINT 422,342*> #<CONTROL-POINT 422,108*>
      #<CONTROL-POINT 742,108*> #<CONTROL-POINT 742,342*>)
    #(#<CONTROL-POINT 1051,934*> #<CONTROL-POINT 1051,788*>
      #<CONTROL-POINT 1506,788*> #<CONTROL-POINT 1598,881*>
      #<CONTROL-POINT 1668,835> #<CONTROL-POINT 1733,776*>
      #<CONTROL-POINT 1706,737*> #<CONTROL-POINT 305,737*>
      #<CONTROL-POINT 277,788*> #<CONTROL-POINT 934,788*>
      #<CONTROL-POINT 934,934*> #<CONTROL-POINT 440,934*>
      #<CONTROL-POINT 416,985*> #<CONTROL-POINT 1325,985*>
      #<CONTROL-POINT 1414,1069*> #<CONTROL-POINT 1471,1034>
      #<CONTROL-POINT 1545,971*> #<CONTROL-POINT 1518,934*>)
    #(#<CONTROL-POINT 934,1325*> #<CONTROL-POINT 934,1479*>
      #<CONTROL-POINT 262,1479*> #<CONTROL-POINT 234,1530*>
      #<CONTROL-POINT 934,1530*> #<CONTROL-POINT 934,1698*>
      #<CONTROL-POINT 1133,1692> #<CONTROL-POINT 1133,1651*>
      #<CONTROL-POINT 1133,1626> #<CONTROL-POINT 1051,1596*>
      #<CONTROL-POINT 1051,1530*> #<CONTROL-POINT 1547,1530*>
      #<CONTROL-POINT 1651,1643*> #<CONTROL-POINT 1678,1620*>
      #<CONTROL-POINT 1717,1589> #<CONTROL-POINT 1794,1520*>
      #<CONTROL-POINT 1774,1479*> #<CONTROL-POINT 1051,1479*>
      #<CONTROL-POINT 1051,1325*> #<CONTROL-POINT 1446,1325*>
      #<CONTROL-POINT 1545,1419*> #<CONTROL-POINT 1611,1382>
      #<CONTROL-POINT 1678,1315*> #<CONTROL-POINT 1653,1274*>
      #<CONTROL-POINT 332,1274*> #<CONTROL-POINT 303,1325*>)
    #(#<CONTROL-POINT 1563,340*> #<CONTROL-POINT 1563,8*>
      #<CONTROL-POINT 1563,-148> #<CONTROL-POINT 1383,-145*>
      #<CONTROL-POINT 1383,-90> #<CONTROL-POINT 1329,-68*>
      #<CONTROL-POINT 1286,-52> #<CONTROL-POINT 1188,-37*>
      #<CONTROL-POINT 1188,18*> #<CONTROL-POINT 1292,4>
      #<CONTROL-POINT 1381,4*> #<CONTROL-POINT 1446,4>
      #<CONTROL-POINT 1446,70*> #<CONTROL-POINT 1446,340*>
      #<CONTROL-POINT 948,340*> #<CONTROL-POINT 922,391*>
      #<CONTROL-POINT 1446,391*> #<CONTROL-POINT 1446,555*>
      #<CONTROL-POINT 146,555*> #<CONTROL-POINT 121,606*>
      #<CONTROL-POINT 1688,606*> #<CONTROL-POINT 1792,714*>
      #<CONTROL-POINT 1876,651> #<CONTROL-POINT 1928,596*>
      #<CONTROL-POINT 1901,555*> #<CONTROL-POINT 1563,555*>
      #<CONTROL-POINT 1563,391*> #<CONTROL-POINT 1665,391*>
      #<CONTROL-POINT 1749,500*> #<CONTROL-POINT 1801,454>
      #<CONTROL-POINT 1866,379*> #<CONTROL-POINT 1840,340*>)
    #(#<CONTROL-POINT 1039,330*> #<CONTROL-POINT 1258,246>
      #<CONTROL-POINT 1258,133*> #<CONTROL-POINT 1258,63>
      #<CONTROL-POINT 1196,63*> #<CONTROL-POINT 1146,63>
      #<CONTROL-POINT 1131,127*> #<CONTROL-POINT 1110,212>
      #<CONTROL-POINT 1014,295*>)
    #(#<CONTROL-POINT 1684,1157*> #<CONTROL-POINT 1768,1251*>
      #<CONTROL-POINT 1905,1129> #<CONTROL-POINT 1905,1094*>
      #<CONTROL-POINT 1905,1074> #<CONTROL-POINT 1876,1071*>
      #<CONTROL-POINT 1804,1061*> #<CONTROL-POINT 1737,993>
      #<CONTROL-POINT 1663,936*> #<CONTROL-POINT 1622,963*>
      #<CONTROL-POINT 1668,1045> #<CONTROL-POINT 1688,1106*>
      #<CONTROL-POINT 152,1106*> #<CONTROL-POINT 125,1157*>))))

;;; Copy TTF Font File
(with-open-file (input "/usr/share/fonts/ipam.ttf" :external-format :latin-1 :element-type 'unsigned-byte)
  (with-open-file (output "foo" :direction :output :if-exists :supersede :external-format :latin-2 :element-type 'unsigned-byte)
    (let ((intro (make-sequence '(vector (unsigned-byte 8)) 300)))
      (read-sequence intro input)
      (write-sequence intro output))
    (loop for (name offset size) in '(("GDEF" 300 30)
                                      ("GSUB" 332 3518)
                                      ("OS/2" 3852 96)
                                      ("cmap" 3948 236166) ; t 2
                                      ("cvt " 240116 204)
                                      ("fpgm" 240320 113)
                                      ("gasp" 240436 16)
                                      ("glyf" 240452 7530768) ;t 1
                                      ("head" 7771220 54) ;t
                                      ("hhea" 7771276 36) ;t
                                      ("hmtx" 7771312 50600) ;t
                                      ("loca" 7821912 50916) ;t 4
                                      ("maxp" 7872828 32) ;t
                                      ("name" 7872860 2438) ;t
                                      ("post" 7875300 120451) ;t 3
                                      ("prep" 7995752 10) 
                                      ("vhea" 7995764 36) 
                                      ("vmtx" 7995800 50910))
        do (let ((seq (make-sequence '(vector (unsigned-byte 8)) size)))
             (file-position input offset)
             (file-position output offset)
             (read-sequence seq input)
             (write-sequence seq output)))
    (loop for byte = (read-byte input nil nil)
        while byte do
          (write-byte byte output))))
;;; $ cmp ~/foo /usr/share/fonts/ipam.ttf

;;; IPAMincho ttf table: (名前 offset size)
("GDEF" 300 30) 
("GSUB" 332 3518) 
("OS/2" 3852 96) 
("cmap" 3948 236166)                    ;t 2
("cvt " 240116 204) 
("fpgm" 240320 113) 
("gasp" 240436 16) 
("glyf" 240452 7530768)                 ;t 1
("head" 7771220 54)                     ;t
("hhea" 7771276 36)                     ;t
("hmtx" 7771312 50600)                  ;t
("loca" 7821912 50916)                  ;t 4
("maxp" 7872828 32)                     ;t
("name" 7872860 2438)                   ;t
("post" 7875300 120451)                 ;t 3
("prep" 7995752 10) 
("vhea" 7995764 36) 
("vmtx" 7995800 50910) 
||#


(defun open-font-loader-from-stream (input-stream &key (collection-index 0))
  (let ((magic (read-uint32 input-stream))
        (font-count))
    (when (/= magic #x00010000 #x74727565 #x74746366)
      (error 'bad-magic
             :location "font header"
             :expected-values (list #x00010000 #x74727565 #x74746366)
             :actual-value magic))
    (when (= magic #x74746366)
      (let ((version (read-uint32 input-stream)))
        (check-version "ttc header" version #x00010000 #x00020000)
        (setf font-count (read-uint32 input-stream))
        (let* ((offset-table (make-array font-count))
               (dsig))
          (when (> collection-index font-count)
            (error 'unsupported-value
                   :description "Font index out of range"
                   :actual-value collection-index
                   :expected-values (list font-count)))
          (loop for i below font-count
                do (setf (aref offset-table i) (read-uint32 input-stream)))
          (when (= version #x00020000)
            (let ((flag (read-uint32 input-stream))
                  (length (read-uint32 input-stream))
                  (offset (read-uint32 input-stream)))
              (list flag length offset)
              (when (= #x44534947 flag)
                (setf dsig (list length offset)))))
          ;; seek to font offset table
          (file-position input-stream (aref offset-table collection-index))
          (let ((magic2 (read-uint32 input-stream)))
            (when (/= magic2 #x00010000 #x74727565)
              (error 'bad-magic
                     :location "font header"
                     :expected-values (list #x00010000 #x74727565)
                     :actual-value magic2))))))

    (let* ((table-count (read-uint16 input-stream))
           (search-range (read-uint16 input-stream))
           (entry-selector (read-uint16 input-stream))
           (range-shift (read-uint16 input-stream))
           (font-loader (make-instance 'font-loader
                                       :input-stream input-stream
                                       :scaler-type magic
                                       :search-range search-range
                                       :entry-selector entry-selector
                                       :range-shift range-shift
                                       :table-count table-count
                                       :collection-font-cont font-count
                                       :collection-font-index
                                       (when font-count
                                         collection-index))))
      ;;uint32 CalcTableChecksum(uint32 *table, uint32 numberOfBytesInTable)
      ;;    {
      ;;    uint32 sum = 0;
      ;;    uint32 nLongs = (numberOfBytesInTable + 3) / 4;
      ;;    while (nLongs-- > 0)
      ;;        sum += *table++;
      ;;    return sum;
      ;;    }
      (loop repeat table-count
            for tag = (read-uint32 input-stream)
            for checksum = (read-uint32 input-stream)
            for offset = (read-uint32 input-stream)
            for size = (read-uint32 input-stream)
            do (setf (gethash tag (tables font-loader))
                     (make-instance 'table-info
                                    :checksum checksum
                                    :offset offset
                                    :name (number->tag tag)
                                    :size size)))
      (load-maxp-info font-loader)
      (load-gdef-info font-loader)      ; new
      (load-gsub-info font-loader)      ; new
      (load-os/2-info font-loader)      ; new
      (load-cmap-info font-loader)
      (load-cvt--info font-loader)      ; new
      (load-fpgm-info font-loader)      ; new
      (load-gasp-info font-loader)      ; new
      (load-head-info font-loader)
      (load-hhea-info font-loader)
      (load-hmtx-info font-loader)
      (load-kern-info font-loader)      ; No Kern in IPAMincho
      (load-loca-info font-loader)
      (load-name-info font-loader)
      (load-post-info font-loader)
      (load-prep-info font-loader)      ; new
      (load-vhea-info font-loader)      ; new
      (load-vmtx-info font-loader)      ; new
      (load-glyf-info font-loader)
      (setf (glyph-cache font-loader)
            (make-array (glyph-count font-loader) :initial-element nil))
      (loop for table-info being the hash-values of (tables font-loader)
          for offset = (offset table-info)
          for name = (name table-info)
          for size = (size table-info)
          do (print (list name offset size)))
      font-loader)))

(defun open-font-loader-from-file (thing &key (collection-index 0))
  (let ((stream (open thing
                      :direction :input
                      :element-type '(unsigned-byte 8))))
    (let ((font-loader (open-font-loader-from-stream
                        stream :collection-index collection-index)))
      (arrange-finalization font-loader stream)
      font-loader)))

(defun open-font-loader (thing &key (collection-index 0))
  (typecase thing
    (font-loader
     (cond
       ((= collection-index (collection-font-index thing))
        (unless (open-stream-p (input-stream thing))
          (setf (input-stream thing) (open (input-stream thing))))
        thing)
       (t
        (open-font-loader-from-file (input-stream thing)
                                    :collection-index collection-index))))
    (stream
     (if (open-stream-p thing)
         (open-font-loader-from-stream thing :collection-index collection-index)
         (error "~A is not an open stream" thing)))
    (t
     (open-font-loader-from-file thing :collection-index collection-index))))

(defun close-font-loader (loader)
  (close (input-stream loader)))

(defmacro with-font-loader ((loader file &key (collection-index 0)) &body body)
  `(let (,loader)
    (unwind-protect
         (progn
           (setf ,loader (open-font-loader ,file
                                           :collection-index ,collection-index))
           ,@body)
      (when ,loader
        (close-font-loader ,loader)))))

#+ignore
(defparameter +ipa-mincho-table-name-offset-size+
    '(("GDEF" 300 30) 
      ("GSUB" 332 3518) 
      ("OS/2" 3852 96) 
      ("cmap" 3948 236166)              ;t 2
      ("cvt " 240116 204) 
      ("fpgm" 240320 113) 
      ("gasp" 240436 16) 
      ("glyf" 240452 7530768)           ;t 1
      ("head" 7771220 54)               ;t
      ("hhea" 7771276 36)               ;t
      ("hmtx" 7771312 50600)            ;t
      ("loca" 7821912 50916)            ;t 4
      ("maxp" 7872828 32)               ;t
      ("name" 7872860 2438)             ;t
      ("post" 7875300 120451)           ;t 3
      ("prep" 7995752 10) 
      ("vhea" 7995764 36) 
      ("vmtx" 7995800 50910)))

#+obsolete
(defun dump-font-loader-to-stream (font-loader output-stream)
  (let* ((magic (scaler-type font-loader))
         (table-list '("cvt " "fpgm" "glyf" "head" "hhea" "hmtx" "loca" "maxp" "post" "prep" "vhea" "vmtx"))
         (table-count (length table-list)) ; (table-count font-loader)
         (search-range (search-range font-loader))
         (entry-selector (entry-selector font-loader))
         (range-shift (range-shift font-loader)))
    (write-uint32 magic output-stream)
    (write-uint16 table-count output-stream)
    (write-uint16 search-range output-stream)
    (write-uint16 entry-selector output-stream)
    (write-uint16 range-shift output-stream)
    ;;uint32 CalcTableChecksum(uint32 *table, uint32 numberOfBytesInTable)
    ;;    {
    ;;    uint32 sum = 0;
    ;;    uint32 nLongs = (numberOfBytesInTable + 3) / 4;
    ;;    while (nLongs-- > 0)
    ;;        sum += *table++;
    ;;    return sum;
    ;;    }
    (let ((start-table (file-position output-stream)))
      (advance-file-position output-stream
                             (loop repeat (hash-table-count (tables font-loader))
                                 sum 16))
      ;; (dump-gdef-info font-loader output-stream)
      (change-table-size "GDEF" 0 font-loader)
      ;; (dump-gsub-info font-loader output-stream)
      (change-table-size "GSUB" 0 font-loader)
      ;; (dump-os/2-info font-loader output-stream)
      (change-table-size "OS/2" 0 font-loader)
      ;; (dump-cmap-info font-loader output-stream)
      (change-table-size "cmap" 0 font-loader)
      (dump-cvt--info font-loader output-stream)
      (dump-fpgm-info font-loader output-stream)
      ;; (dump-gasp-info font-loader output-stream)
      (change-table-size "gasp" 0 font-loader)
      (dump-glyf-info font-loader output-stream)
      (dump-head-info font-loader output-stream)
      (dump-hhea-info font-loader output-stream)
      (dump-hmtx-info font-loader output-stream)
      ;; (dump-kern-info font-loader output-stream)      ; No Kern in IPAMincho
      (dump-loca-info font-loader output-stream)
      (dump-maxp-info font-loader output-stream)
      ;; (dump-name-info font-loader output-stream)
      (change-table-size "name" 0 font-loader)
      (dump-post-info font-loader output-stream)
      (dump-prep-info font-loader output-stream)
      (dump-vhea-info font-loader output-stream)
      (dump-vmtx-info font-loader output-stream)
      #+ignore
      (loop ;; for table-info being the hash-values of (tables font-loader)
          for (name nil nil) in ipa-mincho-table-name-offset-size
          for table-info = (gethash (tag->number name) (tables font-loader))
          for table-position from start-table by 16
          for offset = (offset table-info)
          for size = (size table-info)
          for checksum = (calc-table-checksum offset size output-stream)
          do (print (list name offset size))
             (file-position output-stream table-position)
             (write-uint32 (tag->number (name table-info)) output-stream)
             (write-uint32 checksum output-stream)
             (write-uint32 offset output-stream)
             (write-uint32 size output-stream))
      (loop for name in table-list
          for table-info = (table-info name font-loader)
          for table-position from start-table by 16
          for offset = (offset table-info)
          for size = (size table-info)
          for checksum = (calc-table-checksum offset size output-stream)
          do (print (list name offset size))
             (file-position output-stream table-position)
             (write-uint32 (tag->number (name table-info)) output-stream)
             (write-uint32 checksum output-stream)
             (write-uint32 offset output-stream)
             (write-uint32 size output-stream)))))

(defun dump-font-loader-to-stream (font-loader output-stream &optional (table-name-list '("cvt " "fpgm" "glyf" "head" "hhea" "hmtx" "loca" "maxp" "post" "prep" "vhea" "vmtx")))
  (let* ((magic (scaler-type font-loader))
         (table-count (length table-name-list)) ; (table-count font-loader)
         (search-range (search-range font-loader))
         (entry-selector (entry-selector font-loader))
         (range-shift (range-shift font-loader)))
    (write-uint32 magic output-stream)
    (write-uint16 table-count output-stream)
    (write-uint16 search-range output-stream)
    (write-uint16 entry-selector output-stream)
    (write-uint16 range-shift output-stream)
    (let ((start-table (file-position output-stream))
          (table-offset (loop repeat (hash-table-count (tables font-loader))
                            sum 16))
          (tables (loop for table-info being the hash-values of (tables font-loader)
                      using (hash-key number)
                      collect (cons (number->tag number) table-info))))
      (advance-file-position output-stream table-offset)
      (let ((sorted-table-list
             (sort tables #'< :key #'(lambda (x) (offset (cdr x))))))
        (loop for (name . nil) in sorted-table-list
            do (dump-table-info name font-loader output-stream table-name-list))
        (loop for name in table-name-list
            for table-info = (table-info name font-loader)
            for table-position from start-table by 16
            for offset = (offset table-info)
            for size = (size table-info)
            for checksum = (calc-table-checksum offset size output-stream)
            do (print (list name offset size))
               (file-position output-stream table-position)
               (write-uint32 (tag->number (name table-info)) output-stream)
               (write-uint32 checksum output-stream)
               (write-uint32 offset output-stream)
               (write-uint32 size output-stream))))))
  
(defun dump-table-info (name font-loader output-stream &optional table-name-list)
  (cond ((not (member name table-name-list :test #'string=))
         (change-table-size name 0 font-loader))
        ((string= name "cvt ")
         (dump-cvt--info font-loader output-stream))
        ((string= name "fpgm")
         (dump-fpgm-info font-loader output-stream))
        ((string= name "hhea")
         (dump-hhea-info font-loader output-stream))
        ((string= name "maxp")
         (dump-maxp-info font-loader output-stream))
        ((string= name "post")
         (dump-post-info font-loader output-stream))
        ((string= name "prep")
         (dump-prep-info font-loader output-stream))
        ((string= name "vhea")
         (dump-vhea-info font-loader output-stream))
        ((string= name "glyf")
         (dump-glyf-info font-loader output-stream))
        ((string= name "loca")
         (dump-loca-info font-loader output-stream))
        ((string= name "hmtx")
         (dump-hmtx-info font-loader output-stream))
        ((string= name "vmtx")
         (dump-vmtx-info font-loader output-stream))
        ((string= name "head")
         (dump-head-info font-loader output-stream))
        ((string= name "cmap")
         (dump-cmap-info font-loader output-stream))
        ((string= name "name")
         (dump-name-info font-loader output-stream))
        ((string= name "gasp")
         (dump-gasp-info font-loader output-stream))
        (t
         (error "Unknown Table Name ~A." name))))
