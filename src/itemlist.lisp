(in-package :danmachi)
(defitems
    (cider
     expendables
     "サイダー" "砂糖水。hp回復"
     :effect (lambda (game)
	       (incf (hp (player game)) 50)))
    (soad-pop
     expendables
     "ラムネ" "砂糖水。mp回復"
     :effect (lambda (game)
	       (incf (mp (player game)) 50)))
  (coke
   expendables
   "コーラ" "砂糖水。mpとhp回復"
   :effect (lambda (game)
	     (incf (mp (player game)) 50)
	     (incf (hp (player game)) 50)))
  (string
   expendables
   "ヒモキュー" "グミ。ダンジョンから街に帰還できる")
  
  (sword-of-wood
   weapon
   "木の棒"  "ただの木の棒"
   :atk 1
   :df 0
   :atk-area (list 96 48))
  (bad-cloths
   protect
   "ボロい服" "ゴミ箱から拾ってきた"
   :atk 0
   :df 1)
  
  (fang
   material
   "チョトツのキバ" "チョトツからとれるキバ")
  (meat
   material
   "チョトツの肉" "うまい")
  (needle
   material
   "火針" "シヌガヨイの針")
  (honey
   material
   "はちみつ" "蜂の努力の結晶")
  (root
   material
   "モドキの根" "モドキの根っこ")
  (bill
   material
   "サギマガイのくちばし" "詐欺")
  (grass
   material
   "野草" "そのへんに生えてた")
  (magic-grass
   material
   "マジック草" "マジくそ")
  (iron
   material
   "鉄" "そこそこ硬い")
  (crystal
   material
   "結晶" "きれい"))
