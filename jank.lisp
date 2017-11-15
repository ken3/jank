;; #!/usr/bin/env sbcl

;; 機能: 麻雀の役判定と点数計算
;; 作成: 2017-11-14  ken3@nurs.or.jp
;; 更新: 2017-11-15  ken3@nurs.or.jp

;; 連番生成
(defun iota (m &optional (n 1) (step 1))
  (if (> n m) nil (cons n (iota m (+ n step) step))))

;; 左畳み込み
(defun foldl (fn a ls)
  (let ((acc a))
    (dolist (x ls acc)
      (setf acc (funcall fn acc x)))))

;; 右畳み込み
(defun foldr (fn a ls)
  (let ((acc a))
    (dolist (x (reverse ls) acc)
      (setf acc (funcall fn x acc)))))

;; 関数合成
(defun compose (fn &rest functions)
  (reduce (lambda (f g)
            (lambda (&rest args)
              (funcall f (apply g args))))
          functions
          :initial-value fn))

;; 破壊的ではない昇順ソート
(defun sort-safely (sequence)
  (let ((r (copy-list sequence)))
    (sort r #'<)))

;; フィルタ
(defun filter (pred ls)
  (cond ((null ls) nil)
        ((funcall pred (car ls))
         (cons (car ls) (filter pred (cdr ls))))
        (t (filter pred (cdr ls)))))

;; ZIP
(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

;; リストから指定した要素を削除する
;; 標準関数のdeleteとの違いは、重複要素がある場合に1つだけ削除すること。
;; * (delete-1 3 '(1 1 2 2 3 3 4 4 5 5 6 6 7 7))
;; -> (1 1 2 2 3 4 4 5 5 6 6 7 7)
(defun _delete-1 (x y)
  (cond ((null (cdr y)) nil)
        ((eq (cadr y) x) (rplacd y (cddr y)))
        (t (_delete-1 x (cdr y)))))
(defun delete-1 (x y)
  (let ((r (cons 'dummy (copy-list y))))
    (_delete-1 x r)
    (cdr r)))

;; 日本語文字列表示
(defun show_as_utf8 (string) (print string))

;; 表示用データ
(defparameter image
  '(
    ;; 端末が漢字表示に対応している場合はこちらの image を使用する。
    "00" "一" "二" "三" "四" "五" "六" "七" "八" "九"  ;; 萬子
    "10" "①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨"  ;; 筒子
    "20" "Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ"  ;; 索子
    "30" "東" "32" "南" "34" "西" "36" "北" "38" "39"  ;; 風牌
    "40" "白" "42" "發" "44" "中"                      ;; 三元牌
#|;;==================================================================
    ;; 端末が漢字表示に対応していない場合はこちらの image を使用する。
    "00" "M1" "M2" "M3" "M4" "M5" "M6" "M7" "M8" "M9"  ;; 萬子
    "10" "P1" "P2" "P3" "P4" "P5" "P6" "P7" "P8" "P9"  ;; 筒子
    "20" "S1" "S2" "S3" "S4" "S5" "S6" "S7" "S8" "S9"  ;; 索子
    "30" "We" "32" "Ws" "34" "Ww" "36" "Wn" "38" "39"  ;; 風牌
    "40" "Dw" "42" "Dg" "44" "Dr"                      ;; 三元牌
|#;;==================================================================
  ))

;; 牌種データ
(defparameter wanzu  '(1 2 3 4 5 6 7 8 9))
(defparameter pinzu  '(11 12 13 14 15 16 17 18 19))
(defparameter souzu  '(21 22 23 24 25 26 27 28 29))
(defparameter routou '(1 9 11 19 21 29))
(defparameter kaze   '(31 33 35 37))
(defparameter dragon '(41 43 45))
(defparameter yaochu '(1 9 11 19 21 29 31 33 35 37 41 43 45))
(defparameter greens '(22 23 24 26 28 43))
(defparameter range  (iota 45 0))

;; 手牌の組み合わせ
;; data Mentsu = Twins    -- 対子 [1,1]             => ("Twins" 1)
;;             | Triplets -- 刻子 [1,1,1]           => ("Triplets" 1)
;;             | Series   -- 順子 [1,2,3]           => ("Series" 1)
;;             | Rest     -- 残り [1,3,5]           => ("Rest" (1 3 5))
;;             | Kokushi  -- 国士 [1,1,9,...,43,45] => ("Kokushi" (1))
;;               deriving (Show, Enum, Eq, Ord)
;; type Hand  = (Mentsu,[Int])
;; type Hands = [Hand]

;; Handsコンストラクタ
;; cTwins    :: [Int] -> Hand -- (cTwins '(1))    => ("Twins" 1)
;; cTriplets :: [Int] -> Hand -- (cTriplets '(1)) => ("Triplets" 1)
;; cSeries   :: [Int] -> Hand -- (cSeries '(1))   => ("Series" 1)
;; cRest     :: [Int] -> Hand -- (cRest '(1 3 5)) => ("Rest" 1 3 5)
;; cKokushi  :: [Int] -> Hand -- (cKokushi '(1))  => ("Kokushi" 1)
(defun cTwins (x)
  (cond ((listp x)(cons "Twins" x))
        ((integerp x) (list "Twins" x))
        (t '(nil))))
(defun cTriplets (x)
  (cond ((listp x)(cons "Triplets" x))
        ((integerp x) (list "Triplets" x))
        (t '(nil))))
(defun cSeries (x)
  (cond ((listp x)(cons "Series" x))
        ((integerp x) (list "Series" x))
        (t '(nil))))
(defun cRest (x)
  (cond ((listp x) (cons "Rest" (sort-safely x)))
        (t '(nil))))
(defun cKokushi (x)
  (cond ((listp x) (let ((v (car x))) (list "Kokushi" v)))
        ((integerp x) (list "Kokushi" x))
        (t '(nil))))

;; Handを[Int]に変換する
;; unbox :: Hand -> [Int]
;; * (unbox '("Series" 2))
;; -> (2 3 4)
;; * (unbox '("Series" 2 12 22))
;; -> (2 3 4) ;; FIXME!!
(defun make-series (v) (list v (+ 1 v) (+ 2 v)))
(defun _unbox (hand)
  (let ((type (car hand))
        (args (cdr hand))
        (c (cadr hand)))
    (cond
      ((string= type "Twins")
        (cond ((integerp c)(list c c))
              ((listp c)(append c c))
              (t '(nil))))
      ((string= type "Triplets")
        (cond ((integerp c)(list c c c))
              ((listp c)(append c c c))
              (t '(nil))))
      ((string= type "Series")
        (cond ((integerp c)(make-series c))
              ((listp c)(foldr (lambda (x r) (append (make-series x) r)) nil c))
              (t '(nil))))
      ((string= type "Rest")
        (cond ((listp args) args)
              ((integerp c)(list c))
              (t '(nil))))
      ((string= type "Kokushi")
        (cond ((integerp c)(sort-safely (cons c yaochu)))
              (t '(nil))))
      (t '(nil)))))
(defun unbox (hand) (sort-safely (_unbox hand)))

;; [Int]の差集合を返す(標準関数のdeleteと違い、重複要素をすべて削除することはしない)
;; subset :: [Int] -> [Int] -> [Int]
;; * (subset '(1 1 2 2 3 3 4 4 5 5 6 6 7 7) '(1 2 3))
;; -> (1 2 3 4 4 5 5 6 6 7 7)
(defun subset (a b) (foldr #'delete-1 a b))

;; RestにHandを加算する
;; add :: Hand -> Hand -> Hand
;; * (add '("Rest" (1 2 3)) '("Rest" (2 3 4)))
;; -> ("Rest" (1 2 2 3 3 4))
(defun add (x y)
  (cRest (append (unbox x) (unbox y))))

;; RestからHandを減算する
;; sub :: Hand -> Hand -> Hand
;; * (sub '("Rest" (1 2 3 4 5 6)) '("Rest" (2 3 4)))
;; -> ("Rest" (1 5 6))
(defun sub (x y)
  (cRest (subset (unbox x) (unbox y))))

;; [Int]を文字列化する
;; to_string :: [Int] -> String
(defun to_string (x)
  (format nil "~A" (mapcar (lambda (i) (nth i image)) x)))

;; Handを文字列化する
;; show_hand :: Hand -> String
(defun show_list (list)
  (foldr (lambda (r x) (concatenate 'string r x)) "" list))
(defun show_hand (hand)
  (let ((type (car hand))
        (c (cadr hand)))
    (cond
      ((string= type "Twins")
        ;; (show_hand '("Twins" 35))
        ;; -> "(西 西)"
        ;; (show_hand '("Twins" (2 7 11 18 25 35 43)))
        ;; -> "(二 二)(七 七)(① ①)(⑧ ⑧)(Ⅴ Ⅴ)(西 西)(發 發)"
        (cond ((integerp c)(to_string (list c c)))
              ((listp c) (show_list (mapcar (lambda (x) (to_string (list x x))) c)))
              (t '(nil))))
      ((string= type "Triplets")
        ;; (show_hand '("Triplets" 35))
        ;; -> "(西 西 西)"
        (cond ((integerp c)(to_string (list c c c)))
              ((listp c) (show_list (mapcar (lambda (x) (to_string (list x x x))) c)))
              (t '(nil))))
      ((string= type "Series")
        ;; (show_hand '("Series" 11))
        ;; -> "(① ② ③)"
        (cond ((integerp c) (to_string (list c (+ 1 c) (+ 2 c))))
              ((listp c) (to_string (foldr (lambda (x r) (append (make-series x) r)) nil c)))
              (t '(nil))))
      ((string= type "Rest")
        ;; (show_hand '("Rest" (7 11 15 21 33)))
        ;; -> "(七 ① ⑤ Ⅰ 南)"
        (cond ((integerp c) '(nil))
              ((listp c)(to_string c))
              (t '(nil))))
      ((string= type "Kokushi")
        ;; (show_hand '("Kokushi" 11))
        ;; -> "(一 九 ① ① ⑨ Ⅰ Ⅸ 東 南 西 北 白 發 中)"
        (cond ((integerp c) (to_string (sort-safely (cons c yaochu))))
              ((listp c) '(nil))
              (t '(nil))))
      (t '(nil)))))

;; Handsを文字列化する
;; show_hands :: Hands -> String
;; * (show_hands '("Kokushi" 11))
;; -> "[(一 九 ① ① ⑨ Ⅰ Ⅸ 東 南 西 北 白 發 中)]"
(defun show_hands (hs)
  (concatenate 'string "[" (show_hand hs) "]"))

;; [Hands]を文字列化する
;; show_hands_array :: [Hands] -> [String]
;; * (show_hands_array '(("Triplets" (22))("Twins" 19)))
;; -> ("[(Ⅱ Ⅱ Ⅱ)]" "[(⑨ ⑨)]")
(defun show_hands_array (a)
  (mapcar #'show_hands a))

;; ヒストグラムを返す
;; * (histogram '(1 9 11 19 21 29 31 33 35 37 41 43 45 45))
;; -> (0 1 0 0 0 0 0 0 0 1 0 1 ... 1 0 2)
;; histogram :: [Int] -> [Int]
(defun histogram (a)
  (mapcar (lambda (n) (length (filter (lambda (x) (eql n x)) a))) range))

;; 国士無双判定
;; pick_kokushi :: [Int] -> Hands
;; * (pick_kokushi '(1 9 11 19 21 29 31 33 35 37 41 43 45 45))
;; -> ("Kokushi" 45)
(defun pick_kokushi (body)
  (let* ((p (intersection body yaochu))
         (u (remove-duplicates p))
         (d (subset p u)))
    (if (and (eql (length p) 14) (eql (length u) 13)) (cKokushi d) nil)))

;; 七対子判定
;; pick_7pairs :: [Int] -> Hands
;; * (pick_7pairs '(1 1 2 2 3 3 4 4 5 5 6 6 7 7))
;; -> ("Twins" 1 2 3 4 5 6 7)
(defun pick_7pairs (body)
  (let* ((h (zip range (histogram body)))
         (p (mapcar #'car (filter (lambda (x) (eql (cadr x) 2)) h))))
    (if (eql (length p) 7) (cTwins p) nil)))

;; 雀頭候補を返す
;; pick_twins :: [Int] -> Hands
;; * (pick_twins '(1 1 2 2 3 3 4 4 5 5 5 6 6 6))
;; -> (("Twins" 1) ("Twins" 2) ("Twins" 3) ("Twins" 4) ("Twins" 5) ("Twins" 6))
(defun pick_twins (body)
  (let* ((h (zip range (histogram body)))
         (p (mapcar #'car (filter (lambda (x) (>= (cadr x) 2)) h))))
    (mapcar #'cTwins p)))

;; 刻子候補を返す
;; pick_triplets :: [Int] -> Hands
;; * (pick_triplets '(1 1 2 2 3 3 4 4 5 5 5 6 6 6))
;; -> (("Triplets" 5) ("Triplets" 6))
(defun pick_triplets (body)
  (let* ((h (zip range (histogram body)))
         (p (mapcar #'car (filter (lambda (x) (>= (cadr x) 3)) h))))
    (mapcar #'cTriplets p)))

;; 順子候補を返す
;; pick_series :: [Int] -> Hands
;; * (pick_series '(1 1 2 2 3 3 4 4 5 5 5 6 6 6))
;; -> (("Series" 1) ("Series" 2) ("Series" 3) ("Series" 4))
(defun product3 (h0)
  (let* ((h1 (cdr h0))
         (h2 (cdr h1))
         (p (zip h0 h1 h2)))
    (mapcar (lambda (x) (* (car x) (cadr x) (caddr x))) p)))
(defun pick_series (body)
  (let* ((h (histogram body))
         (z (zip range (product3 h)))
         (p (mapcar #'car (filter (lambda (x) (/= 0 (cadr x))) z))))
    (mapcar #'cSeries p)))

;; 未確定牌(Rest要素)の中にある刻子と順子を返す
;; find_trios :: [Int] -> Hands
;; * (find_trios '(5 5 5 6 6 6 7 7 7))
;; -> (("Triplets" 5) ("Triplets" 6) ("Triplets" 7) ("Series" 5))
(defun find_trios (a)
  (append (pick_triplets a) (pick_series a)))

;; 手牌の中からTwins要素を探し、マージした結果を返す
;; find_twins' :: [Int] -> Hands -> [Int]
;; * (find_twins '(("Twins" 11 15)("Twins" 21)("Rest" 5 7 9)))
;; -> ("Twins" 11 15 21)
(defun _find_twins (list hands)
  (cond ((null hands) list)
        ((string= "Twins" (caar hands)) (_find_twins (append list (cdar hands)) (cdr hands)))
        (t (_find_twins list (cdr hands)))))
;; find_twins :: Hands -> Hands
(defun find_twins (hands)
  (let* ((r (_find_twins nil hands)))
    (if (null r) nil (cTwins (sort-safely r)))))

;; 手牌の中からTriplets要素を探し、マージした結果を返す
;; find_triplets' :: [Int] -> Hands -> [Int]
;; * (find_triplets '(("Twins" 1)("Triplets" 2)("Triplets" 4)("Rest" 5 7 9)))
;; -> ("Triplets" 2 4)
(defun _find_triplets (list hands)
  (cond ((null hands) list)
        ((string= "Triplets" (caar hands)) (_find_triplets (append list (cdar hands)) (cdr hands)))
        (t (_find_triplets list (cdr hands)))))
;; find_triplets :: Hands -> Hands
(defun find_triplets (hands)
  (let* ((r (_find_triplets nil hands)))
    (if (null r) nil (cTriplets (sort-safely r)))))

;; 手牌の中からSeries要素を探し、マージした結果を返す
;; find_series' :: [Int] -> Hands -> [Int]
;; * (find_series '(("Twins" 1)("Triplets" 11)("Series" 21)("Series" 2)))
;; -> ("Series" 2 21)
(defun _find_series (list hands)
  (cond ((null hands) list)
        ((string= "Series" (caar hands)) (_find_series (append list (cdar hands)) (cdr hands)))
        (t (_find_series list (cdr hands)))))
;; find_series :: Hands -> Hands
(defun find_series (hands)
  (let* ((r (_find_series nil hands)))
    (if (null r) nil (cSeries (sort-safely r)))))

;; 手牌の中からKokushi要素を探して返す
;; 手牌の中のKokushi要素は高々1つしか無いはず。
;; * (find_kokushi '(("Kokushi" 1)))
;; -> ("Kokushi" 1)
;; find_kokushi' :: Hands -> [Int]
(defun _find_kokushi (hands)
  (cond ((null hands) nil)
        ((string= "Kokushi" (caar hands)) (cdar hands))
        (t (_find_kokushi (cdr hands)))))
;; find_kokushi :: Hands -> Hands
(defun find_kokushi (hands)
  (let* ((r (_find_kokushi hands)))
    (if (null r) nil (cKokushi (sort-safely r)))))

;; 手牌の中から確定牌(Rest要素以外)を探して返す
;; find_fixed :: Hands -> Hands
;; *Main> find_fixed [(Twins,[1]),(Series,[2]),(Rest,[4,7,9])]
;; [(Twins,[1]),(Series,[2])]
;; * (find_fixed '(("Twins" 1)("Series" 2)("Rest" 4 7 9)))
;; -> (("Twins" 1) ("Series" 2))
(defun find_fixed (x)
  (let* ((r1 (find_kokushi x))
         (r2 (find_twins x))
         (r3 (find_triplets x))
         (r4 (find_series x)))
    (remove nil (cons r1 (cons r2 (cons r3 (cons r4 nil)))))))

;; 手牌の中から未確定牌(Rest要素)を探し、マージした結果を返す
;; find_rest :: Hands -> Hands
;; *Main> find_rest [(Twins,[1]),(Series,[2]),(Rest,[5,5,6,6,7,7])]
;; [(Rest,[5,5,6,6,7,7])]
;; * (find_rest '(("Twins" 1)("Series" 2)("Rest" 5 5 6 6 7 7)))
;; -> ("Rest" 5 5 6 6 7 7)
;; * (find_rest '(("Twins" 1)("Series" 2)("Rest" (5 5 6 6 7 7))))
;; -> ("Rest" (5 5 6 6 7 7)) ;; FIXME!! ("Rest" 5 5 6 6 7 7) としたい。
(defun _find_rest (list hands)
  (cond ((null hands) list)
        ((string= "Rest" (caar hands)) (_find_rest (append list (unbox (car hands))) (cdr hands)))
        (t (_find_rest list (cdr hands)))))
(defun find_rest (hands)
  (let* ((r (_find_rest nil hands)))
    (if (null r) nil (cRest (sort-safely r)))))

;; 手牌の並びを正規化する
;; sorthands :: Hands -> Hands
;; *Main > sorthands $ pick_twins [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
;; [(Twins,[1,2,3,4,5,6])]
;; *Main > sorthands [(Twins,[1]),(Rest,[5,7]),(Rest,[6]),(Twins,[7,5])]
;; [(Twins,[1,5,7]),(Rest,[5,6,7])]
(defun sorthands (x)
  (append (find_fixed x) (find_rest x)))

;; Rest要素のメンツを1つ仮確定し、組み合わせを更新する
;; proceed1 :: [Hand] -> [Hands]
;; *Main > p $ proceed1 [(Twins,[1]),(Series,[2]),(Rest,[5,5,5,6,6,6,7,7,7])]
;;     [(Twins,[1]),(Triplets,[5]),(Series,[2]),(Rest,[6,6,6,7,7,7])]
;;     [(Twins,[1]),(Triplets,[6]),(Series,[2]),(Rest,[5,5,5,7,7,7])]
;;     [(Twins,[1]),(Triplets,[7]),(Series,[2]),(Rest,[5,5,5,6,6,6])]
;;     [(Twins,[1]),(Series,[2,5]),(Rest,[5,5,6,6,7,7])]
;; proceed1 hands = map apply mentsu
;;   where rest    = head $ find_rest hands
;;         fixed   = find_fixed hands
;;         mentsu  = find_trios $ unbox rest
;;         apply x = sorthands $ fixed ++ [x] ++ (remove_from rest x)
;;         remove_from hand mentsu | null rest' = []
;;                                 | otherwise  = [(Rest,sort rest')]
;;           where rest' = subset (unbox hand) (unbox mentsu)

(defun remove_from (hand mentsu)
  (let* ((rest (subset (unbox hand) (unbox mentsu))))
    (cond ((null rest) nil)
          (t (cRest (sort-safely rest))))))

(defun proceed1 (hands)
  (let* ((rest (car (find_rest hands)))
         (fixed (find_fixed hands))
         (mentsu (find_trios (unbox rest)))
         (proc (lambda (x) (sorthands (append fixed (list x) (remove_from rest x))))))
    (mapcar proc mentsu)))

;; 1雀頭+N面子を確定する
;; solve' :: [Hands] -> [Hands]
;; Rest要素が無くなれば再帰呼び出しを終了する(count == 0)
;; *Main > solve' [[(Twins,[1]),(Rest,[14,14,15,15,16,16,18,18,18])]]
;; [[(Twins,[1]),(Triplets,[18]),(Series,[14,14])]]
;; solve' hands | count == 0 = r
;;              | otherwise  = solve' r
;;   where r = nub $ concatMap proceed1 hands -- nubを使って重複要素を削除する
;;         count = length $ filter has_rest r
;;         has_rest []           = False
;;         has_rest ((Rest,_):_) = True
;;         has_rest (_:xs)       = has_rest xs

(defun nub (x) x) ;; FIXME: 重複要素を削除する機能を持たせるが、現状NOP
(defun concat (x) (foldr #'append nil x))       ;; Haskellのconcat
(defun concatmap (fn x) (concat (mapcar fn x))) ;; HaskellのconcatMap
(defun has_rest (list)
  (cond ((null list) nil)
        ((string= "Rest" (car list)) t)
        (t (has_rest (cdr list)))))
(defun _solve (hands)
  (let* ((r (nub (concatmap #'proceed1 hands)))
         (count (length (filter #'has_rest r))))
    (if (eql count 0) r (_solve r))))

;; アガリが成立する組み合せの集合を返す
;; solve :: [Int] -> [Hands]
;; *Main> p $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,19]
;;     [(Kokushi,[19])]
;; *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
;;     [(Twins,[1,2,3,4,5,6,7])]
;;     [(Twins,[1]),(Series,[2,2,5,5])]
;;     [(Twins,[4]),(Series,[1,1,5,5])]
;;     [(Twins,[7]),(Series,[1,1,4,4])]
;; *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
;;     [(Twins,[1]),(Triplets,[5,6]),(Series,[2,2])]
;;     [(Twins,[4]),(Triplets,[5,6]),(Series,[1,1])]
(defun split (body x)
  (list (cTwins (list x)) (cRest (subset body (list x x)))))

(defun solve (body)
  (let* ((r1 (pick_7pairs body))
         (r2 (pick_kokushi body))
         (hands (mapcar (lambda (x) (split body (car (unbox x)))) (pick_twins body)))
         (r3 (_solve hands)))
    (filter (lambda (x) (not (null x))) (cons r1 (cons r2 r3)))))

  ;; 手牌から雀頭を括りだす
  ;; body  = [1,1,9,9,19,20,21,31,43,44]
  ;; ts    = [[1,1],[9,9]]
  ;; hands = [[(Twins,[1]),(Rest,[9,9,19,20,21,31,43,44])],
  ;;          [(Twins,[9]),(Rest,[1,1,19,20,21,31,43,44])]]
  ;; where r1 = pick_7pairs  body  -- 七対子判定
  ;;       r2 = pick_kokushi body  -- 国士無双判定
  ;;       r3 = solve' hands       -- 1雀頭+N面子判定
  ;;       hands   = map (split . head . unbox) $ pick_twins body
  ;;       split x = (Twins,[x]):[(Rest,subset body [x,x])]



;; 実行結果を出力するための共通サービス関数
;; phands :: (Hands -> String) -> [Hands] -> IO ()
(defun phands (hands)
  (mapcar (lambda (x) (print (format nil "    ~A" x))) hands))

;; ASCIIリスト形式で出力する
;; p :: [Hands] -> IO ()
(defun p (hands)
  (phands hands))

;; 麻雀牌(image)形式で出力する
;; pp :: [Hands] -> IO ()
(defun pp (hands)
  (phands (concat (mapcar #'show_hand hands))))

;; メイン関数
;; main :: IO ()
;; * (main)
;; -> TYPE-ERROR: The value "Rest" is not of type LIST.
;;    (_UNBOX "Rest") ;; FIXME!!
(defun main ()
  (let ((m1 '(1 9 11 19 21 29 31 33 35 37 41 43 45 45))
        (m2 '(1 1 2 2 3 3 4 4 5 5 6 6 7 7))
        (m3 '(1 1 2 2 2 3 3 3 4 4 4 26 27 28)))
    (progn (print (format nil "~A" m1))
           (pp (solve m1))
           (print (format nil "~A" m2))
           (pp (solve m2))
           (print (format nil "~A" m3))
           (pp (solve m3)))))

;; テスト
;; # 国士無双
;; *Main> p $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
;; [(Kokushi,[45])]
;; *Main> pp $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
;;     [M1,M9,P1,P9,S1,S9,We,Ws,Ww,Wn,Dw,Dg,Dr,Dr]
;; # 七対子
;; *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
;;     [(Twins,[1,2,3,4,5,6,7])]
;;     [(Twins,[1]),(Series,[2,2,5,5])]
;;     [(Twins,[4]),(Series,[1,1,5,5])]
;;     [(Twins,[7]),(Series,[1,1,4,4])]
;; *Main> pp $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
;;     [M1,M1][M2,M2][M3,M3][M4,M4][M5,M5][M6,M6][M7,M7]
;;     [M1,M1][M2,M3,M4][M2,M3,M4][M5,M6,M7][M5,M6,M7]
;;     [M4,M4][M1,M2,M3][M1,M2,M3][M5,M6,M7][M5,M6,M7]
;;     [M7,M7][M1,M2,M3][M1,M2,M3][M4,M5,M6][M4,M5,M6]
;; # 三連刻
;; *Main> p $ solve [1,1,2,2,2,3,3,3,4,4,4,26,27,28]
;;     [(Twins,[1]),(Triplets,[2,3,4]),(Series,[26])]
;;     [(Twins,[1]),(Series,[2,2,2,26])]
;;     [(Twins,[4]),(Series,[1,1,2,26])]
;; *Main> pp $ solve [1,1,2,2,2,3,3,3,4,4,4,26,27,28]
;;     [M1,M1][M2,M2,M2][M3,M3,M3][M4,M4,M4][S6,S7,S8]
;;     [M1,M1][M2,M3,M4][M2,M3,M4][M2,M3,M4][S6,S7,S8]
;;     [M4,M4][M1,M2,M3][M1,M2,M3][M2,M3,M4][S6,S7,S8]

