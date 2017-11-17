
;; 機能: 麻雀の役判定と点数計算
;; 作成: 2017-11-14  ken3@nurs.or.jp
;; 更新: 2017-11-17  ken3@nurs.or.jp

;; 連番生成
(defun iota (m &optional (n 1) (step 1))
  (if (> n m) nil (cons n (iota m (+ n step) step))))

;; 関数合成
(defun compose (fn &rest functions)
  (reduce (lambda (f g)
            (lambda (&rest args)
              (funcall f (apply g args))))
          functions
          :initial-value fn))

;; ZIP
(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

;; Haskellのconcat
(defun concat (x) (reduce #'append x))

;; HaskellのconcatMap
(defun concatmap (fn x) (concat (mapcar fn x)))

;; 2つのリストを比較する
;; 並び順が同じであることが前提。
;; * (compare-list #'eql '(7 11 13) '(7 11 13))
;; -> T
(defun compare-list (fn a b)
  (if (eql (length a) (length b))
    (let* ((z (zip a b))
           (r (mapcar (lambda (x) (funcall fn (car x) (cadr x))) z)))
      (eql 0 (count nil r))) nil))

;; 2つのHandを比較する
;; * (compare-hand '(Twins 1 5 7) '(Twins 1 5 7))
;; -> T
(defun compare-hand (a b)
  (if (eq (car a) (car b))
    (compare-list #'eql (cdr a) (cdr b)) nil))

;; 2つのHandsを比較する
;; * (compare-hands '((Twins 1) (Free 5 6)) '((Twins 1) (Free 5 6)))
;; -> T
(defun compare-hands (a b)
  (compare-list #'compare-hand a b))

;; Hands要素を追加する
;; 追加しようとする要素が既に存在する場合には追加しない。
;; * (add-if-uniq '(((Twins 1) (Free 5)) ((Twins 1) (Free 7))) '((Twins 1) (Free 9)))
;; -> (((TWINS 1) (FREE 9)) ((TWINS 1) (FREE 5)) ((TWINS 1) (FREE 7)))
;; * (add-if-uniq '(((Twins 1) (Free 5)) ((Twins 1) (Free 7))) '((Twins 1) (Free 7)))
;; -> (((TWINS 1) (FREE 5)) ((TWINS 1) (FREE 7)))
(defun add-if-uniq (acc hand)
  (let ((c (mapcar (lambda (k) (compare-hands hand k)) acc)))
    (if (eql 0 (count t c)) (cons hand acc) acc)))

;; 手配を昇順にソートする
;; * (sort-list '(21 25 9 31 4 19 41 1))
;; -> (1 4 9 19 21 25 31 41)
(defun sort-list (sequence)
  (let ((r (copy-list sequence)))
    (sort r #'<)))

;; リストから指定した要素を削除する
;; 重複要素がある場合には1つだけ削除し、残りのものは削除しない。
;; * (remove-just1 3 '(1 1 2 2 3 3 4 4 5 5 6 6 7 7))
;; -> (1 1 2 2 3 4 4 5 5 6 6 7 7)
(defun remove-just1* (x y)
  (cond ((null (cdr y)) nil)
        ((eq (cadr y) x) (rplacd y (cddr y)))
        (t (remove-just1* x (cdr y)))))
(defun remove-just1 (x y)
  (let ((r (cons 'dummy (copy-list y))))
    (remove-just1* x r)
    (cdr r)))

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
;; data Mentsu = Twins    -- 対子 [1,1]             => (TWINS 1)
;;             | Triplets -- 刻子 [1,1,1]           => (TRIPLETS 1)
;;             | Series   -- 順子 [1,2,3]           => (SERIES 1)
;;             | Free     -- 残り [1,3,5]           => (FREE 1 3 5)
;;             | Kokushi  -- 国士 [1,1,9,...,43,45] => (KOKUSHI 1)
;; type Hand  = (Mentsu,[Int])
;; type Hands = [Hand]

;; Handsコンストラクタを定義するテンプレートマクロ
(defmacro make-constructor (name)
    `(setf (symbol-function ,name)
       #'(lambda (&rest args)
         (let* ((arg1 (car args))
                (h (if (listp arg1) arg1 args)))
           (cond ((listp arg1) (cons ,name h))
                 ((integerp arg1) (cons ,name h))
                 (t nil))))))

;; Handコンストラクタ
;; (Twins 1 2 3)     => (TWINS 1 2 3)
;; (Twins '(1 2 3))  => (TWINS 1 2 3)
(make-constructor 'Twins)    ;; :Twins    :: [Int] -> Hand
(make-constructor 'Triplets) ;; :Triplets :: [Int] -> Hand
(make-constructor 'Series)   ;; :Series   :: [Int] -> Hand
(make-constructor 'Free)     ;; :Free     :: [Int] -> Hand
(make-constructor 'Kokushi)  ;; :Kokushi  :: [Int] -> Hand

;; Handを[Int]に変換する
;; unbox :: Hand -> [Int]
;; * (unbox '(Series 2))
;; -> (2 3 4)
;; * (unbox '(Series 2 12 22))
;; -> (2 3 4 12 13 14 22 23 24)
(defun make-series (v) (list v (+ 1 v) (+ 2 v)))
(defun unbox* (k h)
  (if (listp h)
    (cond ((eq k 'Twins)    (concat (mapcar (lambda (x) (list x x)) h)))
          ((eq k 'Triplets) (concat (mapcar (lambda (x) (list x x x)) h)))
          ((eq k 'Series)   (concat (mapcar #'make-series h)))
          ((eq k 'Free)     h)
          ((eq k 'Kokushi)  (sort-list (cons (car h) yaochu)))
          (t nil)) nil))
(defun unbox (hand)
  (let* ((r (sort-list (unbox* (car hand) (cdr hand))))
         (h (car r)))
    (cond ((listp h) h)
          ((integerp h) r)
          (t nil))))

;; [Int]の差集合を返す(標準関数のdeleteと違い、重複要素をすべて削除することはしない)
;; subset :: [Int] -> [Int] -> [Int]
;; * (subset '(1 1 2 2 3 3 4 4 5 5 6 6 7 7) '(1 2 3))
;; -> (1 2 3 4 4 5 5 6 6 7 7)
(defun subset (a b) (reduce #'remove-just1 b :from-end t :initial-value a))

;; FreeにHandを加算する
;; add :: Hand -> Hand -> Hand
;; * (add '(Free 1 2 3) '(Free 2 3 4))
;; -> (FREE 1 2 2 3 3 4)
(defun add (x y)
  (Free (append (unbox x) (unbox y))))

;; FreeからHandを減算する
;; sub :: Hand -> Hand -> Hand
;; * (sub '(Free 1 2 3 4 5 6) '(Free 2 3 4))
;; -> (FREE 1 5 6)
(defun sub (x y)
  (Free (subset (unbox x) (unbox y))))

;; [Int]を文字列化する
;; to_string :: [Int] -> String
(defun to_string (x)
  (format nil "~A" (mapcar (lambda (i) (nth i image)) x)))

;; Handを文字列化する
;; show_hand :: Hand -> String
;; (show_hand '(Twins (2 7 11 18 25 35 43)))
;; -> "(二 二)(七 七)(① ①)(⑧ ⑧)(Ⅴ Ⅴ)(西 西)(發 發)"
;; (show_hand '(Triplets 35))
;; -> "(西 西 西)"
;; (show_hand '(Series 11))
;; -> "(① ② ③)"
;; (show_hand '(Free 7 11 15 21 33))
;; -> "(七 ① ⑤ Ⅰ 南)"
;; (show_hand '(Kokushi 11))
;; -> "(一 九 ① ① ⑨ Ⅰ Ⅸ 東 南 西 北 白 發 中)"
(defun show_list (list)
  (reduce (lambda (r x) (concatenate 'string r x)) list :from-end t :initial-value ""))
(defun show_hand* (fn hand)
  (let ((c (cadr hand)))
    (cond ((integerp c) (show_list (mapcar fn (cdr hand))))
          ((listp c) (show_list (mapcar fn c)))
          (t nil))))
(defun show_hand (hand)
  (let ((type (car hand))
        (c (cadr hand)))
    (cond
      ((eq type 'Twins) (show_hand* (lambda (x) (to_string (list x x))) hand))
      ((eq type 'Triplets) (show_hand* (lambda (x) (to_string (list x x x))) hand))
      ((eq type 'Series)(show_hand* (lambda (x) (to_string (make-series x))) hand))
      ((eq type 'Free) (cond ((integerp c)(to_string (cdr hand)))
                              ((listp c)(to_string c))
                              (t nil)))
      ((eq type 'Kokushi) (let ((h (if (listp c) (car c) c)))
                            (to_string (sort-list (cons h yaochu))))) (t nil))))

;; ヒストグラムを返す
;; * (histogram '(1 9 11 19 21 29 31 33 35 37 41 43 45 45))
;; -> (0 1 0 0 0 0 0 0 0 1 0 1 ... 1 0 2)
;; histogram :: [Int] -> [Int]
(defun histogram (a)
  (mapcar (lambda (n) (length (remove-if-not (lambda (x) (eql n x)) a))) range))

;; 国士無双判定
;; pick_kokushi :: [Int] -> Hands
;; * (pick_kokushi '(1 9 11 19 21 29 31 33 35 37 41 43 45 45))
;; -> ((KOKUSHI 45))
(defun pick_kokushi (body)
  (let* ((p (intersection body yaochu))
         (u (remove-duplicates p))
         (d (subset p u)))
    (if (and (eql (length p) 14) (eql (length u) 13)) (list (Kokushi d)) nil)))

;; 七対子判定
;; pick_7pairs :: [Int] -> Hands
;; * (pick_7pairs '(1 1 2 2 3 3 4 4 5 5 6 6 7 7))
;; -> (TWINS 1 2 3 4 5 6 7)
(defun pick_7pairs (body)
  (let* ((h (zip range (histogram body)))
         (p (mapcar #'car (remove-if-not (lambda (x) (eql (cadr x) 2)) h))))
    (if (eql (length p) 7) (list (Twins p)) nil)))

;; 雀頭候補を返す
;; pick_twins :: [Int] -> Hands
;; * (pick_twins '(1 1 2 2 3 3 4 4 5 5 5 6 6 6))
;; -> ((Twins 1) (Twins 2) (Twins 3) (Twins 4) (Twins 5) (Twins 6))
(defun pick_twins (body)
  (let* ((h (zip range (histogram body)))
         (p (mapcar #'car (remove-if-not (lambda (x) (>= (cadr x) 2)) h))))
    (mapcar #'Twins p)))

;; 刻子候補を返す
;; pick_triplets :: [Int] -> Hands
;; * (pick_triplets '(1 1 2 2 3 3 4 4 5 5 5 6 6 6))
;; -> ((TRIPLETS 5) (TRIPLETS 6))
(defun pick_triplets (body)
  (let* ((h (zip range (histogram body)))
         (p (mapcar #'car (remove-if-not (lambda (x) (>= (cadr x) 3)) h))))
    (mapcar #'Triplets p)))

;; 順子候補を返す
;; pick_series :: [Int] -> Hands
;; * (pick_series '(1 1 2 2 3 3 4 4 5 5 5 6 6 6))
;; -> ((SERIES 1) (SERIES 2) (SERIES 3) (SERIES 4))
(defun product3 (h0)
  (let* ((h1 (cdr h0))
         (h2 (cdr h1))
         (p (zip h0 h1 h2)))
    (mapcar (lambda (x) (* (car x) (cadr x) (caddr x))) p)))
(defun pick_series (body)
  (let* ((h (histogram body))
         (z (zip range (product3 h)))
         (p (mapcar #'car (remove-if-not (lambda (x) (/= 0 (cadr x))) z))))
    (mapcar #'Series p)))

;; 未確定牌(Free要素)の中にある刻子と順子を返す
;; find_trios :: [Int] -> Hands
;; * (find_trios '(5 5 5 6 6 6 7 7 7))
;; -> ((TRIPLETS 5) (TRIPLETS 6) (TRIPLETS 7) (SERIES 5))
(defun find_trios (a)
  (append (pick_triplets a) (pick_series a)))

;; 手牌の中からTwins要素を探し、マージした結果を返す
;; find_twins' :: [Int] -> Hands -> [Int]
;; * (find_twins '((Twins 11 15)(Twins 21)(Free 5 7 9)))
;; -> (TWINS 11 15 21)
(defun find_twins* (list hands)
  (cond ((null hands) list)
        ((eq 'Twins (caar hands))
          (find_twins* (append list (cdar hands)) (cdr hands)))
        (t (find_twins* list (cdr hands)))))
;; find_twins :: Hands -> Hands
(defun find_twins (hands)
  (let* ((r (find_twins* nil hands)))
    (if (null r) nil (Twins (sort-list r)))))

;; 手牌の中からTriplets要素を探し、マージした結果を返す
;; find_triplets' :: [Int] -> Hands -> [Int]
;; * (find_triplets '((Twins 1)(Triplets 2)(Triplets 4)(Free 5 7 9)))
;; -> (TRIPLETS 2 4)
(defun find_triplets* (list hands)
  (cond ((null hands) list)
        ((eq 'Triplets (caar hands))
          (find_triplets* (append list (cdar hands)) (cdr hands)))
        (t (find_triplets* list (cdr hands)))))
;; find_triplets :: Hands -> Hands
(defun find_triplets (hands)
  (let* ((r (find_triplets* nil hands)))
    (if (null r) nil (Triplets (sort-list r)))))

;; 手牌の中からSeries要素を探し、マージした結果を返す
;; find_series' :: [Int] -> Hands -> [Int]
;; * (find_series '((Twins 1)(Triplets 11)(Series 21)(Series 2)))
;; -> (SERIES 2 21)
(defun find_series* (list hands)
  (cond ((null hands) list)
        ((eq 'Series (caar hands))
          (find_series* (append list (cdar hands)) (cdr hands)))
        (t (find_series* list (cdr hands)))))
;; find_series :: Hands -> Hands
(defun find_series (hands)
  (let ((r (find_series* nil hands)))
    (if (null r) nil (Series (sort-list r)))))

;; 手牌の中からKokushi要素を探して返す
;; 手牌の中のKokushi要素は高々1つしか無いはず。
;; * (find_kokushi '((Kokushi 1)))
;; -> (KOKUSHI 1)
;; find_kokushi' :: Hands -> [Int]
(defun find_kokushi* (hands)
  (cond ((null hands) nil)
        ((eq 'Kokushi (caar hands)) (cdar hands))
        (t (find_kokushi* (cdr hands)))))
;; find_kokushi :: Hands -> Hands
(defun find_kokushi (hands)
  (let ((r (find_kokushi* hands)))
    (if (null r) nil (Kokushi (sort-list r)))))

;; 手牌の中から確定牌(Free要素以外)を探して返す
;; find_fixed :: Hands -> Hands
;; * (find_fixed '((Twins 1)(Series 2)(Free 4 7 9)))
;; -> ((TWINS 1) (SERIES 2))
(defun find_fixed (x)
  (let ((r1 (find_kokushi x))
        (r2 (find_twins x))
        (r3 (find_triplets x))
        (r4 (find_series x)))
    (remove nil (list r1 r2 r3 r4))))

;; 手牌の中から未確定牌(Free要素)を探し、マージした結果を返す
;; find_free :: Hands -> Hands
;; * (find_free '((Twins 1)(Series 2)(Free 5 5 6 6 7 7)))
;; -> ((FREE 5 5 6 6 7 7))
;; * (find_free '((Free 1)(Free 2)(Free (5 5 6 6 7 7))))
;; -> ((FREE 1 2 5 5 6 6 7 7))
(defun find_free* (list hands)
  (cond ((null hands) list)
        ((eq 'Free (caar hands))
          (find_free* (append list (unbox (car hands))) (cdr hands)))
        (t (find_free* list (cdr hands)))))
(defun find_free (hands)
  (let ((r (find_free* nil hands)))
    (if (null r) nil (list (Free (sort-list r))))))

;; 手牌の並びを正規化する
;; normalize :: Hands -> Hands
;; * (normalize (pick_twins '(1 1 2 2 3 3 4 4 5 5 5 6 6 6)))
;; -> ((TWINS 1 2 3 4 5 6))
;; * (normalize '((Twins 1)(Free 5 7)(Free 6)(Twins 7 5)))
;; -> ((TWINS 1 5 7) (FREE 5 6 7))
(defun normalize (x)
  (append (find_fixed x) (find_free x)))

;; リストから重複要素を削除する
;; * (nub '(((Twins 1))((Series 3))((Twins 1))((Series 3))))
;; -> (((SERIES 3)) ((TWINS 1)))
(defun nub (hands)
  (let ((n (mapcar #'normalize hands)))
    (reduce #'add-if-uniq n :initial-value nil)))

;; Free要素のメンツを1つ仮確定し、組み合わせを更新する
;; proceed1 :: [Hand] -> [Hands]
;; * (p (proceed1 '((Twins 1)(Series 2)(Free 5 5 5 6 6 6 7 7 7))))
;; -> ((TWINS 1) (TRIPLETS 5) (SERIES 2) (FREE 6 6 6 7 7 7))
;;    ((TWINS 1) (TRIPLETS 6) (SERIES 2) (FREE 5 5 5 7 7 7))
;;    ((TWINS 1) (TRIPLETS 7) (SERIES 2) (FREE 5 5 5 6 6 6))
;;    ((TWINS 1) (SERIES 2 5) (FREE 5 5 6 6 7 7))
(defun remove_from (hand mentsu)
  (let* ((rest (subset (unbox hand) (unbox mentsu))))
    (cond ((null rest) nil)
          (t (Free (sort-list rest))))))
(defun proceed1 (hands)
  (let* ((rest (car (find_free hands)))
         (fixed (find_fixed hands))
         (mentsu (find_trios (unbox rest)))
         (proc (lambda (x)
           (normalize (append fixed (list x) (list (remove_from rest x)))))))
    (mapcar proc mentsu)))

;; Free要素の有無を検査する
;; * (has_free '((Twins 1)(Free 5 7)(Free 6)(Twins 7 5)))
;; -> T
;; * (has_free '((Twins 1)(Twins 7 5)))
;; -> NIL
(defun has_free (list)
  (cond ((null list) nil)
        ((eq 'Free (caar list)) t)
        (t (has_free (cdr list)))))

;; 1雀頭+N面子を確定する
;; solve' :: [Hands] -> [Hands]
;; Free要素が無くなれば再帰呼び出しを終了する(count == 0)
;; * (solve* '(((Twins 1)(Free 14 14 15 15 16 16 18 18 18))))
;; -> (((TWINS 1) (TRIPLETS 18) (SERIES 14 14)))
(defun solve* (hands)
  (let* ((r (nub (concatmap #'proceed1 hands)))
         (count (length (remove-if-not #'has_free r))))
    (if (eql count 0) r (solve* r))))

;; 手牌から雀頭を括りだす
;; * (split '(1 1 2 2 2 3 3 3 4 4 4 26 27 28) 1)
;; -> ((TWINS 1) (FREE 2 2 2 3 3 3 4 4 4 26 27 28))
(defun split (body x)
  (list (Twins (list x)) (Free (subset body (list x x)))))

;; アガリが成立する組み合せの集合を返す
;; solve :: [Int] -> [Hands]
(defun solve (body)
  (let* ((hands (mapcar (lambda (x) (split body (car (unbox x)))) (pick_twins body)))
         (r1 (pick_7pairs body))  ;; 七対子判定
         (r2 (pick_kokushi body)) ;; 国士無双判定
         (r3 (solve* hands)))     ;; 1雀頭+N面子判定
    (remove-if-not (lambda (x) (not (null x))) (cons r1 (cons r2 r3)))))

;; 実行結果を出力するための共通サービス関数
;; * (p '((Twins 1 2 3 4 5 6 7)))
;; -> (TWINS 1 2 3 4 5 6 7)
(defun p (hands)
  (progn (mapcar (lambda (x) (princ (format nil "    ~A~%" x))) hands) t))

;; 麻雀牌(image)形式で出力する
;; * (pp '(((Twins 1 2 3 4 5 6 7))))
;; -> ((一 一)(二 二)(三 三)(四 四)(五 五)(六 六)(七 七))
(defun pp (list)
  (let* ((proc (lambda (x) (format nil "(~{~A~})" (mapcar #'show_hand x)))))
    (p (mapcar proc list))))

;; メイン関数
(defun main ()
  (let ((m1 '(1 9 11 19 21 29 31 33 35 37 41 43 45 45)) ;; 国士無双
        (m2 '(1 1 2 2 3 3 4 4 5 5 6 6 7 7))             ;; 七対子
        (m3 '(1 1 2 2 2 3 3 3 4 4 4 26 27 28))          ;; 三連刻
        (m4 '(1 1 2 2 2 3 3 3 4 4 4 5 5 5)))            ;; 四暗刻
    (progn (princ (format nil "~%~A~%" m1))
           (pp (solve m1))
           (princ (format nil "~%~A~%" m2))
           (pp (solve m2))
           (princ (format nil "~%~A~%" m3))
           (pp (solve m3))
           (princ (format nil "~%~A~%" m4))
           (pp (solve m4)))))

;; テスト
;; # 国士無双
;; * (p (solve '(1 9 11 19 21 29 31 33 35 37 41 43 45 45)))
;; ->  ((KOKUSHI 45))
;; * (pp (solve '(1 9 11 19 21 29 31 33 35 37 41 43 45 45)))
;; ->  ((一 九 ① ⑨ Ⅰ Ⅸ 東 南 西 北 白 發 中 中))
;; # 七対子
;; * (p (solve '(1 1 2 2 3 3 4 4 5 5 6 6 7 7)))
;; ->  ((TWINS 1 2 3 4 5 6 7))
;;     ((TWINS 1) (SERIES 2 2 5 5))
;;     ((TWINS 4) (SERIES 1 1 5 5))
;;     ((TWINS 7) (SERIES 1 1 4 4))
;; * (pp (solve '(1 1 2 2 3 3 4 4 5 5 6 6 7 7)))
;; ->  ((一 一)(二 二)(三 三)(四 四)(五 五)(六 六)(七 七))
;;     ((一 一)(二 三 四)(二 三 四)(五 六 七)(五 六 七))
;;     ((四 四)(一 二 三)(一 二 三)(五 六 七)(五 六 七))
;;     ((七 七)(一 二 三)(一 二 三)(四 五 六)(四 五 六))
;; # 三連刻
;; * (p (solve '(1 1 2 2 2 3 3 3 4 4 4 26 27 28)))
;; ->  ((TWINS 1) (TRIPLETS 2 3 4) (SERIES 26))
;;     ((TWINS 1) (SERIES 2 2 2 26))
;;     ((TWINS 4) (SERIES 1 1 2 26))
;; * (pp (solve '(1 1 2 2 2 3 3 3 4 4 4 26 27 28)))
;; ->  ((一 一)(二 二 二)(三 三 三)(四 四 四)(Ⅵ Ⅶ Ⅷ))
;;     ((一 一)(二 三 四)(二 三 四)(二 三 四)(Ⅵ Ⅶ Ⅷ))
;;     ((四 四)(一 二 三)(一 二 三)(二 三 四)(Ⅵ Ⅶ Ⅷ))
;; # 四暗刻
;; * (p (solve '(1 1 2 2 2 3 3 3 4 4 4 5 5 5)))
;; ->  ((TWINS 1) (TRIPLETS 2 3 4 5))
;;     ((TWINS 1) (TRIPLETS 5) (SERIES 2 2 2))
;;     ((TWINS 1) (TRIPLETS 2) (SERIES 3 3 3))
;;     ((TWINS 4) (TRIPLETS 5) (SERIES 1 1 2))
;; * (pp (solve '(1 1 2 2 2 3 3 3 4 4 4 5 5 5)))
;; ->  ((一 一)(二 二 二)(三 三 三)(四 四 四)(五 五 五))
;;     ((一 一)(五 五 五)(二 三 四)(二 三 四)(二 三 四))
;;     ((一 一)(二 二 二)(三 四 五)(三 四 五)(三 四 五))
;;     ((四 四)(五 五 五)(一 二 三)(一 二 三)(二 三 四))

