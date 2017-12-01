
;; 機能: 麻雀の役判定と点数計算
;; 作成: 2017-11-18  ken3@nurs.or.jp
;; 更新: 2017-11-21  ken3@nurs.or.jp
;; Scheme(Gauche)版

(use srfi-1)  ;; リストライブラリ
(use srfi-13) ;; 文字列ライブラリ

;; HaskellのconcatMap
(define (concatmap fn x) (concatenate (map fn x)))

;; 2つのリストに共通の要素を抽出する
(define (intersection set1 set2)
    (if (null? set1) '()
      (let* ((h1 (car set1))
             (t1 (cdr set1))
             (c  (lset-intersection = t1 set2)))
        (if (member h1 set2) (cons h1 c) c))))

;; Hands要素を追加する
;; 追加しようとする要素が既に存在する場合には追加しない。
;; * (add-if-uniq '((:Twins 1) (:Free 9)) '(((:Twins 1) (:Free 5)) ((:Twins 1) (:Free 7))))
;; -> (((:Twins 1) (:Free 9)) ((:Twins 1) (:Free 5)) ((:Twins 1) (:Free 7)))
;; * (add-if-uniq '((:Twins 1) (:Free 7)) '(((:Twins 1) (:Free 5)) ((:Twins 1) (:Free 7))))
;; -> (((:Twins 1) (:Free 5)) ((:Twins 1) (:Free 7)))
(define (add-if-uniq acc hand) (lset-adjoin equal? acc hand))

;; リストから指定した要素を削除する
;; 重複要素がある場合には1つだけ削除し、残りのものは削除しない。
;; * (remove-just1 3 '(1 1 2 2 3 3 4 4 5 5 6 6 7 7))
;; -> (1 1 2 2 3 4 4 5 5 6 6 7 7)
(define (remove-just1* acc x y)
  (if (null? y) acc
    (let ((y1 (car y))
          (y2 (cdr y)))
      (if (= y1 x) (remove-just1* acc 0 y2)
                      (remove-just1* (cons y1 acc) x y2)))))
(define (remove-just1 x y)
    (reverse (remove-just1* '() x y)))

;; 表示用データ
(define image
  #(
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
(define wanzu  '(1 2 3 4 5 6 7 8 9))
(define pinzu  '(11 12 13 14 15 16 17 18 19))
(define souzu  '(21 22 23 24 25 26 27 28 29))
(define routou '(1 9 11 19 21 29))
(define kaze   '(31 33 35 37))
(define dragon '(41 43 45))
(define yaochu '(1 9 11 19 21 29 31 33 35 37 41 43 45))
(define greens '(22 23 24 26 28 43))
(define range  (iota 45 0))

;; 手牌の組み合わせ
;; data Mentsu = Twins    -- 対子 [1,1]             => (:Twins 1)
;;             | Triplets -- 刻子 [1,1,1]           => (:Triplets 1)
;;             | Series   -- 順子 [1,2,3]           => (:Series 1)
;;             | Free     -- 残り [1,3,5]           => (:Free 1 3 5)
;;             | Kokushi  -- 国士 [1,1,9,...,43,45] => (:Kokushi 1)
;; type Hand  = (Mentsu,[Int])
;; type Hands = [Hand]

;; Handsコンストラクタを定義するテンプレートマクロ
(define-macro (make-constructor name)
  (let ((label (make-keyword (symbol->string name))))
    `(define (,name . args)
       (let* ((arg1 (car args))
              (h (if (list? arg1) arg1 args)))
         (cond ((list? arg1) (cons ,label h))
               ((number? arg1) (cons ,label h))
               (else '()))))))

;; Handコンストラクタ
;; (Twins 1 2 3)     => (:Twins 1 2 3)
;; (Twins '(1 2 3))  => (:Twins 1 2 3)
(make-constructor Twins)    ;; Twins    :: [Int] -> Hand
(make-constructor Triplets) ;; Triplets :: [Int] -> Hand
(make-constructor Series)   ;; Series   :: [Int] -> Hand
(make-constructor Free)     ;; Free     :: [Int] -> Hand
(make-constructor Kokushi)  ;; Kokushi  :: [Int] -> Hand

;; Handを[Int]に変換する
;; unbox :: Hand -> [Int]
;; * (unbox '(:Series 2))
;; -> (2 3 4)
;; * (unbox '(:Series 2 12 22))
;; -> (2 3 4 12 13 14 22 23 24)
(define (make-series v) (list v (+ 1 v) (+ 2 v)))
(define (unbox* k h)
  (if (list? h) (case k
      ((:Twins)    (concatmap (lambda (x) (list x x)) h))
      ((:Triplets) (concatmap (lambda (x) (list x x x)) h))
      ((:Series)   (concatmap make-series h))
      ((:Free)     h)
      ((:Kokushi)  (sort (cons (car h) yaochu)))
      (else '())) '()))
(define (unbox hand)
  (let* ((r (sort (unbox* (car hand) (cdr hand))))
         (h (car r)))
    (cond ((list? h) h)
          ((number? h) r)
          (else '()))))

;; [Int]の差集合を返す(標準関数のdeleteと違い、重複要素をすべて削除することはしない)
;; subset :: [Int] -> [Int] -> [Int]
;; * (subset '(1 1 2 2 3 3 4 4 5 5 6 6 7 7) '(1 2 3))
;; -> (1 2 3 4 4 5 5 6 6 7 7)
(define (subset a b) (fold-right remove-just1 a b))

;; FreeにHandを加算する
;; add :: Hand -> Hand -> Hand
;; * (add '(Free 1 2 3) '(Free 2 3 4))
;; -> (:Free 1 2 2 3 3 4)
(define (add x y) (Free (append (unbox x) (unbox y))))

;; FreeからHandを減算する
;; sub :: Hand -> Hand -> Hand
;; * (sub '(Free 1 2 3 4 5 6) '(Free 2 3 4))
;; -> (:Free 1 5 6)
(define (sub x y) (Free (subset (unbox x) (unbox y))))

;; [Int]を文字列化する
;; to-string :: [Int] -> String
;; * (to-string '(11 12 13))
;; -> "(① ② ③)"
(define (to-string x) (format #f "~A" (map (lambda (i) (vector-ref image i)) x)))

;; Handを文字列化する
;; show-hand :: Hand -> String
;; (show-hand '(:Twins (2 7 11 18 25 35 43)))
;; -> "(二 二)(七 七)(① ①)(⑧ ⑧)(Ⅴ Ⅴ)(西 西)(發 發)"
;; (show-hand '(:Triplets 35))
;; -> "(西 西 西)"
;; (show-hand '(:Series 11))
;; -> "(① ② ③)"
;; (show-hand '(:Free 7 11 15 21 33))
;; -> "(七 ① ⑤ Ⅰ 南)"
;; (show-hand '(:Kokushi 11))
;; -> "(一 九 ① ① ⑨ Ⅰ Ⅸ 東 南 西 北 白 發 中)"
(define (show-hand* fn hand)
  (let ((c (cadr hand)))
    (cond ((number? c) (string-concatenate (map fn (cdr hand))))
          ((list? c) (string-concatenate (map fn c)))
          (else '()))))
(define (show-hand hand)
  (let ((type (car hand))
        (c (cadr hand)))
    (case type
      ((:Twins)    (show-hand* (lambda (x) (to-string (list x x))) hand))
      ((:Triplets) (show-hand* (lambda (x) (to-string (list x x x))) hand))
      ((:Series)   (show-hand* (lambda (x) (to-string (make-series x))) hand))
      ((:Free)     (cond ((number? c) (to-string (cdr hand)))
                         ((list? c) (to-string c))
                         (else '())))
      ((:Kokushi)  (let ((h (if (list? c) (car c) c)))
                     (to-string (sort (cons h yaochu)))))
      (else '()))))

;; ヒストグラムを返す
;; * (histogram '(1 9 11 19 21 29 31 33 35 37 41 43 45 45))
;; -> (0 1 0 0 0 0 0 0 0 1 0 1 ... 1 0 2)
;; histogram :: [Int] -> [Int]
(define (histogram a)
  (map (lambda (n) (length (filter (lambda (x) (= n x)) a))) range))

;; 国士無双判定
;; pick-kokushi :: [Int] -> Hands
;; * (pick-kokushi '(1 9 11 19 21 29 31 33 35 37 41 43 45 45))
;; -> ((:Kokushi 45))
(define (pick-kokushi body)
  (let* ((p (intersection body yaochu))
         (u (delete-duplicates p))
         (d (subset p u)))
    (if (and (= (length p) 14) (= (length u) 13)) (list (Kokushi d)) '())))

;; 七対子判定
;; pick-7pairs :: [Int] -> Hands
;; * (pick-7pairs '(1 1 2 2 3 3 4 4 5 5 6 6 7 7))
;; -> (:Twins 1 2 3 4 5 6 7)
(define (pick-7pairs body)
  (let* ((h (zip range (histogram body)))
         (p (map car (filter (lambda (x) (= (cadr x) 2)) h))))
    (if (= (length p) 7) (list (Twins p)) '())))

;; 雀頭候補を返す
;; pick-twins :: [Int] -> Hands
;; * (pick-twins '(1 1 2 2 3 3 4 4 5 5 5 6 6 6))
;; -> ((:Twins 1) (:Twins 2) (:Twins 3) (:Twins 4) (:Twins 5) (:Twins 6))
(define (pick-twins body)
  (let* ((h (zip range (histogram body)))
         (p (map car (filter (lambda (x) (>= (cadr x) 2)) h))))
    (map Twins p)))

;; 刻子候補を返す
;; pick-triplets :: [Int] -> Hands
;; * (pick-triplets '(1 1 2 2 3 3 4 4 5 5 5 6 6 6))
;; -> ((:Triplets 5) (:Triplets 6))
(define (pick-triplets body)
  (let* ((h (zip range (histogram body)))
         (p (map car (filter (lambda (x) (>= (cadr x) 3)) h))))
    (map Triplets p)))

;; 順子候補を返す
;; pick-series :: [Int] -> Hands
;; * (pick-series '(1 1 2 2 3 3 4 4 5 5 5 6 6 6))
;; -> ((:Series 1) (:Series 2) (:Series 3) (:Series 4))
(define (product3 h0)
  (let* ((h1 (cdr h0))
         (h2 (cdr h1))
         (p (zip h0 h1 h2)))
    (map (lambda (x) (* (car x) (cadr x) (caddr x))) p)))
(define (pick-series body)
  (let* ((h (histogram body))
         (z (zip range (product3 h)))
         (p (map car (filter (lambda (x) (not (= 0 (cadr x)))) z))))
    (map Series p)))

;; 未確定牌(Free要素)の中にある刻子と順子を返す
;; find-trios :: [Int] -> Hands
;; * (find-trios '(5 5 5 6 6 6 7 7 7))
;; -> ((:Triplets 5) (:Triplets 6) (:Triplets 7) (:Series 5))
(define (find-trios a) (append (pick-triplets a) (pick-series a)))

;; 手牌の中からTwins要素を探し、マージした結果を返す
;; * (find-twins '((:Twins 11 15)(:Twins 21)(:Free 5 7 9)))
;; -> (:Twins 11 15 21)
;; find-twins* :: [Int] -> Hands -> [Int]
(define (find-twins* list hands)
  (cond ((null? hands) list)
        ((eq? :Twins (caar hands))
          (find-twins* (append list (cdar hands)) (cdr hands)))
        (else (find-twins* list (cdr hands)))))
;; find-twins :: Hands -> Hands
(define (find-twins hands)
  (let* ((r (find-twins* '() hands)))
    (if (null? r) '() (Twins (sort r)))))

;; 手牌の中からTriplets要素を探し、マージした結果を返す
;; * (find-triplets '((:Twins 1)(:Triplets 2)(:Triplets 4)(:Free 5 7 9)))
;; -> (:Triplets 2 4)
;; find-triplets* :: [Int] -> Hands -> [Int]
(define (find-triplets* list hands)
  (cond ((null? hands) list)
        ((eq? :Triplets (caar hands))
          (find-triplets* (append list (cdar hands)) (cdr hands)))
        (else (find-triplets* list (cdr hands)))))
;; find-triplets :: Hands -> Hands
(define (find-triplets hands)
  (let ((r (find-triplets* '() hands)))
    (if (null? r) '() (Triplets (sort r)))))

;; 手牌の中からSeries要素を探し、マージした結果を返す
;; * (find-series '((:Twins 1)(:Triplets 11)(:Series 21)(:Series 2)))
;; -> (:Series 2 21)
;; find-series* :: [Int] -> Hands -> [Int]
(define (find-series* list hands)
  (cond ((null? hands) list)
        ((eq? :Series (caar hands))
          (find-series* (append list (cdar hands)) (cdr hands)))
        (else (find-series* list (cdr hands)))))
;; find-series :: Hands -> Hands
(define (find-series hands)
  (let ((r (find-series* '() hands)))
    (if (null? r) '() (Series (sort r)))))

;; 手牌の中からKokushi要素を探して返す
;; 手牌の中のKokushi要素は高々1つしか無いはず。
;; * (find-kokushi '((:Kokushi 1)))
;; -> (:Kokushi 1)
;; find-kokushi* :: Hands -> [Int]
(define (find-kokushi* hands)
  (cond ((null? hands) '())
        ((eq? :Kokushi (caar hands)) (cdar hands))
        (else (find-kokushi* (cdr hands)))))
;; find-kokushi :: Hands -> Hands
(define (find-kokushi hands)
  (let ((r (find-kokushi* hands)))
    (if (null? r) '() (Kokushi (sort r)))))

;; 手牌の中から確定牌(Free要素以外)を探して返す
;; find-fixed :: Hands -> Hands
;; * (find-fixed '((:Twins 1)(:Series 2)(:Free 4 7 9)))
;; -> ((:Twins 1) (:Series 2))
(define (find-fixed x)
  (let* ((r1 (find-kokushi x))
         (r2 (find-twins x))
         (r3 (find-triplets x))
         (r4 (find-series x)))
    (remove null? (list r1 r2 r3 r4))))

;; 手牌の中から未確定牌(Free要素)を探し、マージした結果を返す
;; find-free :: Hands -> Hands
;; * (find-free '((:Twins 1)(:Series 2)(:Free 5 5 6 6 7 7)))
;; -> ((:Free 5 5 6 6 7 7))
;; * (find-free '((:Free 1)(:Free 2)(:Free (5 5 6 6 7 7))))
;; -> ((:Free 1 2 5 5 6 6 7 7))
;; * (find-free '((:Twins 1)(:Series 2)))
;; -> ()
(define (find-free* list hands)
  (cond ((null? hands) list)
        ((eq? :Free (caar hands))
          (find-free* (append list (unbox (car hands))) (cdr hands)))
        (else (find-free* list (cdr hands)))))
(define (find-free hands)
  (let ((r (find-free* '() hands)))
    (if (null? r) '() (list (Free (sort r))))))

;; 手牌の並びを正規化する
;; normalize :: Hands -> Hands
;; * (normalize (pick-twins '(1 1 2 2 3 3 4 4 5 5 5 6 6 6)))
;; -> ((:Twins 1 2 3 4 5 6))
;; * (normalize '((:Twins 1)(:Free 5 7)(:Free 6)(:Twins 7 5)))
;; -> ((:Twins 1 5 7) (:Free 5 6 7))
(define (normalize hands)
  (let ((x (remove null? hands)))
    (append (find-fixed x) (find-free x))))

;; リストから重複要素を削除する
;; * (nub '(((:Twins 1))((:Series 3))((:Twins 1))((:Series 3))))
;; -> (((:Series 3)) ((:Twins 1)))
(define (nub hands)
  (let ((n (map normalize hands)))
    (fold-left add-if-uniq '() n)))

;; Free要素からmentsu要素を削除する
(define (remove-from hand mentsu)
  (let ((rest (subset (unbox hand) (unbox mentsu))))
    (cond ((null? rest) '())
           (else (Free (sort rest))))))

;; Free要素のメンツを1つ仮確定し、組み合わせを更新する
;; proceed1 :: [Hand] -> [Hands]
;; * (p (proceed1 '((:Twins 1)(:Series 2)(:Free 5 5 5 6 6 6 7 7 7))))
;; -> ((Twins 1) (Triplets 5) (Series 2) (Free 6 6 6 7 7 7))
;;    ((Twins 1) (Triplets 6) (Series 2) (Free 5 5 5 7 7 7))
;;    ((Twins 1) (Triplets 7) (Series 2) (Free 5 5 5 6 6 6))
;;    ((Twins 1) (Series 2 5) (Free 5 5 6 6 7 7))
;; * (proceed1 '((:Twins 1)(:Series 2)(:Free 5 6 7)))
;; -> (((:Twins 1) (:Series 2 5)))
(define (proceed1 hands)
  (let* ((rest (car (find-free hands)))
         (fixed (find-fixed hands))
         (mentsu (find-trios (unbox rest)))
         (proceed1* (lambda (x)
           (normalize (cons x (cons (remove-from rest x) fixed))))))
    (map proceed1* mentsu)))

;; Free要素の有無を検査する
;; * (has-free '((:Twins 1)(:Free 5 7)(:Free 6)(:Twins 7 5)))
;; -> #t
;; * (has-free '((:Twins 1)(:Twins 7 5)))
;; -> #f
(define (has-free list)
  (cond ((null? list) #f)
        ((eq? :Free (caar list)) #t)
        (else (has-free (cdr list)))))

;; 1雀頭+N面子を確定する
;; solve* :: [Hands] -> [Hands]
;; Free要素が無くなれば再帰呼び出しを終了する(count == 0)
;; * (solve* '(((:Twins 1)(:Free 14 14 15 15 16 16 18 18 18))))
;; -> (((:Twins 1) (:Triplets 18) (:Series 14 14)))
(define (solve* hands)
  (let* ((r (nub (concatmap proceed1 hands)))
         (c (length (filter has-free r))))
    (if (= c 0) r (solve* r))))

;; 手牌から雀頭を括りだす
;; * (split '(1 1 2 2 2 3 3 3 4 4 4 26 27 28) 1)
;; -> ((:Twins 1) (:Free 2 2 2 3 3 3 4 4 4 26 27 28))
(define (split body x) (list (Twins (list x)) (Free (subset body (list x x)))))

;; アガリが成立する組み合せの集合を返す
;; solve :: [Int] -> [Hands]
(define (solve body)
  (let* ((hands (map (lambda (x) (split body (car (unbox x)))) (pick-twins body)))
         (r1 (pick-7pairs body))  ;; 七対子判定
         (r2 (pick-kokushi body)) ;; 国士無双判定
         (r3 (solve* hands)))     ;; 1雀頭+N面子判定
    (filter (lambda (x) (not (null? x))) (cons r1 (cons r2 r3)))))

;; 実行結果を出力するための共通サービス関数
;; * (p '((Twins 1 2 3 4 5 6 7)))
;; -> (Twins 1 2 3 4 5 6 7)
(define (p hands) (map (lambda (x) (print (format #f "    ~A" x))) hands) #t)

;; 麻雀牌(image)形式で出力する
;; * (pp '(((:Twins 1 2 3 4 5 6 7))))
;; -> ((一 一)(二 二)(三 三)(四 四)(五 五)(六 六)(七 七))
(define (pp list)
  (let ((lines (map (lambda (x) (string-concatenate (map show-hand x))) list)))
    (map (lambda (x) (print (format #f "    ~A" x))) lines) #t))

;; メイン関数
(define (main)
  (let ((m1 '(1 9 11 19 21 29 31 33 35 37 41 43 45 45)) ;; 国士無双
        (m2 '(1 1 2 2 3 3 4 4 5 5 6 6 7 7))             ;; 七対子
        (m3 '(1 1 2 2 2 3 3 3 4 4 4 26 27 28))          ;; 三連刻
        (m4 '(1 1 2 2 2 3 3 3 4 4 4 5 5 5)))            ;; 四暗刻
    (print (format #f "~%~A" m1))
    (pp (solve m1))
    (print (format #f "~%~A" m2))
    (pp (solve m2))
    (print (format #f "~%~A" m3))
    (pp (solve m3))
    (print (format #f "~%~A" m4))
    (pp (solve m4))
    #t))

;; テスト
;; # 国士無双
;; * (p (solve '(1 9 11 19 21 29 31 33 35 37 41 43 45 45)))
;; ->  ((Kokushi 45))
;; * (pp (solve '(1 9 11 19 21 29 31 33 35 37 41 43 45 45)))
;; ->  ((一 九 ① ⑨ Ⅰ Ⅸ 東 南 西 北 白 發 中 中))
;; # 七対子
;; * (p (solve '(1 1 2 2 3 3 4 4 5 5 6 6 7 7)))
;; ->  ((Twins 1 2 3 4 5 6 7))
;;     ((Twins 1) (Series 2 2 5 5))
;;     ((Twins 4) (Series 1 1 5 5))
;;     ((Twins 7) (Series 1 1 4 4))
;; * (pp (solve '(1 1 2 2 3 3 4 4 5 5 6 6 7 7)))
;; ->  ((一 一)(二 二)(三 三)(四 四)(五 五)(六 六)(七 七))
;;     ((一 一)(二 三 四)(二 三 四)(五 六 七)(五 六 七))
;;     ((四 四)(一 二 三)(一 二 三)(五 六 七)(五 六 七))
;;     ((七 七)(一 二 三)(一 二 三)(四 五 六)(四 五 六))
;; # 三連刻
;; * (p (solve '(1 1 2 2 2 3 3 3 4 4 4 26 27 28)))
;; ->  ((Twins 1) (Triplets 2 3 4) (Series 26))
;;     ((Twins 1) (Series 2 2 2 26))
;;     ((Twins 4) (Series 1 1 2 26))
;; * (pp (solve '(1 1 2 2 2 3 3 3 4 4 4 26 27 28)))
;; ->  ((一 一)(二 二 二)(三 三 三)(四 四 四)(Ⅵ Ⅶ Ⅷ))
;;     ((一 一)(二 三 四)(二 三 四)(二 三 四)(Ⅵ Ⅶ Ⅷ))
;;     ((四 四)(一 二 三)(一 二 三)(二 三 四)(Ⅵ Ⅶ Ⅷ))
;; # 四暗刻
;; * (p (solve '(1 1 2 2 2 3 3 3 4 4 4 5 5 5)))
;; ->  ((Twins 1) (Triplets 2 3 4 5))
;;     ((Twins 1) (Triplets 5) (Series 2 2 2))
;;     ((Twins 1) (Triplets 2) (Series 3 3 3))
;;     ((Twins 4) (Triplets 5) (Series 1 1 2))
;; * (pp (solve '(1 1 2 2 2 3 3 3 4 4 4 5 5 5)))
;; ->  ((一 一)(二 二 二)(三 三 三)(四 四 四)(五 五 五))
;;     ((一 一)(五 五 五)(二 三 四)(二 三 四)(二 三 四))
;;     ((一 一)(二 二 二)(三 四 五)(三 四 五)(三 四 五))
;;     ((四 四)(五 五 五)(一 二 三)(一 二 三)(二 三 四))

