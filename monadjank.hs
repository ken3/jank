#!/usr/bin/env runhaskell

-- 機能: 麻雀の役判定と点数計算
-- 作成: 2017-10-19  ken3@nurs.or.jp
-- 更新: 2017-10-30  ken3@nurs.or.jp

import Data.List

-- 表示用データ
image :: [String]
image = [
    -- 端末が漢字表示に対応している場合はこちらの image を使用する。
    "00", "一", "二", "三", "四", "五", "六", "七", "八", "九",  -- 萬子
    "10", "①", "②", "③", "④", "⑤", "⑥", "⑦", "⑧", "⑨",  -- 筒子
    "20", "Ⅰ", "Ⅱ", "Ⅲ", "Ⅳ", "Ⅴ", "Ⅵ", "Ⅶ", "Ⅷ", "Ⅸ",  -- 索子
    "30", "東", "32", "南", "34", "西", "36", "北", "38", "39",  -- 風牌
    "40", "白", "42", "發", "44", "中"                         -- 三元牌
    {-
    -- 端末が漢字表示に対応していない場合はこちらの image を使用する。
    "00", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9",  -- 萬子
    "10", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9",  -- 筒子
    "20", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9",  -- 索子
    "30", "We", "32", "Ws", "34", "Ww", "36", "Wn", "38", "39",  -- 風牌
    "40", "Dw", "42", "Dg", "44", "Dr"                         -- 三元牌
    -}
    ]

-- 日本語文字列表示
show_as_utf8 :: Show a => a -> String
show_as_utf8 x = cons (show x)
  where cons :: String -> String
        cons [] = []
        cons m@('\"':xs) = s ++ (cons r)
             where (s,r):_   = reads m
        cons m@('\'':xs) = '\'':s:'\'':(cons r)
             where (s,r):_ = reads m
        cons m@(x:xs)    = x:cons xs

-- 牌種データ
wanzu  = [1..9]
pinzu  = [11..19]
souzu  = [21..29]
routou = [1,9,11,19,21,29]
kaze   = [31,33,35,37]
dragon = [41,43,45]
yaochu = [1,9,11,19,21,29,31,33,35,37,41,43,45]
greens = [22,23,24,26,28,43]
-- range  = [0..45]
range  = map fst $ zip [0..] image

-- 手牌の組み合わせ
data Mentsu = Twins    -- 対子 [1,1]             => (Twins,[1,1])
            | Triplets -- 刻子 [1,1,1]           => (Triplets,[1,1,1])
            | Series   -- 順子 [1,2,3]           => (Series,[1,2,3])
            | Rest     -- 残り [1,3,5]           => (Rest,[1,3,5])
            | Kokushi  -- 国士 [1,1,9,...,43,45] => (Kokushi,[1,1,9,11,19,21,29,31,33,35,37,41,43,45])
              deriving (Show, Enum, Eq, Ord)
type Hand  = (Mentsu,[Int])
type Hands = [Hand]

-- Handsコンストラクタ
cTwins    :: [Int] -> Hand -- cTwins [1]    => (Twins,[1,1])
cTriplets :: [Int] -> Hand -- cTriplets [1] => (Triplets,[1,1,1])
cSeries   :: [Int] -> Hand -- cSeries [1]   => (Series,[1,2,3])
cRest     :: [Int] -> Hand -- cRest [1,3,5] => (Rest,[1,3,5])
cKokushi  :: [Int] -> Hand -- cKokushi [1]  => (Kokushi,[1,1,9,11,19,21,29,31,33,35,37,41,43,45])
cTwins    (x:_) = (Twins,[x,x])
cTriplets (x:_) = (Triplets,[x,x,x])
cSeries   (x:_) = (Series,[x,x+1,x+2])
cRest     x     = (Rest,sort x)
cKokushi  x     = (Kokushi,sort $ yaochu ++ x)

-- Handを[Int]に変換する
unbox :: Hand -> [Int]
unbox (_,x) = x

-- [Int]の差集合を返す(重複要素は残す)
-- *Main> subset [1,1,2,2,3,3,4,4,5,5,6,6,7,7] [1,2,3]
-- [1,2,3,4,4,5,5,6,6,7,7]
subset :: [Int] -> [Int] -> [Int]
subset a b = foldr delete a b

-- RestにHandを加算する
add :: Hand -> Hand -> Hand
add x y = (Rest, (unbox x) ++ (unbox y))

-- RestからHandを減算する
sub :: Hand -> Hand -> Hand
sub x y = (Rest, subset (unbox x) (unbox y))

-- [Int]を文字列化する
to_string :: [Int] -> String
to_string x = show_as_utf8 $ map ((!!) image) x

-- Handを文字列化する
-- *Main > show_hand $ (Triplets,[11,11,11])
-- "[P1,P1,P1]"
show_hand :: Hand -> String
show_hand (_,x) = to_string $ sort x

-- Handsを文字列化する
-- *Main > show_hands [(Kokushi,[1,9,11,19,19,21,29,31,33,35,37,41,43,45])]
-- "[[M1,M9,P1,P9,P9,S1,S9,We,Ws,Ww,Wn,Dw,Dg,Dr]]"
show_hands :: Hands -> String
show_hands hs = "[" ++ (concatMap show_hand hs) ++ "]"

-- [Hands]を文字列化する
-- *Main > show_hands_array [[(Triplets,[22,22,22])],[(Twins,[19,19])]]
-- ["[[S2,S2,S2]]","[[P9,P9]]"]
show_hands_array :: [Hands] -> [String]
show_hands_array a = map show_hands a

-- ヒストグラムを返す
-- *Main> histogram [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
-- [0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,...,0,1,0,1,0,2]
histogram :: [Int] -> [Int]
histogram a = map (\n -> length $ filter (== n) a) range

-- Hand抽出関数
-- cons : Handコンストラクタ
-- cond : タプルの抽出条件
-- hs   : ヒストグラム
pick :: ([Int] -> Hand) -> ((Int,Int) -> Bool) -> [Int] -> Hands
pick cons cond hs = map (cons . (:[]) . fst) $ filter cond $ zip [0..] hs

-- 国士無双判定
-- *Main> pick_kokushi [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
-- [(Kokushi,[1,9,11,19,21,29,31,33,35,37,41,43,45,45])]
pick_kokushi :: [Int] -> Hands
pick_kokushi body | (length p) == 14 && (length t) == 1 = t
                  | otherwise = []
  where t = pick cKokushi ((== 2) . snd) (histogram body)
        p = intersect body yaochu

-- 七対子判定
-- *Main> pick_7pairs [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
-- [(Twins,[1,1]),(Twins,[2,2]),(Twins,[3,3]),(Twins,[4,4]),(Twins,[5,5]),(Twins,[6,6]),(Twins,[7,7])]
pick_7pairs :: [Int] -> Hands
pick_7pairs body | (length t) == 7 = sorthands $ t
                 | otherwise = []
  where t = pick cTwins ((== 2) . snd) (histogram body)

-- 雀頭候補を返す
-- *Main> pick_twins [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [(Twins,[1,1]),(Twins,[2,2]),(Twins,[3,3]),(Twins,[4,4]),(Twins,[5,5]),(Twins,[6,6])]
pick_twins :: [Int] -> Hands
pick_twins body = pick cTwins ((>= 2) . snd) (histogram body)

-- 刻子候補を返す
-- *Main> pick_triplets [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [(Triplets,[5,5,5]),(Triplets,[6,6,6])]
pick_triplets :: [Int] -> Hands
pick_triplets body = pick cTriplets ((>= 3) . snd) (histogram body)

-- 順子候補を返す
-- *Main> pick_series [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [(Series,[1,2,3]),(Series,[2,3,4]),(Series,[3,4,5]),(Series,[4,5,6])]
pick_series :: [Int] -> Hands
pick_series body = pick cSeries ((/= 0) . snd) (product3 $ histogram body)
  where product3 h0@(_:h1@(_:h2)) = zipWith3 (\x y z -> x*y*z) h0 h1 h2

-- アガリが成立する組み合せの集合を返す
-- *Main> p $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,19]
--     [(Kokushi,[1,9,11,19,19,21,29,31,33,35,37,41,43,45])]
-- *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
--     [(Twins,[1,1]),(Twins,[2,2]),(Twins,[3,3]),(Twins,[4,4]),(Twins,[5,5]),(Twins,[6,6]),(Twins,[7,7])]
--     [(Twins,[1,1]),(Series,[2,3,4]),(Series,[2,3,4]),(Series,[5,6,7]),(Series,[5,6,7])]
--     [(Twins,[4,4]),(Series,[1,2,3]),(Series,[1,2,3]),(Series,[5,6,7]),(Series,[5,6,7])]
--     [(Twins,[7,7]),(Series,[1,2,3]),(Series,[1,2,3]),(Series,[4,5,6]),(Series,[4,5,6])]
-- *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
--     [(Twins,[1,1]),(Triplets,[5,5,5]),(Triplets,[6,6,6]),(Series,[2,3,4]),(Series,[2,3,4])]
--     [(Twins,[4,4]),(Triplets,[5,5,5]),(Triplets,[6,6,6]),(Series,[1,2,3]),(Series,[1,2,3])]
solve :: [Int] -> [Hands]
solve body = filter (not . null) $ r1:r2:r3
  -- 手牌から雀頭を括りだす
  -- body  = [1,1,9,9,19,20,21,31,43,44]
  -- ts    = [[1,1],[9,9]]
  -- hands = [[(Twins,[1,1]),(Rest,[9,9,19,20,21,31,43,44])],
  --          [(Twins,[9,9]),(Rest,[1,1,19,20,21,31,43,44])]]
  where r1 = pick_7pairs  body  -- 七対子判定
        r2 = pick_kokushi body  -- 国士無双判定
        r3 = solve' hands       -- 1雀頭+N面子判定
        hands   = map (split . head . unbox) $ pick_twins body
        split x = (Twins,[x,x]):[(Rest,subset body [x,x])]

-- 1雀頭+N面子を確定する
-- Rest要素が無くなれば再帰呼び出しを終了する(count == 0)
-- *Main > solve' [[(Twins,[1,1]),(Rest,[14,14,15,15,16,16,18,18,18])]]
-- [[(Twins,[1,1]),(Triplets,[18,18,18]),(Series,[14,15,16]),(Series,[14,15,16])]]
solve' :: [Hands] -> [Hands]
solve' hands | count == 0 = r
             | otherwise  = solve' r
  where r = nub $ concatMap proceed1 hands -- nubを使って重複要素を削除する
        count = length $ filter has_rest r
        has_rest []           = False
        has_rest ((Rest,_):_) = True
        has_rest (_:xs)       = has_rest xs

-- Rest要素のメンツを1つ仮確定し、組み合わせを更新する
-- *Main > p $ proceed1 [(Twins,[1,1]),(Series,[2,3,4]),(Rest,[5,5,5,6,6,6,7,7,7])]
--     [(Twins,[1,1]),(Triplets,[5,5,5]),(Series,[2,3,4]),(Rest,[6,6,6,7,7,7])]
--     [(Twins,[1,1]),(Triplets,[6,6,6]),(Series,[2,3,4]),(Rest,[5,5,5,7,7,7])]
--     [(Twins,[1,1]),(Triplets,[7,7,7]),(Series,[2,3,4]),(Rest,[5,5,5,6,6,6])]
--     [(Twins,[1,1]),(Series,[2,3,4]),(Series,[5,6,7]),(Rest,[5,5,6,6,7,7])]
proceed1 :: [Hand] -> [Hands]
proceed1 hands = map apply mentsu
  where rest    = head $ find_rest hands
        fixed   = find_fixed hands
        mentsu  = find_trios $ unbox rest
        apply x = sorthands $ fixed ++ [x] ++ (remove_from rest x)
        remove_from hand mentsu | null rest'    = []
                                | otherwise = [(Rest,sort rest')]
          where rest' = subset (unbox hand) (unbox mentsu)

-- 手牌の並びを正規化する
-- *Main > sorthands $ pick_twins [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [(Twins,[1,1]),(Twins,[2,2]),(Twins,[3,3]),(Twins,[4,4]),(Twins,[5,5]),(Twins,[6,6])]
-- *Main > sorthands [(Twins,[1,1]),(Series,[2,3,4]),(Rest,[5,7]),(Rest,[6,6,6]),(Twins,[7,7]),(Twins,[5,5])]
-- [(Twins,[1,1]),(Twins,[5,5]),(Twins,[7,7]),(Series,[2,3,4]),(Rest,[5,6,6,6,7])]
sorthands :: Hands -> Hands
sorthands x = (find_fixed x) ++ (find_rest x)

-- 未確定牌(Rest要素)の中にある刻子と順子を返す
-- *Main > find_trios [5,5,5,6,6,6,7,7,7]
-- [(Triplets,[5,5,5]),(Triplets,[6,6,6]),(Triplets,[7,7,7]),(Series,[5,6,7])]
find_trios :: [Int] -> Hands
find_trios a = (pick_triplets a) ++ (pick_series a)

-- 手牌の中から確定牌(Rest要素以外)を探して返す
-- *Main> find_fixed [(Twins,[1,1]),(Series,[2,3,4]),(Rest,[5,5,5,6,6,6,7,7,7])]
-- [(Twins,[1,1]),(Series,[2,3,4])]
find_fixed :: Hands -> Hands
find_fixed x = r1 ++ r2 ++ r3 ++ r4
  where r1 = find_kokushi  x
        r2 = find_twins    x
        r3 = find_triplets x
        r4 = find_series   x

-- 手牌の中から未確定牌(Rest要素)を探し、マージした結果を返す
-- *Main> find_rest [(Twins,[1,1]),(Series,[2,3,4]),(Rest,[5,5,5,6,6,6,7,7,7])]
-- [(Rest,[5,5,5,6,6,6,7,7,7])]
find_rest :: Hands -> Hands
find_rest hands | null r    = []
                | otherwise = [(Rest,sort r)]
  where r = find_rest' [] hands
        find_rest' :: [Int] -> Hands -> [Int]
        find_rest' r []              = r
        find_rest' r (x@(Rest,_):xs) = find_rest' (r ++ (unbox x)) xs
        find_rest' r (_:xs)          = find_rest' r xs

-- 手牌の中からTwins要素を探し、マージした結果を返す
-- *Main> find_twins [(Twins,[1,1]),(Twins,[15,15]),(Twins,[21,21]),(Series,[2,3,4]),(Rest,[5,7,9])]
-- [(Twins,[1,1]),(Twins,[15,15]),(Twins,[21,21])]
find_twins :: Hands -> Hands
find_twins hands | null r    = []
                 | otherwise = sort r
  where r = find_twins' [] hands
        find_twins' :: Hands -> Hands -> Hands
        find_twins' r []               = r
        find_twins' r (x@(Twins,_):xs) = find_twins' (x:r) xs
        find_twins' r (_:xs)           = find_twins' r xs

-- 手牌の中からTriplets要素を探し、マージした結果を返す
-- *Main> find_triplets [(Twins,[1,1]),(Triplets,[11,11,11]),(Series,[2,3,4]),(Rest,[5,7,9])]
-- [(Triplets,[11,11,11])]
find_triplets :: Hands -> Hands
find_triplets hands | null r    = []
                    | otherwise = sort r
  where r = find_triplets' [] hands
        find_triplets' :: Hands -> Hands -> Hands
        find_triplets' r []                  = r
        find_triplets' r (x@(Triplets,_):xs) = find_triplets' (x:r) xs
        find_triplets' r (_:xs)              = find_triplets' r xs

-- 手牌の中からSeries要素を探し、マージした結果を返す
-- *Main> find_series [(Twins,[1,1]),(Triplets,[11,11,11]),(Series,[21,22,23]),(Series,[2,3,4])]
-- [(Series,[2,3,4]),(Series,[21,22,23])]
find_series :: Hands -> Hands
find_series hands | null r    = []
                  | otherwise = sort r
  where r = find_series' [] hands
        find_series' :: Hands -> Hands -> Hands
        find_series' r []                = r
        find_series' r (x@(Series,_):xs) = find_series' (x:r) xs
        find_series' r (_:xs)            = find_series' r xs

-- 手牌の中からKokushi要素を探して返す
-- 手牌の中のKokushi要素は高々1つしか無いはず。
-- *Main> find_kokushi [(Kokushi,[1,1,9,11,19,21,29,31,33,35,37,41,43,45])]
-- [(Kokushi,[1,1,9,11,19,21,29,31,33,35,37,41,43,45])]
find_kokushi :: Hands -> Hands
find_kokushi hands | null r    = []
                   | otherwise = r
  where r = find_kokushi' hands
        find_kokushi' :: Hands -> Hands
        find_kokushi' []                 = []
        find_kokushi' (x@(Kokushi,_):xs) = [x]
        find_kokushi' (_:xs)             = find_kokushi' xs

-- 実行結果を出力するための共通サービス関数
phands :: (Hands -> String) -> [Hands] -> IO ()
phands f hands = mapM_ (\x -> putStrLn $ "    " ++ (f x)) hands

-- ASCIIリスト形式で出力する
p :: [Hands] -> IO ()
p = phands show

-- 麻雀牌(image)形式で出力する
pp :: [Hands] -> IO ()
pp = phands (concatMap show_hand)

-- メイン関数
main :: IO ()
main = do putStrLn $ show m1
          pp $ solve m1
          putStrLn $ show m2
          pp $ solve m2
          putStrLn $ show m3
          pp $ solve m3
    where m1 = [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
          m2 = [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
          m3 = [1,1,2,2,2,3,3,3,4,4,4,26,27,28]

-- テスト
-- # 国士無双
-- *Main> p $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
--     [(Kokushi,[1,9,11,19,21,29,31,33,35,37,41,43,45,45])]
-- *Main> pp $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
--     [M1,M9,P1,P9,S1,S9,We,Ws,Ww,Wn,Dw,Dg,Dr,Dr]
-- # 七対子
-- *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
--     [(Twins,[1,1]),(Twins,[2,2]),(Twins,[3,3]),(Twins,[4,4]),(Twins,[5,5]),(Twins,[6,6]),(Twins,[7,7])]
--     [(Twins,[1,1]),(Series,[2,3,4]),(Series,[2,3,4]),(Series,[5,6,7]),(Series,[5,6,7])]
--     [(Twins,[4,4]),(Series,[1,2,3]),(Series,[1,2,3]),(Series,[5,6,7]),(Series,[5,6,7])]
--     [(Twins,[7,7]),(Series,[1,2,3]),(Series,[1,2,3]),(Series,[4,5,6]),(Series,[4,5,6])]
-- *Main> pp $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
--     [M1,M1][M2,M2][M3,M3][M4,M4][M5,M5][M6,M6][M7,M7]
--     [M1,M1][M2,M3,M4][M2,M3,M4][M5,M6,M7][M5,M6,M7]
--     [M4,M4][M1,M2,M3][M1,M2,M3][M5,M6,M7][M5,M6,M7]
--     [M7,M7][M1,M2,M3][M1,M2,M3][M4,M5,M6][M4,M5,M6]
-- # 三連刻
-- *Main> p $ solve [1,1,2,2,2,3,3,3,4,4,4,26,27,28]
--     [(Twins,[1,1]),(Triplets,[2,2,2]),(Triplets,[3,3,3]),(Triplets,[4,4,4]),(Series,[26,27,28])]
--     [(Twins,[1,1]),(Series,[2,3,4]),(Series,[2,3,4]),(Series,[2,3,4]),(Series,[26,27,28])]
--     [(Twins,[4,4]),(Series,[1,2,3]),(Series,[1,2,3]),(Series,[2,3,4]),(Series,[26,27,28])]
-- *Main> pp $ solve [1,1,2,2,2,3,3,3,4,4,4,26,27,28]
--     [M1,M1][M2,M2,M2][M3,M3,M3][M4,M4,M4][S6,S7,S8]
--     [M1,M1][M2,M3,M4][M2,M3,M4][M2,M3,M4][S6,S7,S8]
--     [M4,M4][M1,M2,M3][M1,M2,M3][M2,M3,M4][S6,S7,S8]

