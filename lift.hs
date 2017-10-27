#!/usr/bin/env runhaskell

-- モナドに対する関数適用

import Data.Maybe
import Data.Monoid
import Data.Foldable
import Control.Monad
import Control.Applicative

flatMap :: Data.Foldable.Foldable t => (a -> t b) -> [a] -> [b]
flatMap f = concatMap (Data.Foldable.toList . f)

-- (Just 5) の中の要素に3を加算して (Just 8) を得る演算
-- (3 +)演算をリフトした関数を(Just 5)に適用する。
r01  = liftM (3 +) (Just 5)             -- Monad lift演算(1引数)
r01' = liftA (3 +) (Just 5)             -- Applicative演算(1引数)
r02  = liftM2 (+) (Just 3) (Just 5)     -- Monad lift演算(2引数)
r02' = liftA2 (+) (Just 3) (Just 5)     -- Applicative演算(2引数)
r03  = fmap (3 +) (Just 5)              -- Functor fmap (1引数)
r04  = (3 +) <$> (Just 5)               -- Applicative <$>  (1引数)
r05  = (+) <$> Just 3 <*> Just 5        -- Applicative <$> <*> (2引数)
r05' = Just (3 +) <*> Just 5            -- 部分適用 (+) <$> Just 3 => Just (3 +)
r06  = flip (+) <$> Just 3 <*> Just 5   -- (+)の部分適用引数を入れ替える
r06' = Just (+ 3) <*> Just 5            -- 部分適用 flip (+) <$> Just 3 => Just (+ 3)
r07  = return (3 +) `ap` Just 5         -- ap : ($)をリフトした演算 (1引数)
r07' = pure (3 +) `ap` Just 5           -- ap : ($)をリフトした演算 (1引数)
r07''= Just (3 +) `ap` Just 5           -- ap : ($)をリフトした演算 (1引数)
r08  = Just (+) `ap` Just 3 `ap` Just 5 -- ap : ($)をリフトした演算 (2引数)

-- [1,2,3] の中の各要素に3を加算して [4,5,6] を得る演算
r11  = liftM (3 +) [1,2,3]              -- Monad lift演算(1引数)
r11' = liftA (3 +) [1,2,3]              -- Applicative演算(1引数)
r12  = liftM2 (+) [3] [1,2,3]           -- Monad lift演算(2引数)
r12' = liftA2 (+) [3] [1,2,3]           -- Applicative演算(2引数)
r13  = fmap (3 +) [1,2,3]               -- Functor fmap (1引数)
r13' = map (3 +) [1,2,3]                -- リストの fmap 演算は map と等価
r14  = (3 +) <$> [1,2,3]                -- Applicative <$>  (1引数)
r15  = (+) <$> [3] <*> [1,2,3]          -- Applicative <$> <*> (2引数)
r15' = [(3 +)] <*> [1,2,3]              -- 部分適用 (+) <$> [3] => [(3 +)]
r16  = flip (+) <$> [3] <*> [1,2,3]     -- (+)の部分適用引数を入れ替える
r16' = [(+ 3)] <*> [1,2,3]              -- 部分適用 flip (+) <$> [3] => [(+ 3)]
r17  = return (3 +) `ap` [1,2,3]        -- ap : ($)をリフトした演算 (1引数)
r17' = pure (3 +) `ap` [1,2,3]          -- ap : ($)をリフトした演算 (1引数)
r17''= [(3 +)] `ap` [1,2,3]             -- ap : ($)をリフトした演算 (1引数)
r18  = [(+)] `ap` [3] `ap` [1,2,3]      -- ap : ($)をリフトした演算 (2引数)

-- 2つのリストに対して関数を適用すると、総当たりの結果を返す
r20  = liftM2 (+) [4,5,6] [1,2,3]       -- [5,6,7,6,7,8,7,8,9]

-- 同一インデックスの要素同士を演算したい場合には zipWith を使用する
r21  = zipWith (*) [1,2,3] [2,3,4]                       -- => [2,6,12]
r22  = foldr (+) 0 $ zipWith (*) [1,2,3] [2,3,4]         -- ベクトルの内積 => 20
r22' = sum $ zipWith (*) [1,2,3] [2,3,4]                 -- 同上
r23  = zip [1,2,3] [2,3,4]                               -- => [(1,2),(2,3),(3,4)]
r24  = foldr (+) 0 $ zipWith (\x y->x*y) [1,2,3] [2,3,4] -- ベクトルの内積 => 20

-- 1つのリストに対して関数のリストを適用すると、総当たりの結果を返す
r30  = [(*2),(+3)] `ap` [1,2,3]           -- [2,4,6,4,5,6]
r30' = [(*2),(+3)] <*> [1,2,3]            -- [2,4,6,4,5,6]
r31  = [(+)] `ap` [2,3] `ap` [1,2,3]      -- [3,4,5,4,5,6]
r31' = (+) <$> [2,3] <*> [1,2,3]          -- [3,4,5,4,5,6]
r32  = [(*)] `ap` [2,3] `ap` [1,2,3]      -- [2,4,6,3,6,9]
r32' = (*) <$> [2,3] <*> [1,2,3]          -- [2,4,6,3,6,9]
r33  = [(*),(+)] `ap` [2,3] `ap` [1,2,3]  -- [2,4,6,3,6,9,3,4,5,4,5,6]

-- 関数合成(関数がFunctorであること)
f40  = fmap (+ 1) (+ 2)             -- (+ 1) と (+ 2) を合成して (+ 3) を得る
f40' = (+ 1) . (+ 2)                -- . による合成と等価
r40  = f40 5                        -- => 8
r40' = f40 $ 5                      -- => 8
r41  = fmap f40 (Just 5)            -- => Just 8
r41' = fmap f40 $ Just 5            -- => Just 8

-- タプルとリストの相互変換
r50  = zip [1,2,3] [2,3,4]                   -- => 結果はタプルで返る [(1,2),(2,3),(3,4)]
r50' = zipWith (\x y->[x,y]) [1,2,3] [2,3,4] -- => 結果はリストで返る [[1,2],[2,3],[3,4]]
f51 :: (a,a) -> [a]                          -- タプル→リスト変換関数(個別)
f51  = \(x,y) -> x:[y]
f51' :: [(a,a)] -> [[a]]                     -- タプル→リスト変換関数(複数)
f51' = map (\(x,y) -> x:[y])
r51  = f51' $ [(1,2),(2,3),(3,4)]            -- => [[1,2],[2,3],[3,4]]
f52 :: [a] -> (a,a)                          -- リスト→タプル変換関数(個別)
f52  = \(x:xs) -> (x,head xs)
f52' :: [[a]] -> [(a,a)]                     -- リスト→タプル変換関数(複数)
f52' = map (\(x:xs) -> (x,head xs))
r52  = f52' $ [[1,2],[2,3],[3,4]]            -- => [(1,2),(2,3),(3,4)]

r53  = (,) 1 101              -- タプルのコンストラクタ(2引数) => (1,101)
f53  = (,) 1                  -- タプルのコンストラクタ: 2要素のうち1つを部分適用
r53' = f53 101                -- タプルのコンストラクタ(1引数) => (1,101)

r54  = [1,101]                -- [1,101] => 1:[101] => 1:101:[]
r54' = (1:) $ (:[]) 101       -- リストのコンストラクタ(2要素) => [1,101]
                              -- (:) :: a -> [a] -> [a]
                              -- (1:) :: Num a => [a] -> [a]
                              -- (:[]) 101     => [101]
                              -- (1:) $ (:[]) 101 :: Num a => [a]

f55 :: Num a => [a] -> [a]    -- リストのコンストラクタ(1要素を部分適用)
f55  = (:)1
r55  = (:)1 $ [101]           -- => [1,101]
f56 :: Num a => a -> [a]      -- リストのコンストラクタ(部分適用引数を入れ替える)
f56  = (:[101])
r56  = (:[101]) 1             -- => [1,101]

-- Maybeモナド演算 :: Maybe Int
r60  = Just 5   >>= Just . (3 +)                                    -- => Just 8
r60' = Nothing  >>= Just . (3 +)                                    -- => Nothing
r61  = return 5 >>= (\x -> if x > 7 then Nothing else Just (3 + x)) -- => Just 8
r61' = return 8 >>= (\x -> if x > 7 then Nothing else Just (3 + x)) -- => Nothing
r62  = Just 5   >>= (\x -> if x > 7 then Nothing else Just (3 + x)) -- => Just 8
r62' = Just 8   >>= (\x -> if x > 7 then Nothing else Just (3 + x)) -- => Nothing
r63a = (Just 1) `mplus` (Just 2)                                    -- => Just 1
r63b = (Just 1) `mplus` Nothing                                     -- => Just 1
r63c = Nothing  `mplus` (Just 2)                                    -- => Just 2
r63d = Nothing  `mplus` Nothing                                     -- => Nothing
r64  = mzero :: Maybe Int                                           -- => Nothing

-- リストモナド演算 :: [Int]
r70  = [1,2,3] >>= (:[]) . (3 +)                                    -- => [4,5,6]
r70' = concatMap return [1,2,3] >>= (:[]) . (3 +)                   -- => [4,5,6]
r71  = return [1,2,3] >>= (\x -> return (3 +) <*> x)                -- => [4,5,6]
r71' = [[1,2,3]] >>= (\x -> return (3 +) <*> x)                     -- => [4,5,6]
r72  = map return [1,2,3] >>= (\x -> return (3 +) <*> x)            -- => [4,5,6]
f73  = \x -> if x > 7 then [] else [3 + x]          -- リストの各要素に対する演算
r73  = concatMap return [1,2,3] >>= f73             -- => [4,5,6]
r73' = concatMap return [6,7,8,9,10] >>= f73        -- => [9,10]
r74  = [1,2,3] `mplus` [4,5,6]                      -- => [1,2,3,4,5,6]
r74' = [1,2,3] ++ [4,5,6]                           -- => [1,2,3,4,5,6]
r75  = mzero ::[Int]                                -- => []
r75a = 1:2:3:mzero                                  -- => [1,2,3]
r75b = 1:2:3:[]                                     -- => [1,2,3]

-- Eitherモナド演算 :: Either String Int
r80  = Right 5  >>= Right . (3 +)                                     -- => Right 8
r80' = Left "Error" >>= Right . (3 +)                                 -- => Left "Error"
r81  = return 5 >>= (\x -> if x>7 then Left "Error" else Right (3+x)) -- => Right 8
r81' = return 8 >>= (\x -> if x>7 then Left "Error" else Right (3+x)) -- => Left "Error"
r82  = Right 5 >>= (\x -> if x>7 then Left "Error" else Right (3+x))  -- => Right 8
r82' = Right 8 >>= (\x -> if x>7 then Left "Error" else Right (3+x))  -- => Left "Error"

