#!/usr/bin/env runhaskell

-- Lazy K
-- https://www.npca.jp/works/magazine/2012_3/

-- I x = x
-- K x y = x
-- S x y z = (x z) (y z)
-- I = SKK と表せる。

import Data.Char (isSpace)
import Data.Functor ((<$>))
import System.Environment
import Text.Parsec
import Text.Parsec.String
data Expr = V Int -- Value (Index)
          | L Expr -- Lambda
          | A Expr Expr -- Application
          deriving Eq

-- evaluate
step :: Expr -> Expr
step (A f e) = step' (A (step f) e)
  where step' (A (L v) e) = step $ subst 0 v (step e)
        step' (A g e) = A g (step e)
step (L e) = L $ step e
step e = e

subst :: Int -> Expr -> Expr -> Expr -- substitute
subst v (L w) e = L (subst (v + 1) w e)
subst v (A m n) e = A (subst v m e) (subst v n e)
subst v (V n) e | n < v = V n
                | n == v = subst' 0 v e
                | n > v = V (n - 1)
  where subst' t v (L w)   = L (subst' (t + 1) v w)
        subst' t v (A m n) = A (subst' t v m) (subst' t v n)
        subst' t v (V n) | n < t  = V n
                         | n >= t = V (n + v)

-- combinators
s, k, i, u, b, zero, suc, cons, car, cdr, nil :: Expr
s = L (L (L (A (A (V 2) (V 0)) (A (V 1) (V 0)))))
k = L (L (V 1))
i = L (V 0)
u = L (A (A (V 0) s) k)
b = L (L (L (A (A (V 2) (V 1)) (V 0))))
zero = (L (L (V 0)))
suc = (L (L (L (A (V 1) (A (A (V 2) (V 1)) (V 0))))))
cons = (L (L (L (A (A (V 0) (V 2)) (V 1)))))
car = (L (A (V 0) (L (L (V 1)))))
cdr = (L (A (V 0) (L (L (V 0)))))
nil = (L (L (V 0)))

-- encode / decode
s2e :: String -> Expr -- input
s2e [] = nil
s2e (c:cs) = A (A cons $ i2e $ fromEnum c) $ s2e cs
  where i2e 0 = zero
        i2e n = A suc $ i2e $ n - 1
e2s :: Expr -> String -- output
e2s e | e == nil = ""
      | otherwise = (toEnum $ e2s' $ step $ (A car e)) : (e2s $ step $ A cdr e)
  where e2s' (L (L (V _))) = 0
        e2s' (L (L e)) = f e 0
        e2s' _ = 0
        f (A _ n) x = f n (x + 1)
        f _ x = x

-- parsers
pExpr, pTerm, pS, pK, pI, pU, pB, pCb, pAp :: Parser Expr
pExpr = foldl1 A <$> many1 pTerm
pTerm = pS <|> pK <|> pI <|> pU <|> pB <|> pCb <|> pAp
pS = (char 'S' <|> char 's') >> return s
pK = (char 'K' <|> char 'k') >> return k
pI = char 'I' >> return i
pU = (char 'i' <|> char '0') >> return u
pB = (char '1') >> return b
pCb = between (char '(') (char ')') pExpr
pAp = (char '`' <|> char '*') >> pTerm >> pTerm
eval :: String -> String -> String
eval code input = case parse pExpr "" code of
  Left err -> show err
  Right val -> e2s $ step $ A val (s2e input)

main :: IO()
main = do
  [path] <- getArgs
  code <- filter (not.isSpace) <$> readFile path
  input <- getContents
  putStr $ eval code input

