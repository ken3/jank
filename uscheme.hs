--
-- mscheme.hs : microScheme
--
--              Copyright (C) 2013 Makoto Hiroi
--
-- 作成: 2017-11-30 ken3@nurs.or.jp
-- 更新: 2017-12-01 ken3@nurs.or.jp
--
-- ChangeLog
-- 2017-12-01 Ctrl+Dを入力するとEOFの無限ループに陥るのを修正した。
-- 2017-12-01 replはEOF検出時に終了させるが、loadは終了しないようにした。
-- 2017-12-01 repl起動前に標準ライブラリをロードする処理を追加した。

import Data.Char
import Data.IORef
import qualified Data.HashTable as H
import Control.Monad.Error
import Control.Monad.Trans.Except
import System.IO

-- S 式の定義
type ScmFunc = Env -> SExpr -> Scm SExpr

data SExpr = INT  !Integer
           | REAL !Double
           | SYMBOL  String
           | STRING  String
           | CELL SExpr SExpr
           | NIL
           | PRIMITIVE ScmFunc
           | SYNTAX ScmFunc
           | CLOSURE SExpr LEnv
           | MACRO SExpr

-- 等値の定義
instance Eq SExpr where
    INT x  == INT y  = x == y
    REAL x == REAL y = x == y
    SYMBOL x  == SYMBOL y  = x == y
    STRING x  == STRING y  = x == y
    NIL    == NIL    = True
    _      == _      = False

-- パーサエラーの定義
data ParseErr = ParseErr String String deriving Show

instance Error ParseErr where
    noMsg    = ParseErr "" ""
    strMsg s = ParseErr "" s

-- パーサの定義
type Parser a = Either ParseErr a

-- 評価器の定義
type Scm a = ErrorT String IO a

-- ローカル環境の定義
type LEnv = [(String, IORef SExpr)]

pushLEnv :: String -> SExpr -> LEnv -> IO LEnv
pushLEnv s v env = do a <- v `seq` newIORef v
                      return ((s, a):env)

lookupLEnv :: String -> LEnv -> IO (Maybe SExpr)
lookupLEnv s env = case lookup s env of
    Nothing -> return Nothing
    Just v  -> do a <- readIORef v
                  return (Just a)

updateLEnv :: String -> SExpr -> LEnv -> IO (LEnv)
updateLEnv s v env = case lookup s env of
    Nothing -> pushLEnv s v env
    Just a  -> do writeIORef a v
                  return env

-- グローバルな環境
type GEnv = H.HashTable String SExpr

-- 両方の環境を保持する
type Env = (GEnv, LEnv)

-- 真偽値
true  = SYMBOL "true"
false = SYMBOL "false"

-- Primitive の定義
errNUM  = "Illegal argument, Number required"
errINT  = "Illegal argument, Integer required"
errNEA  = "Not enough arguments"
errCELL = "Illegal argument, List required"
errZERO = "Divide by zero"

-- リスト操作
car, cdr, cons, pair :: ScmFunc
car _ NIL = throwError $ strMsg $ "car : " ++ errNEA
car _ (CELL (CELL a _) _) = return a
car _ _ = throwError $ strMsg $ "car : " ++ errCELL

cdr _ NIL = throwError $ strMsg $ "cdr : " ++ errNEA
cdr _ (CELL (CELL _ d) _) = return d
cdr _ _ = throwError $ strMsg $ "cdr : " ++ errCELL

cons _ (CELL a (CELL b _)) = return (CELL a b)
cons _ _ = throwError $ strMsg $ "cons : " ++ errNEA

pair _ NIL                 = throwError $ strMsg $ "pair? : " ++ errNEA
pair _ (CELL (CELL _ _) _) = return true
pair _ _ = return false

-- 畳み込み
foldCell :: (SExpr -> SExpr -> Scm SExpr) -> SExpr -> SExpr -> Scm SExpr
foldCell _ a NIL = return a
foldCell f a (CELL x rest) = do v <- f a x
                                foldCell f v rest
foldCell _ _ _ = throwError $ strMsg errCELL

-- 四則演算
adds, subs, muls, divs, mod' :: ScmFunc
add, sub, mul, div' :: SExpr -> SExpr -> Scm SExpr

add (INT x)  (INT y)  = return (INT (x + y))
add (INT x)  (REAL y) = return (REAL (fromIntegral x + y))
add (REAL x) (INT y)  = return (REAL (x + fromIntegral y))
add (REAL x) (REAL y) = return (REAL (x + y))
add _ _ = throwError $ strMsg $ "+ : " ++ errNUM

adds _ xs = foldCell add (INT 0) xs

sub (INT x)  (INT y)  = return (INT (x - y))
sub (INT x)  (REAL y) = return (REAL (fromIntegral x - y))
sub (REAL x) (INT y)  = return (REAL (x - fromIntegral y))
sub (REAL x) (REAL y) = return (REAL (x - y))
sub _ _ = throwError $ strMsg $ "- : " ++ errNUM

subs _ NIL = throwError $ strMsg $ "- : " ++ errNEA
subs _ (CELL (INT a) NIL)  = return (INT (-a))
subs _ (CELL (REAL a) NIL) = return (REAL (-a))
subs _ (CELL a rest) = foldCell sub a rest

mul (INT x)  (INT y)  = return (INT (x * y))
mul (INT x)  (REAL y) = return (REAL (fromIntegral x * y))
mul (REAL x) (INT y)  = return (REAL (x * fromIntegral y))
mul (REAL x) (REAL y) = return (REAL (x * y))
mul _ _ = throwError $ strMsg $ "- : " ++ errNUM

muls _ xs = foldCell mul (INT 1) xs

div' _        (INT 0)  = throwError $ strMsg errZERO
div' _        (REAL 0) = throwError $ strMsg errZERO
div' (INT x)  (INT y)  = return (INT (x `div` y))
div' (INT x)  (REAL y) = return (REAL (fromIntegral x / y))
div' (REAL x) (INT y)  = return (REAL (x / fromIntegral y))
div' (REAL x) (REAL y) = return (REAL (x / y))
div' _ _ = throwError $ strMsg $ "- : " ++ errNUM

divs _ NIL = throwError $ strMsg $ "/ : " ++ errNEA
divs _ (CELL a NIL)  = div' (INT 1) a
divs _ (CELL a rest) = foldCell div' a rest

mod' _ NIL          = throwError $ strMsg $ "mod : " ++ errNEA
mod' _ (CELL _ NIL) = throwError $ strMsg $ "mod : " ++ errNEA
mod' _ (CELL _ (CELL (INT 0) _))  = throwError $ strMsg errZERO
mod' _ (CELL _ (CELL (REAL 0) _)) = throwError $ strMsg errZERO
mod' _ (CELL (INT x) (CELL (INT y) _)) = return (INT (mod x y))
mod' _ _ = throwError $ strMsg $ "mod : " ++ errINT

-- 等値の判定
eq', equal' :: ScmFunc

eq' _ (CELL x (CELL y _)) =
  if x == y then return true else return false
eq' _ _ = throwError $ strMsg $ "eq : " ++ errNEA

equal' _ (CELL x (CELL y _)) =
  if iter x y then return true else return false
  where iter (CELL a b) (CELL c d) = iter a c && iter b d
        iter x y = x == y
equal' _ _ = throwError $ strMsg $ "equal : " ++ errNEA

-- 数値の比較演算子
compareNum :: SExpr -> SExpr -> Scm Ordering
compareNum (INT x)  (INT y)  = return $ compare x y
compareNum (INT x)  (REAL y) = return $ compare (fromIntegral x) y
compareNum (REAL x) (INT y)  = return $ compare x (fromIntegral y)
compareNum (REAL x) (REAL y) = return $ compare x y
compareNum _ _ = throwError $ strMsg errNUM

compareNums :: (Ordering -> Bool) -> SExpr -> Scm SExpr
compareNums _ NIL          = throwError $ strMsg errNEA
compareNums _ (CELL _ NIL) = throwError $ strMsg errNEA
compareNums p (CELL x (CELL y NIL)) = do
  r <- compareNum x y
  if p r then return true else return false
compareNums p (CELL x ys@(CELL y _)) = do
  r <- compareNum x y
  if p r then compareNums p ys else return false
compareNums _ _ = throwError $ strMsg "invalid function form"

eqNum, ltNum, gtNum, ltEq, gtEq :: ScmFunc
eqNum _ = compareNums (== EQ)
ltNum _ = compareNums (== LT)
gtNum _ = compareNums (== GT)
ltEq  _ = compareNums (<= EQ)
gtEq  _ = compareNums (>= EQ)

-- apply
apply' :: ScmFunc
apply' _ (CELL _ NIL) = throwError $ strMsg $ "apply : " ++ errNEA
apply' env (CELL func args) = do
  xs <- iter args
  apply env func xs
  where iter (CELL NIL NIL) = return NIL
        iter (CELL xs@(CELL _ _) NIL) = return xs
        iter (CELL _ NIL) = throwError $ strMsg errCELL
        iter (CELL x xs) = do ys <- iter xs
                              return (CELL x ys)
apply' _ _ = throwError $ strMsg $ "apply : " ++ errNEA

-- エラー
error' :: ScmFunc
error' _ (CELL (STRING x) NIL) = throwError $ strMsg $ "ERROR: " ++ x
error' _ (CELL (STRING x) (CELL y _)) = let m = "ERROR: " ++ x ++ " " ++ show y
                                        in throwError m
error' _ (CELL x _) = throwError $ strMsg $ "ERROR: " ++ show x
error' _ _ = throwError $ strMsg "ERROR: "

-- load
load :: ScmFunc
load env (CELL (STRING filename) _) = do
    xs <- lift $ readFile filename
    r <- lift $ iter xs
    if r then return true
         else return false
    where iter :: String -> IO Bool
          iter xs = case readSExpr True xs of
               Left  (ParseErr xs' m) -> if m == "EOF"
                                            then return True
                                            else print m >> return False
               Right (expr, xs') -> do result <- runErrorT $ eval env expr 
                                       case result of
                                            Left m  -> print m >> return False
                                            Right _ -> iter xs'
load _ _ = throwError $ strMsg "invalid load form"

--
-- S 式の表示
--
showCell :: SExpr -> String
showCell (CELL a d) =
  show a ++ case d of NIL           -> ""
                      PRIMITIVE _   -> "<primitive>"
                      CLOSURE _ _   -> "<closure>"
                      SYNTAX _      -> "<syntax>"
                      MACRO _       -> "<macro>"
                      INT x         -> " . " ++ show x
                      REAL x        -> " . " ++ show x
                      SYMBOL x      -> " . " ++ x
                      STRING x      -> " . " ++ show x
                      _             -> " " ++ showCell d
showCell xs = show xs

instance Show SExpr where
  show (INT x)       = show x
  show (REAL x)      = show x
  show (SYMBOL x)    = x
  show (STRING x)    = show x
  show NIL           = "()"
  show (SYNTAX _)    = "<syntax>"
  show (PRIMITIVE _) = "<primitive>"
  show (CLOSURE _ _) = "<closure>"
  show (MACRO _)     = "<macro>"
  show xs            = "(" ++ showCell xs ++ ")"

--
-- S 式の読み込み
--

isAlpha' :: Char -> Bool
isAlpha' x = elem x "!$%&*+-/:<=>?@^_~"

isIdent0 :: Char -> Bool
isIdent0 x = isAlpha x || isAlpha' x

isIdent1 :: Char -> Bool
isIdent1 x = isAlphaNum x || isAlpha' x

isREAL :: Char -> Bool
isREAL x = elem x ".eE"

quote           = SYMBOL "quote"
quasiquote      = SYMBOL "quasiquote"
unquote         = SYMBOL "unquote"
unquoteSplicing = SYMBOL "unquote-splicing"

isNUM :: String -> Bool
isNUM (x:_) = isDigit x
isNUM _     = False

getNumber :: String -> Parser (SExpr, String)
getNumber xs =
  let (s, ys) = span isDigit xs
  in if not (null ys) && isREAL (head ys)
     then case reads xs of
            [] -> throwError noMsg  -- ありえないエラー
            [(y', ys')] -> return (REAL y', ys')
     else return (INT (read s), ys)

-- S式をインタラクティブに読み込んで実行する
-- 2017-12-01 replから呼ばれた場合 batch = True, loadから呼ばれた場合 batch = False とする。
readSExpr :: Bool -> String -> Parser (SExpr, String)
readSExpr batch [] = if batch then throwError $ strMsg "EOF" -- EOFを検出してもREPLを終了しない。
                              else error "quit" -- EOF(Ctrl+D)を検出したらREPLを終了する。
-- FIXME: 2017-12-01 S式の途中でCtrl+Dを検出した場合に error "quit" するのが気に入らない。

readSExpr batch (x:xs) 
  | isSpace x  = readSExpr batch xs
  | isDigit x  = getNumber (x:xs)
  | isIdent0 x = if x == '+' && isNUM xs
                 then getNumber xs
                 else if x == '-' && isNUM xs
                 then do (y, ys) <- getNumber xs
                         case y of
                           INT x  -> return (INT  (- x), ys)
                           REAL x -> return (REAL (- x), ys)
                 else let (name, ys) = span isIdent1 (x:xs)
                      in return (SYMBOL name, ys)
  | otherwise  = case x of
        '('  -> readCell batch 0 xs
        ';'  -> readSExpr batch $ dropWhile (/= '\n') xs
        '"'  -> case reads (x:xs) of [] -> throwError noMsg
                                     [(y, ys)] -> return (STRING y, ys)
        '\'' -> readSExpr batch xs >>= expandQuote
        '`'  -> readSExpr batch xs >>= expandQuasiquote
        ','  -> if not (null xs) && head xs == '@'
                  then readSExpr batch (tail xs) >>= expandUnquoteSplicing
                  else readSExpr batch xs >>= expandUnquote
        _    -> throwError $ ParseErr xs ("unexpected token: " ++ show x)

expandQuote (e, ys) = return (CELL quote (CELL e NIL), ys)
expandQuasiquote (e, ys) = return (CELL quasiquote (CELL e NIL), ys)
expandUnquote (e, ys) = return (CELL unquote (CELL e NIL), ys)
expandUnquoteSplicing (e, ys) = return (CELL unquoteSplicing (CELL e NIL), ys)

readCell :: Bool -> Int -> String -> Parser (SExpr, String)
readCell batch _ [] = throwError $ strMsg "EOF"
readCell batch n (x:xs)
  | isSpace x = readCell batch n xs
  | otherwise = case x of
        ')' -> return (NIL, xs)
        '.' -> if n == 0
               then throwError errorMsg
               else do (e, ys) <- readSExpr batch xs
                       case dropWhile isSpace ys of
                         ')':zs -> return (e, zs)
                         _      -> throwError errorMsg
        '(' -> do (a, ys) <- readCell batch 0 xs
                  (d, zs) <- readCell batch 1 ys
                  return (CELL a d, zs)
        _   -> do (a, ys) <- readSExpr batch (x:xs)
                  (d, zs) <- readCell batch 1 ys
                  return (CELL a d, zs)
 where errorMsg = ParseErr xs "invalid dotted list"

--
-- S 式の評価
--
eval :: Env -> SExpr -> Scm SExpr
eval env NIL        = return NIL
eval env v@(INT _)  = return v
eval env v@(REAL _) = return v
eval env v@(STRING _)  = return v
eval env (SYMBOL name) = do
  let unboundMsg = strMsg $ "unbound variable: " ++ name
  a <- liftIO $ lookupLEnv name $ snd env
  case a of Nothing -> do b <- liftIO $ H.lookup (fst env) name
                          case b of
                               Nothing -> throwError unboundMsg
                               Just v  -> return v
            Just v  -> return v
eval env (CELL func args) = do
  v <- eval env func
  case v of SYNTAX f -> f env args
            MACRO f  -> do expr <- apply env f args
                           eval env expr
            _ -> do vs <- evalArguments env args
                    apply env v vs

-- 引数の評価
evalArguments :: Env -> SExpr -> Scm SExpr
evalArguments env NIL = return NIL
evalArguments env (CELL expr rest) = do
  v  <- eval env expr
  vs <- evalArguments env rest
  return (CELL v vs)
evalArguments _ _ = throwError $ strMsg "invalid function form"

-- 変数束縛
makeBindings :: LEnv -> SExpr -> SExpr -> Scm LEnv
makeBindings lenv NIL        _    = return lenv
makeBindings lenv (SYMBOL name) rest = lift $ pushLEnv name rest lenv
makeBindings lenv (CELL (SYMBOL name) parms) (CELL v args) = do
  lenv' <- makeBindings lenv parms args
  lift (pushLEnv name v lenv')
makeBindings lenv _ NIL = throwError $ strMsg errNEA
makeBindings lenv _ _   = throwError $ strMsg "invalid arguments form"

-- 関数適用
apply :: Env -> SExpr -> SExpr -> Scm SExpr
apply env func actuals =
  case func of
    PRIMITIVE f -> f env actuals
    CLOSURE (CELL parms body) lenv0 -> do
      lenv1 <- makeBindings lenv0 parms actuals
      evalBody (fst env, lenv1) body
    _ -> throwError $ strMsg $ "Not Function: " ++ show func

-- 本体の評価
evalBody :: Env -> SExpr -> Scm SExpr
evalBody env (CELL expr NIL) = eval env expr
evalBody env (CELL expr rest) = do
  eval env expr
  evalBody env rest
evalBody _ _ = throwError $ strMsg "invalid body form"

--
-- シンタックス形式
--

-- quote
evalQuote :: Env -> SExpr -> Scm SExpr
evalQuote env (CELL expr _) = return expr
evalQuote _ _ = throwError $ strMsg "invalid quote form"

-- define
evalDef :: Env -> SExpr -> Scm SExpr
evalDef env (CELL sym@(SYMBOL name) (CELL expr NIL)) = do
  v <- eval env expr
  lift $ H.update (fst env) name v
  return sym
evalDef _ _ = throwError $ strMsg "invalid define form"

-- define-macro
evalDefM :: Env -> SExpr -> Scm SExpr
evalDefM env (CELL sym@(SYMBOL name) (CELL expr NIL)) = do
  v <- eval env expr
  lift $ H.update (fst env) name (MACRO v)
  return sym
evalDefM _ _ = throwError $ strMsg "invalid define-macro form"

-- if
evalIf :: Env -> SExpr -> Scm SExpr
evalIf env (CELL pred (CELL thenForm rest)) = do
  v <- eval env pred
  if v /= false
  then eval env thenForm
  else case rest of
         CELL elseForm _ -> eval env elseForm
         _               -> return false
evalIf _ _ = throwError $ strMsg $ "if : " ++ errNEA

-- lambda
evalLambda :: Env -> SExpr -> Scm SExpr
evalLambda env expr = return (CLOSURE expr (snd env))

-- set!
evalSet :: Env -> SExpr -> Scm SExpr
evalSet env (CELL (SYMBOL name) (CELL expr _)) = do
  let unboundMsg = strMsg $ "unbound variable: " ++ name
  v <- eval env expr
  a <- lift $ lookupLEnv name (snd env)
  case a of
    Nothing -> do b <- lift $ H.lookup (fst env) name
                  case b of
                    Nothing -> throwError unboundMsg
                    Just _ -> do lift $ H.update (fst env) name v
                                 return v
    Just _  -> do lift $ updateLEnv name v (snd env)
                  return v
evalSet _ _ = throwError (strMsg "invalid set! form")

--
-- 大域変数の初期化
--
initGEnv :: [(String, SExpr)]
initGEnv = [("true",   true),
            ("false",  false),
            ("quote",  SYNTAX evalQuote),
            ("define", SYNTAX evalDef),
            ("lambda", SYNTAX evalLambda),
            ("if",     SYNTAX evalIf),
            ("set!",   SYNTAX evalSet),
            ("define-macro", SYNTAX evalDefM),
            ("eq?",    PRIMITIVE eq'),
            ("equal?", PRIMITIVE equal'),
            ("pair?",  PRIMITIVE pair),
            ("+",      PRIMITIVE adds),
            ("-",      PRIMITIVE subs),
            ("*",      PRIMITIVE muls),
            ("/",      PRIMITIVE divs),
            ("mod",    PRIMITIVE mod'),
            ("=",      PRIMITIVE eqNum),
            ("<",      PRIMITIVE ltNum),
            (">",      PRIMITIVE gtNum),
            ("<=",     PRIMITIVE ltEq),
            (">=",     PRIMITIVE gtEq),
            ("car",    PRIMITIVE car),
            ("cdr",    PRIMITIVE cdr),
            ("cons",   PRIMITIVE cons),
            ("load",   PRIMITIVE load),
            ("apply",  PRIMITIVE apply'),
            ("error",  PRIMITIVE error')]

-- REPLのプロンプト表示処理
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- read-eval-print-loop
repl :: Env -> String -> IO ()
repl env xs = do
     flushStr "Scm> "
     case readSExpr False xs of
          Left  (ParseErr xs' mes) -> do putStrLn mes
                                         repl env $ dropWhile (/= '\n') xs'
          Right (expr, xs') -> do result <- runErrorT $ eval env expr 
                                  case result of
                                       Left mes -> putStrLn mes
                                       Right v  -> print v
                                  repl env xs'

-- read-eval-print (oneshot)
repo :: Env -> String -> IO ()
repo env xs = do
       putStr $ xs ++ " => "
       case readSExpr False xs of
            Left  (ParseErr xs' mes) -> putStrLn mes
            Right (expr, xs') -> do result <- runErrorT $ eval env expr 
                                    case result of
                                         Left mes -> putStrLn mes
                                         Right v  -> print v

-- メイン処理
-- FIXME: hGetContentsを使うと、行単位で判定処理を入れるのが難しい。
main :: IO ()
main = do ht <- H.fromList H.hashString initGEnv
          let env = (ht, [])
          repo env "(load \"lib.scm\")"
          xs <- hGetContents stdin
          repl env xs

