#!/usr/local/bin/runghc
{-# LANGUAGE ExistentialQuantification #-}

-- 機能: Schemeインタプリタ - "48時間でSchemeを書こう"より
-- 作成: 2017-11-21  ken3@nurs.or.jp
-- 更新: 2017-11-25  ken3@nurs.or.jp

module Main where
import Control.Monad
import System.Environment
import Control.Monad.Error
import Control.Monad.Trans.Except
import Control.Exception as E hiding (try, interruptible)
import Data.IORef
import Data.Char
import Data.List
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO hiding (try)
import System.Exit
import Text.Printf
import System.Posix.Signals hiding (Default)
import Debug.Trace

-- パラメータ定義
program = "Scheme48"
version = "v1.0.0"
prompt  = program ++ "> "

-- トレース制御
-- enableTrace = False
enableTrace = True

-- REPLを正常終了させるための例外
data ExitException = ExitException deriving (Show)
instance Exception ExitException

-- SIGINTハンドラ(Ctrl+C対策)
handler :: IO ()
handler = putStrLn "" >> flushStr prompt
maskInterrupt :: IO System.Posix.Signals.Handler
maskInterrupt = installHandler keyboardSignal (Catch handler) Nothing

-- 2017-11-22 エンジン初期化時に標準ライブラリをロードする機能を追加
-- 2017-11-24 ExitExceptionでREPLを正常終了する機能を追加
-- 2017-11-24 Ctrl+C対策を追加。
main :: IO ()
main = do args <- getArgs
          env  <- primitiveBindings
          loadLibs env stdlibs
          if null args then maskInterrupt >> runRepl env
                       else runOne env args
    `catch` \(ExitException) -> return ()

-- 2017-11-22 標準ライブラリファイル名(複数ファイルを記述可能)
stdlibs :: [String]
stdlibs = ["stdlib.scm"]

-- Parsecに関するメモ
-- sepByコンビネータ: 区切り子が解析できなくなるまで解析を続ける
-- endByコンビネータ: 行解析(最後の要素の末尾に区切り子が必要)
-- <|>演算子: 左側要素優先でマッチングを試す
-- try関数: 先読みを表現する

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func {params  :: [String],
                     vararg  :: (Maybe String),
                     body    :: [LispVal],
                     closure :: Env}
             | ReplQuit

-- 式をパーズする
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

-- 2017-11-25 記号を認識するパーザ
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

-- 2017-11-25 連続した空白を読み飛ばすパーザ
spaces :: Parser ()
spaces = skipMany1 space

-- 2017-11-25 文字列を認識するパーザ
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

-- 2017-11-25 アトムを認識するパーザ
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _ -> Atom atom

-- 2017-11-25 数値を認識するパーザ
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- 2017-11-25 リストを認識するパーザ
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- 2017-11-25 ドット対を認識するパーザ
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- 2017-11-25 シングルクオート構文を認識するパーザ
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- 2017-11-25 S式を認識するパーザ
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               -- リストの解析にバックトラックを適用
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x
        <?> "expr"

-- LispValを文字列化する
instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
    "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
  "(lambda (" ++ unwords (map show args) ++ 
     (case varargs of 
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)" 

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- 評価器
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) = 
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- 関数適用
-- 2017-11-22 パターンマッチを抜ける場合があるのでデフォルトパターンを追加。
-- FIXME: ioPrimitiveFunc(read, write等)がマッチしない。
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else ioenv >>= ioargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body 
          bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env 
          ioenv = liftIO $ bindVars closure $ zip params args
          ioargs = bindVarArgs varargs
apply _ _ = throwError $ Default "*** Exhaust patterns in function apply ***"

-- 評価エラーの種類
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
instance Error LispError where
     noMsg  = Default "An error has occurred"
     strMsg = Default

-- LispErrorを文字列化する
-- 2017-11-22 Defaultのメッセージを追加。
instance Show  LispError where show = showError
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
    "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default message) = "Unexpected condition: " ++ show message

type ThrowsError = Either LispError

trapError action = do catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- REPLのプロンプト表示処理
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- REPLの1行入力処理
-- FIXME: 入力する式は1行で完結していることが前提。
readPrompt :: IO String
readPrompt = do
    flushStr prompt
    eof <- isEOF
    if eof then putStrLn "" >> return "" -- EOF(Ctrl+D)
           else getLine

-- REPLの1行評価
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- 入力された式をパーズ・評価する
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ do
    val <- liftThrows $ readExpr expr
    ret <- eval env val
    -- 2017-11-24 evalの戻り値がReplQuitであれば例外を投げて終了する
    -- 2017-11-25 valとretのトレース出力処理を追加
    _TRACE val ret $ case ret of
      ReplQuit -> throw ExitException
      _ -> return ret
    where _TRACE v r e | enableTrace = trace ("TRACE: expr = " ++ (show expr) ++
                                            "\nTRACE: in = " ++ (show v) ++
                                            "\nTRACE: out = " ++ (show r)) e
                       | otherwise = e

-- 標準ライブラリをロードする
loadLibs :: Env -> [String] -> IO Env
loadLibs env args = do
    let commands = map (\x -> printf "(load \"%s\")" x) args
    mapM_ (\x -> evalAndPrint env x) commands
    return env

-- バッチモードのメインループ
runOne :: Env -> [String] -> IO ()
runOne env args = do
    let expr = List [Atom "load", String (args !! 0)]
    bindVars env [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env expr) >>= hPutStrLn stderr

-- 対話モードのメインループ
-- 2017-11-22 REPLプロンプトを変更。
runRepl :: Env -> IO ()
runRepl env = do result <- readPrompt
                 case result of
                   []  -> return ()
                   ':':body  -> replControl body
                   _ -> evalAndPrint env result
                 runRepl env

lowerCase :: String -> String
lowerCase (x:xs) = toLower x : lowerCase xs
lowerCase []     = []

-- 2017-11-24 REPL制御コマンド
-- 2017-11-24 REPL終了コマンドを :quit とした。
-- 2017-11-24 HELP表示コマンドを :help 又は :? とした。
-- 2017-11-25 コマンドの部分一致判定を実装した。
replCommands = ["?", "help", "quit"]
replControl :: String -> IO ()
replControl input = case body of
    "?"    -> helpCommand -- HELPを表示する。:だけ入力した時にもマッチする。
    "help" -> helpCommand -- HELPを表示する。
    "quit" -> quitCommand -- REPLを終了する。
    _ -> return ()
    where candies = filter (isPrefixOf $ lowerCase input) replCommands
          body | null candies = ""
               | otherwise  = head candies

-- 2017-11-25 終了コマンド
quitCommand :: IO ()
quitCommand = throw ExitException

-- 2017-11-24 HELP表示
helpCommand :: IO ()
helpCommand = do putStrLn $ program ++ " " ++ version
                 putStrLn ":quit or (quit) to exit interpreter."
                 putStrLn ":help or :? to show help."

-- Envの型定義
-- IORefは再代入可能な変数として扱える
type Env = IORef [(String, IORef LispVal)]

-- 空のEnv
nullEnv :: IO Env
nullEnv = newIORef []

-- 組み込み関数を登録済みのEnv
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ a ++ b)
    where embedFunc constructor (var, func) = (var, constructor func)
          a = map (embedFunc IOFunc) ioPrimitives
          b = map (embedFunc PrimitiveFunc) primitives

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Right val) = return val
liftThrows (Left err)  = throwError err

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do x <- runErrorT (trapError action)
                        return $ extractValue x

isBound :: Env -> String -> IO Bool
isBound envRef var = do x <- readIORef envRef
                        return $ maybe False (const True) (lookup var x)

-- Envからシンボルを取り出す
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                        (liftIO . readIORef)
                        (lookup var env)

-- Envのシンボルに再代入する
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var) 
                        (liftIO . (flip writeIORef value))
                        (lookup var env)
    return value

-- Envのシンボルに代入する
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value >> return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

-- 束縛を作成する
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

-- 関数を作成する
makeFunc vargs env params body = let ps = map showVal params in
    return Func {params = ps, vararg = vargs, body = body, closure = env}
makeNormalFunc = makeFunc Nothing
makeVarargs args = makeFunc $ Just (showVal args)

-- インタプリタ組み込みの基本関数定義
-- 2017-11-23 quitプリミティブを追加。
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("quit", quit),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = do xs <- mapM unpackNum params
                            return $ Number $ foldl1 op xs

boolBinop ::
  (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
    if null parsed then throwError $ TypeMismatch "number" $ String n
                   else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- 2017-11-23 quit関数を追加。
quit :: [LispVal] -> ThrowsError LispVal
quit _ = return ReplQuit

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $
    (length arg1 == length arg2) && (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of Left err -> False
                                                  Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- GHCの -XExistentialQuantification が必要。
-- With Existential-Quantification, foralls in data definitions means that,
-- the value contained can be of any suitable type, not that it must be of
-- all suitable types.
data Unpacker = forall a. Eq a => Unpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (Unpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
    `catchError` (const $ return False) -- doアクション中に例外が発生した時

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    let fs = [Unpacker unpackNum, Unpacker unpackStr, Unpacker unpackBool]
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) fs
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- インタプリタ組み込みのI/O関数定義
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine stdin) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

