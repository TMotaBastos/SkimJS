import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    proc <- stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    --if(proc == NaoDeclarado) then criarVarGlobal var e
    --else setVar var e
    case proc of
        NaoDeclarado -> criarVarGlobal var e
        _ -> setVar var e
--evalExpr env (AssignExpr OpAssign (LBracket expr1 expr2) expr) = do
--    case
evalExpr env (StringLit str) = return $ String str

evalExpr env (ArrayLit []) = return (Array [])
evalExpr env (ArrayLit a) = evalArray env a (Array [])

-- evalExpr env (ArrayLit (x:xs)) = return $ (List (evalExpr env x):(evalExpr env (ArrayLit xs)))
-- evalExpr env (ArrayLit (x:xs)) = return $ List (evalExpr env x) (evalExpr env (ArrayLit xs))
evalExpr env (UnaryAssignExpr op (LVar var)) = case op of
    PrefixInc -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1)))
    PrefixDec -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) (IntLit 1)))
    PostfixInc -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1)))
    PostfixDec -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) (IntLit 1)))

evalExpr env (CondExpr cond expr1 expr2) = do
    v <- evalExpr env cond
    if(v == (Bool True)) then
        evalExpr env expr1
    else if(v == (Bool False)) then
        evalExpr env expr2
    else
        return Nil

evalExpr env (FuncExpr maybeId id stmt) = do
    case maybeId of
        Nothing -> return $ Function (Id "fun") id stmt
        Just v -> return $ Function v id stmt

evalExpr env (CallExpr name args) =
    case name of
        -- DotRef?
        DotRef expr (Id id) -> return Nil
        _ -> do
            v <- evalExpr env name
            case v of
                (Function nome arguments stmt) -> do
            --if(v == (Function nome arguments stmt)) then do
                    pushScope
                    compareArgs env arguments args
                    retorno <- evalStmt env (BlockStmt stmt)
                    --error $ "Loucura meu " ++ show retorno
                    popScope
                    case retorno of
                        Return ret -> return ret
                        Break b -> error $ "Break usado errado"
                        _ -> return Nil

compareArgs env [] [] = return Nil
compareArgs env ((Id name):names) (arg:args) = do
    v <- evalExpr env arg
    criarVarLocal name v
    compareArgs env names args
compareArgs env _ _ = error $ "Numero de argumentos incompativeis"

--TA DANDO ERRO
evalArray :: StateT -> [Expression] -> Value -> StateTransformer Value
evalArray env [] (Array a) = return (Array a)
evalArray env (x:xs) (Array a) = do
    exprArray <- evalExpr env x
    evalArray env xs (Array (a ++ [exprArray])) --- ANTES tava a++exprArray

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil

evalStmt env (VarDeclStmt []) = return Nil

evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)

evalStmt env (ExprStmt expr) = evalExpr env expr

evalStmt env (IfSingleStmt expr stmt) = do
    v1 <- evalExpr env expr
    if(v1 == (Bool True)) then
        evalStmt env stmt
    else
        return Nil
evalStmt env (IfStmt expr stmt1 stmt2) = do
    v1 <- evalExpr env expr
    if(v1 == (Bool True)) then
        evalStmt env stmt1
    else if(v1 == (Bool False)) then
        evalStmt env stmt2
    else
        return Nil
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (x:xs)) = do
    v <- evalStmt env x
    case v of
        Break b -> return (Break b)
        Return r -> return (Return r)
        _ -> evalStmt env (BlockStmt xs)
{--evalStmt env (BlockStmt l) = do
    pushScope
    evalBlock env l
    popScope--}
evalStmt env (WhileStmt expr stmt) = do
    v1 <- evalExpr env expr
    if(v1 == (Bool True)) then
        do
            evalStmt env stmt
            evalStmt env (WhileStmt expr stmt)
    else
        return Nil
evalStmt env (DoWhileStmt stmt expr) = do
    evalStmt env stmt
    v1 <- evalExpr env expr
    if(v1 == (Bool True)) then
        evalStmt env (DoWhileStmt stmt expr)
    else
        return Nil

evalStmt env (ReturnStmt maybeExpr) = do
    case maybeExpr of
        Nothing -> return (Return Nil)
        (Just expr) -> do
            exprReturn <- evalExpr env expr
            return (Return exprReturn)

evalStmt env (BreakStmt maybeId) = do
    case maybeId of
        Nothing -> return (Break Nothing)
        (Just idBreak) -> return (Break (Just idBreak))

evalStmt env (ForStmt init maybeExpr1 maybeExpr2 stmt) = do
    forInit env init
    case maybeExpr1 of
        Nothing -> do
            stmtFor <- evalStmt env stmt
            case stmtFor of
                Break idB -> return Nil
                Return retorno -> return (Return retorno)
                _ -> do
                    case maybeExpr2 of
                        Nothing -> evalStmt env (ForStmt NoInit maybeExpr1 maybeExpr2 stmt)
                        Just expr -> do
                            evalExpr env expr
                            evalStmt env (ForStmt NoInit maybeExpr1 maybeExpr2 stmt)
        Just expr1 -> do
            v1 <- evalExpr env expr1
            if(v1 == (Bool True)) then do
                stmtFor <- evalStmt env stmt
                case stmtFor of
                    Break idB -> return Nil
                    Return retorno -> return (Return retorno)
                    _ -> do
                        case maybeExpr2 of
                            Nothing -> evalStmt env (ForStmt NoInit maybeExpr1 maybeExpr2 stmt)
                            Just expr -> do
                                evalExpr env expr
                                evalStmt env (ForStmt NoInit maybeExpr1 maybeExpr2 stmt)
            else {--if(v1 == (Bool False))--} return Nil
evalStmt env (SwitchStmt expr cases) = do
    v <- evalExpr env expr
    searchCase env v cases
evalStmt env (FunctionStmt (Id name) args body) =
    criarVarGlobal name (Function (Id name) args body)

searchCase env v [] = return Nil
searchCase env v (c:cs) = do
    case c of
        CaseClause expr stmt -> do
            v1 <- evalExpr env expr
            if(v == v1) then evalStmt env (BlockStmt stmt)
            else searchCase env v cs
        -- precisa desse do?
        CaseDefault stmt -> do
            evalStmt env (BlockStmt stmt)
-- evalStmt env ()

{--evalBlock :: StateT -> [Statement] -> StateTransformer Value
evalBlock env [] = return Nil
evalBlock env (x:xs) = do
    evalStmt env x
    evalBlock env xs--}

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

--
-- Environment and auxiliary functions
--

environment :: StateT
environment = [Map.empty]

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    {--case Map.lookup var (union s env) of
        Nothing -> error $ "Variable " ++ show var ++ " not defiend."
        Just val -> (val, s)--}
    case scopeLookup s var of
        Nothing -> (NaoDeclarado, s)
        Just valor -> (valor, s)

scopeLookup :: StateT -> String -> Maybe Value
scopeLookup [] _ = Nothing
scopeLookup (e:es) var =
    case Map.lookup var e of
        Nothing -> scopeLookup es var
        Just valor -> Just valor

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> criarVarLocal id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            criarVarLocal id val
     
forInit :: StateT -> ForInit -> StateTransformer Value       
forInit env (NoInit) = return Nil
forInit env (VarInit []) = return Nil
forInit env (VarInit (x:xs)) = do
    varDecl env x
    forInit env (VarInit xs)
forInit env (ExprInit expr) = do
    evalExpr env expr

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, (varProcurarAtualizar var val s))

varProcurarAtualizar :: String -> Value -> StateT -> StateT
varProcurarAtualizar var _ [] = error $ "Variable " ++ show var ++ " not defined."
varProcurarAtualizar var valor (e:es) = case (Map.lookup var e) of
    Nothing -> e:(varProcurarAtualizar var valor es)
    Just v -> (insert var valor e):es

pushScope :: StateTransformer Value
pushScope = ST (\estado -> (Nil, (Map.empty):estado))

popScope :: StateTransformer Value
popScope = ST (\estado -> (Nil, (tail estado)))

criarVarLocal :: String -> Value -> StateTransformer Value
criarVarLocal var valor = ST (\estado -> (valor, (insert var valor (head estado)):(tail estado)))

criarVarGlobal :: String -> Value -> StateTransformer Value
criarVarGlobal var valor = ST (\estado -> (valor, criarGlobal var valor estado))

criarGlobal :: String -> Value -> StateT -> StateT
criarGlobal var valor (e:es) = if(es == []) then
                                   (insert var valor e):[]
                               else e:(criarGlobal var valor es)

igualArray :: [Value] -> [Value] -> Bool
igualArray [] [] = True
igualArray [] l = False
igualArray l [] = False
igualArray (x:xs) (y:ys) | (x == y) = igualArray xs ys
                         | otherwise = False 

-- TA DANDO ERRO
buscarElemento :: StateT -> Value -> Value -> StateTransformer Value
buscarElemento env (Array []) (Int n) = return Nil
buscarElemento env (Array (a:as)) (Int 0) = return a --- ANTES tava (Array a:as)
buscarElemento env (Array (a:as)) (Int n) = do --- ANTES tava (Array a:as)
    buscarElemento env (Array as) (Int (n-1))

--
-- Types and boilerplate
--

type StateT = [Map String Value]
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, []) = ""
showResult (val, (e:es)) = show val ++ "\n" ++ show (toList $ union e (Map.empty)) ++ "\n" ++ showResult (val, es)
{--showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union defs environment) ++ "\n"--}

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f [Map.empty]

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
