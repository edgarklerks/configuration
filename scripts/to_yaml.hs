{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, DeriveGeneric #-}

import Language.Bash.Parse
import Language.Bash.Syntax
import Language.Bash.Word
import qualified Language.Bash.Cond as C
import System.Environment
import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.Map as S
import Data.List
import Data.Typeable
import GHC.Generics



deriving instance Generic ListTerm
deriving instance Typeable ListTerm
deriving instance Generic Parameter
deriving instance Typeable Parameter
deriving instance Generic RValue
deriving instance Typeable RValue
deriving instance Generic AssignOp
deriving instance Typeable AssignOp
deriving instance Generic CaseTerm
deriving instance Typeable CaseTerm
deriving instance Generic ProcessSubstOp
deriving instance Typeable ProcessSubstOp
deriving instance Generic ParamSubst
deriving instance Typeable ParamSubst
deriving instance Typeable Statement
deriving instance Generic Statement
deriving instance Typeable C.CondExpr
deriving instance Generic (C.CondExpr a)
deriving instance Generic C.UnaryOp
deriving instance Typeable C.UnaryOp
deriving instance Generic C.BinaryOp
deriving instance Typeable C.BinaryOp
deriving instance Typeable AndOr
deriving instance Generic AndOr
deriving instance Typeable RedirOp
deriving instance Generic RedirOp
deriving instance Typeable HeredocOp
deriving instance Generic HeredocOp
deriving instance Typeable IODesc
deriving instance Generic IODesc
deriving instance Typeable Redir
deriving instance Generic Redir
deriving instance Typeable Command
deriving instance Generic Command
deriving instance Typeable ShellCommand
deriving instance Generic ShellCommand
deriving instance Typeable Assign
deriving instance Generic Assign
deriving instance Typeable CaseClause
deriving instance Generic CaseClause
deriving instance Typeable Span
deriving instance Generic Span
deriving instance Typeable List
deriving instance Generic List



main :: IO ()
main = do
     [p] <- getArgs
     xs <- readFile p
     case parse p xs of
      Right stm -> print $ execState ( pass1 stm)  S.empty
      Left e -> error (show e)


data Sym = Sym {
           par :: Parameter,
           assignop :: AssignOp,
           rvalue :: RValue
                } deriving Show

type Symtab = S.Map String [Sym]


putSym :: Assign -> State Symtab ()
putSym (Assign  p o r) = modify upsert
       where upsert :: Symtab -> Symtab
             upsert sm = case p of
                          (Parameter x _) -> S.alter (\x -> case x of
                                                              Nothing -> Just [Sym p o r]
                                                              Just m -> Just (Sym p o r : m)
                                                     ) x sm


pass2 :: Symtab -> List -> List
pass2 smt l = undefined

pass1 :: List ->  State Symtab List
pass1 (List lst) = List <$> mapM pass1' lst
          where pass1' :: Statement -> State Symtab Statement
                pass1' (Statement p t) = Statement <$> pass1AndOr p <*> pure t


pass1Pipeline :: Pipeline -> State Symtab Pipeline
pass1Pipeline (Pipeline tm tp iv cmds) = Pipeline tm tp iv <$> pass1Cmds cmds

pass1Cmds :: [Command] -> State Symtab [Command]
pass1Cmds = mapM pass1Command


pass1Command :: Command -> State Symtab Command
pass1Command (Command shcmd redirs) = case shcmd of
                                           SimpleCommand asgn wrds -> do
                                             mapM_ putSym asgn
                                             return $  Command ( SimpleCommand asgn wrds) redirs
                                           AssignBuiltin word xs -> do
                                             forM_ xs $ \i -> do
                                                                  case i of
                                                                    Left as -> putSym as
                                                                    Right _ -> return ()
                                             return $ Command ( AssignBuiltin word xs) redirs
                                           xs -> return (Command xs redirs)




pass1AndOr :: AndOr -> State Symtab AndOr
pass1AndOr (Last p) = Last <$> pass1Pipeline p
pass1AndOr (And p anor) = And <$> pass1Pipeline p <*> pass1AndOr anor
pass1AndOr (Or p anor) = Or <$> pass1Pipeline p <*> pass1AndOr anor
