{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}
import Data.List
import Data.Bits 
import Control.Applicative
import Data.Word
import Control.Monad 


data BitTerm a where 
        BinOp :: String -> (a -> a -> a) -> BitTerm a -> BitTerm a ->  BitTerm a 
        MonOp :: String -> (a -> a) -> BitTerm a -> BitTerm a 
        Var :: a -> BitTerm a 

instance Show a => Show (BitTerm a) where 
        show (BinOp p _ a b) = "(" ++ show a ++ " " ++ p ++ " " ++ show b ++ ")"
        show (MonOp p _ a) = "(" ++ p  ++ " " ++ show a ++ ")"
        show (Var a) = show a



getCoDomain :: Eq a => [BitTerm a] -> [a]
getCoDomain xs = nub $ eval  <$> xs

eval (MonOp _ f a) = f (eval a)
eval (BinOp _ f a b) = f (eval a) (eval b)
eval (Var a) = a  


shiftr a = MonOp ("`shiftR`" ++ show a ) (`shiftR` a)
shiftl a = MonOp ("`shiftL`" ++ show a) (`shiftL` a)
rotr a = MonOp ("`rotateR`" ++ show a)  (`rotateR` a)
rotl a = MonOp ("`rotateR`" ++ show a) (`rotateL` a)
add = BinOp ("+")(+)
sub = BinOp ("-") (-)
mul = BinOp ("*") (*)
mods = BinOp "`mod`" mod 
ors = BinOp "|" (.|.)
ands = BinOp "&" (.&.)
xors = BinOp "`xor`" (xor)
oid = MonOp "id" id 

binops = [add, sub, mul, mods, ors, ands, xors]
shiftops  bs = bs >>= (\n -> [shiftr n, shiftl n, rotl n, rotr n, oid])

experiment x = [ (Var 0x20) `bopa` ((sop x) `bopb` (((sopb x) `bopc` x) `mods` (Var $ 0x7E - 0x20))) 
               | bopa <- binops, bopb <- binops, bopc <- binops, sop <- shiftops [1..31], sopb <- shiftops [1..31]
        ]


tests :: [Word8] -> [[(BitTerm Word8)]]
tests range = forM range $ \i -> do  
                 t <- experiment (Var i) 
                 return t

findTest xs = getCoDomain xs 
