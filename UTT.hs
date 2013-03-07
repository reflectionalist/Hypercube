{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}


module UTT where


import Prelude as P hiding (all)
import Data.Map.Strict as M
import Data.Sequence as S


type Nam = String
type Lvl = Int
type Ind = Int

data Atm
data Can
data Raw

data Chk
data Inf

data UTT c d where
  Typ :: Lvl -> UTT Can Inf
  Var :: Ind -> Nam -> UTT c Inf
  All :: (Nam, UTT c1 Chk) -> UTT c2 Chk -> UTT c Chk
  App :: UTT c1 Inf -> UTT c2 Chk -> UTT c Inf
  Ann :: UTT c1 d1 -> UTT c2 d2 -> UTT Raw Inf
  Let :: (Nam, UTT c1 Chk) -> UTT c2 Chk -> UTT c3 Inf -> UTT c Inf
  Met :: UTT c Inf -> UTT c Chk

type Env c d = Map Nam (Seq (UTT c d))


data Result r
  = None
  | Exact r
  | Beyond

search :: Env c d -> Nam -> Ind -> Result (UTT c d)
search env nam ind = case M.lookup nam env of
  Just seq | ind < S.length seq -> Exact (S.index seq ind)
           | otherwise          -> Beyond
  Nothing  -> None

extend :: Env c d -> Nam -> UTT c d -> Env c d
extend env nam utt
  | M.null env = M.singleton nam (S.singleton utt)
  | otherwise  = M.insertWith (S.><) nam (S.singleton utt) env

