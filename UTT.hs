module UTT
  ( UTT
  , typ, var, als, aps, tpu, tmu
  , normalize, check )
where


import Prelude as P
import Data.Map.Strict as M
import Data.Sequence as S


type Nam = String
type Lvl = Int
type Ind = Int

data UTT
  = Typ Lvl
  | Var Ind Nam
  | All (Nam, UTT) UTT
  | App UTT UTT
  | Clo Env (Nam, UTT) UTT
  | TpC Nam
  | TmC Nam
  deriving (Show, Eq)

type Env = Map Nam (Seq UTT)


typ :: Lvl -> UTT
typ = Typ

var :: Nam -> UTT
var = Var 0

als :: [(Nam, UTT)] -> UTT -> UTT
als = flip (P.foldr All) 

aps :: UTT -> [UTT] -> UTT
aps = P.foldl App

tpu :: UTT
tpu = TpC "Uni"

tmu :: UTT
tmu = TmC "uni"


data Result a
  = None
  | Exact a
  | Beyond

search :: Env -> Nam -> Ind -> Result UTT
search env nam ind = case M.lookup nam env of
  Just seq | ind < S.length seq -> Exact (S.index seq ind)
           | otherwise          -> Beyond
  Nothing  -> None

extend :: Env -> Nam -> UTT -> Env
extend env nam utt
  | M.null env = M.singleton nam (S.singleton utt)
  | otherwise  = M.insertWith (S.><) nam (S.singleton utt) env

norm :: Env -> UTT -> UTT
norm env utt = case utt of
  Var ind nam -> case search env nam ind of
    None      -> utt
    Exact utt -> utt  
    Beyond    -> Var (ind - 1) nam
  All bnd bod -> Clo env bnd bod
  App opr opd -> case norm env opr of
    Clo sen bnd@(nam, _) bod -> norm (extend sen nam $ norm env opd) bod
    wnf                      -> App wnf (norm env opd)
  _           -> utt

shift :: Nam -> Lvl -> UTT -> UTT
shift nic lvl utt = case utt of
  Var ind nam | nam /= nic -> utt
              | ind < lvl  -> utt
              | otherwise  -> Var (ind + 1) nam
  All bnd@(nam, _) bod     -> All bnd     $ shift nic (if nic == nam then lvl + 1 else lvl) bod
  Clo env bnd@(nam, _) bod -> Clo env bnd $ shift nic (if nic == nam then lvl + 1 else lvl) bod
  App opr opd              -> App (shift nic lvl opr) (shift nic lvl opd)

form :: UTT -> UTT
form utt = case utt of
  Clo env bnd@(nam, _) bod ->
    let len = extend (M.map (fmap $ shift nam 0) env) nam (var nam)
     in All bnd $ form (norm len bod)
  App opr opd -> App (form opr) (form opd)
  _           -> utt

normalize :: UTT -> UTT
normalize = form . norm M.empty


type Sig = Map Nam UTT

sig :: Sig
sig = M.fromList [("Uni", Typ 0), ("uni", TpC "Uni")]

type Ctx = Map Nam UTT

check :: Ctx -> UTT -> Maybe UTT
check ctx utt = case utt of
  Typ lvl   -> Just $ Typ (lvl + 1)
  Var _ nam -> M.lookup nam ctx
  All bnd@(nam, ptp) bod -> do
    _ <- check ctx ptp
    let ptp' = normalize ptp
        ctx' = M.insert nam ptp' ctx
    btp <- check ctx' bod
    _ <- check ctx' btp
    return $ All (nam, ptp') (normalize btp)
  App opr opd -> do
    ftp@(All (nam, ptp) btp) <- check ctx opr
    atp <- check ctx opd
    if normalize atp == normalize ptp
       then return $ normalize (App ftp opd)
       else Nothing
  TpC nam -> M.lookup nam sig
  TmC nam -> M.lookup nam sig

