module UTT
  ( UTT
  , typ, var, all, als, app, aps, ann, nat, nil, suc, bin, bis
  , normalize, check, infer, chk, inf )
where


import Prelude as P hiding (all)
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
  | Ann UTT UTT
  | TpC Nam
  | TmC Nam
  | Clo Env (Nam, UTT) UTT
  deriving (Show, Eq)

type Env = Map Nam (Seq UTT)


typ :: Lvl -> UTT
typ = Typ

var :: Nam -> UTT
var = Var 0

all :: (Nam, UTT) -> UTT -> UTT
all = All

als :: [(Nam, UTT)] -> UTT -> UTT
als = flip (P.foldr all)

app :: UTT -> UTT -> UTT
app = App

aps :: UTT -> [UTT] -> UTT
aps = P.foldl app

ann :: UTT -> UTT -> UTT
ann = Ann

nat :: UTT
nat = TpC "Nat"

nil :: UTT
nil = TmC "Nil"

suc :: UTT
suc = TmC "Suc"

bin :: (Nam, UTT, UTT) -> UTT -> UTT
bin (nam, dtp, dtm) bod = App (All (nam, dtp) bod) dtm

bis :: [(Nam, UTT, UTT)] -> UTT -> UTT
bis = flip (P.foldr bin)


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
    Clo sen (nam, _) bod -> norm (extend sen nam $ norm env opd) bod
    wnf                  -> App wnf (norm env opd)
  Ann utm utp -> Ann (norm env utm) (norm env utp)
  _           -> utt

shift :: Nam -> Lvl -> UTT -> UTT
shift nic lvl utt = case utt of
  Var ind nam | nam /= nic -> utt
              | ind < lvl  -> utt
              | otherwise  -> Var (ind + 1) nam
  All bnd@(nam, _) bod     -> All bnd     (shb nic nam lvl bod)
  Clo env bnd@(nam, _) bod -> Clo env bnd (shb nic nam lvl bod)
  App opr opd -> App (shift nic lvl opr) (shift nic lvl opd)
  Ann utm utp -> Ann (shift nic lvl utm) (shift nic lvl utp)
  _           -> utt
  where shb nic nam lvl bod = shift nic (if nic == nam then lvl + 1 else lvl) bod

form :: UTT -> UTT
form utt = case utt of
  Clo env bnd@(nam, _) bod -> All bnd (fb env nam bod)
  App opr opd -> App (form opr) (form opd)
  Ann utm utp -> Ann (form utm) (form utp)
  _           -> utt
  where fb env nam bod =
          let len = extend (M.map (fmap $ shift nam 0) env) nam (var nam)
           in form (norm len bod)

normalize :: UTT -> UTT
normalize = form . norm M.empty


type Sig = Map Nam UTT

sig :: Sig
sig = M.fromList [("Nat", Typ 0), ("Nil", TpC "Nat"), ("Suc", All ("_", TpC "Nat") (TpC "Nat"))]

type Ctx = Map Nam UTT

equtt :: UTT -> UTT -> Bool
equtt = (==)

usort :: Ctx -> UTT -> Maybe UTT
usort ctx utt = su 0
  where su l = case check ctx utt (Typ l) of
          Nothing -> su (l + 1)
          utp     -> utp

check :: Ctx -> UTT -> UTT -> Maybe UTT
check ctx utm utp = case utm of
  All (nam, ptp) bod ->
    do usort ctx ptp
       let ptp' = normalize ptp
           ctx' = M.insert nam ptp' ctx
       case utp of
         All (nic, vtp) btp
           | equtt (normalize vtp) ptp' ->
               do usort ctx' btp
                  let btp' = normalize btp
                  check ctx' bod btp'
                  return $ All (nam, ptp') btp'
         Typ _ -> do check ctx' bod utp
                     return utp
  _ -> do rtp <- infer ctx utm
          if equtt rtp (normalize utp)
             then return rtp
             else Nothing

infer :: Ctx -> UTT -> Maybe UTT
infer ctx utm = case utm of
  Typ lvl   -> Just $ Typ (lvl + 1)
  Var _ nam -> M.lookup nam ctx
  All (nam, ptp) bod ->
    do usort ctx ptp
       let ptp' = normalize ptp
           ctx' = M.insert nam ptp' ctx
       btp <- infer ctx' bod
       let btp' = normalize btp
       check ctx' bod btp'
       return $ All (nam, ptp') btp'
  App opr opd ->
    do ftp@(All (nam, ptp) btp) <- infer ctx opr
       check ctx opd ptp
       return $ normalize (App ftp opd)
  Ann utm utp ->
    do check ctx utm utp
       return (normalize utp)
  TpC nam -> M.lookup nam sig
  TmC nam -> M.lookup nam sig

chk :: UTT -> UTT -> Maybe UTT
chk = check M.empty

inf :: UTT -> Maybe UTT
inf = infer M.empty

