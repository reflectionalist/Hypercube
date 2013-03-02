module UTT
  ( UTT
  , typ, var, als, aps, ann, tpu, tmu
  , normalize, check, infer, chk, inf )
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
  | Ann UTT UTT
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

ann :: UTT -> UTT -> UTT
ann = Ann

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
sig = M.fromList [("Uni", Typ 0), ("uni", TpC "Uni")]

type Ctx = Map Nam UTT

equtt :: UTT -> UTT -> Bool
equtt = (==)

check :: Ctx -> UTT -> UTT -> Maybe UTT
check ctx utm utp = case utm of
  All (nam, ptp) bod -> do
    Typ _ <- infer ctx ptp
    let ptp' = normalize ptp
        ctx' = M.insert nam ptp' ctx
    case utp of
      All (nic, vtp) btp
        | equtt (normalize vtp) ptp' ->
            do Typ _ <- infer ctx' btp
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
  All bnd@(nam, ptp) bod -> do
    Typ m <- infer ctx ptp
    let ptp' = normalize ptp
        ctx' = M.insert nam ptp' ctx
    Typ n <- infer ctx' bod
    return $ Typ (max m n)
  App opr opd -> do
    ftp@(All (nam, ptp) btp) <- infer ctx opr
    check ctx opd ptp
    return $ normalize (App ftp opd)
  Ann utm utp -> do
    check ctx utm utp
    return (normalize utp)
  TpC nam -> M.lookup nam sig
  TmC nam -> M.lookup nam sig

chk :: UTT -> UTT -> Maybe UTT
chk = check M.empty

inf :: UTT -> Maybe UTT
inf = infer M.empty

