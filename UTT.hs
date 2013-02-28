module UTT
  ( UTT
  , typ, var, als, aps
  , normalize, check )
where


import Prelude as P
import Data.Map.Strict as M
import Data.Sequence as S


type Nom = String
type Lvl = Int
type Ind = Int

data UTT
  = Typ Lvl
  | Var Ind Nom
  | All (Nom, UTT) UTT
  | App UTT UTT
  | Clo Env (Nom, UTT) UTT
  | TpC Nom
  | TmC Nom
  deriving (Show, Eq)

typ :: Lvl -> UTT
typ = Typ

var :: Nom -> UTT
var = Var 0

als :: [(Nom, UTT)] -> UTT -> UTT
als = flip (P.foldr All) 

aps :: UTT -> [UTT] -> UTT
aps = P.foldl App

type Env = Map Nom (Seq UTT)

data Result a
  = None
  | Exact a
  | Beyond


search :: Env -> Nom -> Ind -> Result UTT
search env nom ind = case M.lookup nom env of
  Just seq | ind < S.length seq -> Exact (S.index seq ind)
           | otherwise          -> Beyond
  Nothing  -> None

extend :: Env -> Nom -> UTT -> Env
extend env nom utt
  | M.null env = M.singleton nom (S.singleton utt)
  | otherwise  = M.insertWith (S.><) nom (S.singleton utt) env

norm :: Env -> UTT -> UTT
norm env utt = case utt of
  Var ind nom -> case search env nom ind of
    None      -> utt
    Exact utt -> utt  
    Beyond    -> Var (ind - 1) nom
  All bnd bod -> Clo env bnd bod
  App opr opd -> case norm env opr of
    Clo sen bnd@(nom, _) bod -> norm (extend sen nom $ norm env opd) bod
    wnf                      -> App wnf (norm env opd)
  _           -> utt

shift :: Nom -> Lvl -> Int -> UTT -> UTT
shift nam lvl stp utt = case utt of
  Var ind nom | nom /= nam -> utt
              | ind < lvl  -> utt
              | otherwise  -> Var nom (ind + stp)
  All bnd@(nom, _) bod     -> All bnd     $ shift nam (if nam == nom then lvl + 1 else lvl) stp bod
  Clo env bnd@(nom, _) bod -> Clo env bnd $ shift nam (if nam == nom then lvl + 1 else lvl) stp bod
  App opr opd              -> App (shift nam lvl stp opr) (shift nam lvl stp opd)

form :: UTT -> UTT
form utt = case utt of
  Clo env bnd@(nom, _) bod ->
    let len = extend (M.map (fmap $ shift nom 0 1) env) nom (var nom)
     in All bnd $ form (norm len bod)
  App opr opd -> App (form opr) (form opd)
  _           -> utt

normalize :: UTT -> UTT
normalize = form . norm M.empty


type Sig = Map Nom UTT

sig :: Sig
sig = M.fromList [("Uni", Typ 0), ("uni", TpC "Uni")]

type Ctx = Map Nom UTT

check :: Ctx -> UTT -> Maybe UTT
check ctx utt = case utt of
  Typ lvl   -> Just $ Typ (lvl + 1)
  Var _ nom -> M.lookup nom ctx
  All bnd@(nom, ptp) bod -> do
    Typ m <- check ctx ptp
    let ptp' = normalize ptp
        ctx' = M.insert nom npt ctx
    btp <- check ctx' bod
    case btp of
      Typ n -> return $ Typ (max m n)
      _     -> do Typ _ <- check ctx' btp
                  return $ All (nom, ptp') (normalize btp)
  App opr opd -> do
    ftp@(All (nom, ptp) btp) <- check ctx opr
    atp <- check ctx opd
    if normalize atp == normalize ptp
       then return $ normalize (App ftp opd)
       else Nothing
  TpC nom -> M.lookup nom sig
  TmC nom -> M.lookup nom sig

