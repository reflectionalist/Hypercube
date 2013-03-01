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
  | TpC Nam
  | TmC Nam
  deriving (Show, Eq)


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


normalize :: UTT -> UTT
normalize utt = case utt of
  Var _ _     -> utt
  All bnd bod -> All bnd (normalize bod)
  App opr opd -> case normalize opr of
    All (nam, _) bod -> normalize $ substitute nam 0 (normalize opd) bod
    nf               -> App nf (normalize opd)
  _           -> utt

-- capture-free substitution
substitute :: Nam -> Lvl -> UTT -> UTT -> UTT
substitute nic lvl sub utt = case utt of
  Var ind nam | nam /= nic -> utt
              | ind < lvl  -> utt
              | ind > lvl  -> Var (ind - 1) nam
              | otherwise  -> sub
  All bnd@(nam, _) bod
    | nam /= nic -> All bnd $ substitute nic lvl (shift nam 0 sub) bod
    | otherwise  -> All bnd $ substitute nic (lvl + 1) (shift nam lvl sub) bod
  App opr opd    -> App (substitute nic lvl sub opr) (substitute nic lvl sub opd)
  _              -> utt

-- shift named-index by 1
shift :: Nam -> Lvl -> UTT -> UTT
shift nic lvl utt = case utt of
  Var ind nam | nam /= nic -> utt
              | ind < lvl  -> utt
              | otherwise  -> Var (ind + 1) nam
  All bnd@(nam, _) bod -> All bnd $ shift nic (if nam == nic then lvl + 1 else lvl) bod
  App opr opd          -> App (shift nic lvl opr) (shift nic lvl opd)
  _                    -> utt


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

