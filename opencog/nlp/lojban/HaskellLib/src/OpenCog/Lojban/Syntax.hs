{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RelaxedPolyRec             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}

module OpenCog.Lojban.Syntax where

import Prelude hiding (id,(.),(<*>),(<$>),(*>),(<*))

import qualified Data.List.Split as S
import Data.List (nub,partition)
import qualified Data.Foldable as F (find)
import Data.Maybe (fromJust)
import Data.List (isInfixOf)
import Data.Hashable

import System.Random

import Control.Applicative hiding (many,some,optional)
import Control.Monad.RWS
import Control.Monad.Trans.Class
import Control.Category (id,(.),(<<<),(>>>))

import Iso hiding (SynIso,Syntax)

import OpenCog.AtomSpace (Atom(..),TruthVal(..),noTv,stv,atomFold,nodeName,atomElem)
import OpenCog.Lojban.Util

import OpenCog.Lojban.Syntax.Types
import OpenCog.Lojban.Syntax.Util
import OpenCog.Lojban.Syntax.AtomUtil

import Debug.Trace

mytrace a = traceShow a a
mytrace2 s a = trace (s ++(' ':show a)) a

--ReadMe:
--It is recommend to first take a look at Syntax.Types

--TODO Parser
-- FIX: pa + ki'o
--      da + quantifier
--      ZAhO explicit representation
--      me'au
--      tenseSumti
--      GOhA
--      vo'a rule see ../Lojban.hs
--      make statments explicit
--      ge'e cai on logical conectives and else?
--      Does SEI have implicit args?
--      What if you have SEI and UI

-- argslots
-- Synonims for tenses and gismu

-- ki
-- references

-- ConceptAnd/OrLink NotLink isntead of boolea and/or


--Sentences To Check
--gau ko ta akti

--TODO Printer
-- da,de,di differentiation

---------
--General
---------

_BO :: Syntax (Tagged Selbri)
_BO = toSelbri <<< anytense <&& sepSelmaho "BO"
    where toSelbri = mkIso f g where
              f (CN name) = ((highTv,cPN name lowTv),Nothing)
              g ((_,PN n),_) = cCN n lowTv

-------------------------------------------------------------------------------
--Sumti
-------------------------------------------------------------------------------

_LE :: Syntax String
_LE = selmaho "LE"

--Handles anykind of LE phrases
leP :: Syntax Atom
leP = (setWithSize ||| id) --Handle inner quantifier if it exists
      .
      reorder2
      .
      second (choice leHandlers .> (_ssl . handleBRIDI))
      .
      reorder1
      . (_LE
      &&& optional pa
      &&& tolist1 . pureVarNode
      &&& bridiBETail
      <&& optSelmaho "KU")
    where pureVarNode :: Syntax Sumti
          pureVarNode = insert (Node "VariableNode" "$var" noTv,Nothing)

          reorder1 = mkIso f g where
              f (le,(pa,(v,(s,a)))) = (pa,(le,(v,(s,a))))
              g (pa,(le,(v,(s,a)))) = (le,(pa,(v,(s,a))))

          reorder2 = mkIso f g where
              f (Just pa,a)   = Left (pa,a)
              f (Nothing,a)   = Right a
              g (Left (pa,a)) = (Just pa,a)
              g (Right a)     = (Nothing,a)


--Depending on the word we have a different Relation between the predicate
-- and the resulting Concept
leHandlers = [genInstance "IntensionalInheritanceLink" . rmfst "le"
             ,genInstance "InheritanceLink"            . rmfst "lo"
             ,massOf "IntensionalInheritanceLink"      . rmfst "lei"
             ,massOf "InheritanceLink"                 . rmfst "loi"
             ,setOf "IntensionalInheritanceLink"       . rmfst "le'i"
             ,setOf "InheritanceLink"                  . rmfst "lo'i"
             ,genInstance "IntensionalInheritanceLink" . rmfst "le'e"
             ,genInstance "InheritanceLink"            . rmfst "lo'e"
             ]

-- A Mass according to Lojban is a thing that has all properties of it's parts
massOf :: String -> SynIso Atom Atom
massOf itype = instanceOf . _ssl . _frames . addStuff . (implicationOf *** genInstance itype) . reorder
    where pp = Node "PredicateNode" "gunma" noTv
          arg1 = (Node "VariableNode" "$var" noTv,tag)
          tag = Nothing

          reorder = mkIso f g where
              f a     = (pp,a)
              g (_,m) = m

          addStuff = mkIso f g where
              f (p,a) = (((noTv,p),tag),[arg1,(a,tag)])
              g (((_,p),_),[_,(a,_)]) = (p,a)

fstToState :: Int -> SynIso ([Atom],a) a
fstToState i = Iso f g where
    f (as,a) =
        if length as == i
           then modify (\s -> s {sAtoms = as ++ sAtoms s}) >> pure a
           else lift $ Left "List has wrong lenght, is this intended?"
    g a = do
        allatoms <- gets sAtoms
        let (as,atoms) = splitAt i allatoms
        modify (\s -> s {sAtoms = atoms})
        pure (as,a)


sndToState :: Int -> SynIso (a,[Atom]) a
sndToState i = Iso f g where
    f (a,as) =
        if length as == i
           then modify (\s -> s {sAtoms = as ++ sAtoms s}) >> pure a
           else lift $ Left "List has wrong lenght, is this intended?"
    g a = do
        as <- popAtoms i
        pure (a,as)

setOf :: String -> SynIso Atom Atom
setOf itype = sndToState 1 . second (tolist1 . setTypeL . tolist2)
            . makeSet . genInstance itype
    where makeSet = Iso f g
              where f a = do
                        name <- randName (show a)
                        let set = cCN name noTv
                        pure (set,(set,a))
                    g (_,(_,a)) = pure a



--Handle anykind of LA phrase
laP :: Syntax Atom
laP = handleName . wordNode <<< sepSelmaho "LA"
                            &&> anyWord
                            <&& optSelmaho "KU"
    where handleName :: SynIso Atom Atom
          handleName = Iso f g where
              f a = do
                  p <- apply instanceOf (cPN "cmene" lowTv)
                  name <- randName (nodeName a ++ "___" ++ nodeName a)
                  let c = cCN name lowTv
                      ct = (c,Nothing)
                      at = (a,Nothing)
                      pt = ((highTv,p),Nothing)
                  l <- apply _frames (pt,[at,ct])
                  pushAtom l
                  pure c
              g _ = do
                  (EvalL _ _ (LL [a,_])) <- popAtom
                  pure a



liP :: Syntax Atom
liP = sepSelmaho "LI" &&> (xo <+> pa) <&& optSelmaho "LOhO"

xo :: Syntax Atom
xo = varnode <<< word "xo"

pa :: Syntax Atom
pa =  (         number       |||    concept   )
    . (showReadIso . paToNum |^| isoConcat " ")
    <<< some (selmaho "PA")
    where paToNum :: SynIso [String] Int
          paToNum = isoFoldl (digitsToNum . second paToDigit) . addfst 0

          digitsToNum :: SynIso (Int,Int) Int
          digitsToNum = Iso f g where
              f (i,j) = pure (i*10+j)
              g 0     = lift $ Left "Last Digit"
              g n     = pure (n `div` 10,n `mod` 10)

          paToDigit :: SynIso String Int
          paToDigit = mkSynonymIso [("no",0),("pa",1)
                                   ,("re",2),("ci",3)
                                   ,("vo",4),("mu",5)
                                   ,("xa",6),("ze",7)
                                   ,("bi",8),("so",9)]


zoP :: Syntax Atom
zoP = instanceOf . wordNode <<< mytext "zo" &&> anyWord

--KohaPharse for any kind of Pro-Noune
kohaP :: Syntax Atom
kohaP = da <+> ma <+> ko <+> koha
    where koha = concept . selmaho "KOhA"
          ma   = varnode . word "ma"
          da   = concept . oneOfS word ["da","de","di"]
          ko   = setFlagIso "ko" . concept . word "ko"

luP' :: Syntax Atom
luP' = sepSelmaho "LU" &&> lojban <&& optSelmaho "LIhU"

luP :: Syntax Atom
luP = instanceOf . luP'

sumti :: Syntax Atom
sumti = kohaP <+> leP <+> laP <+> liP <+> zoP <+> luP

--This Handles relative phrases
noiP :: Syntax Atom
noiP = sepSelmaho "NOI" &&> (hasKEhA <<< bridi) <&& optSelmaho "KUhO"
    where hasKEhA = Iso f f
          f a = if atomAny (\case { Link _ _ _ -> False
                                  ; (Node _ n _) ->"ke'a" `isInfixOf` n
                                  }) a
                   then pure a
                   else lift $ Left "No ke'a in bridi."
          kea = Node "ConceptNode" "ke'a" noTv

noiShort :: Syntax Atom
noiShort = sepSelmaho "NOI" &&> ptp selbriUI addKEhA bridi <&& optSelmaho "KUhO"
    where addKEhA = mkIso f g
          f = (++) "ke'a "
          g = drop 4

goiP :: Syntax Atom
goiP = ptp (selmaho "GOI") goiToNoi noiP
    where goiToNoi = mkSynonymIso [("pe "  ,"poi ke'a srana ")
                                  ,("po "  ,"poi ke'a se steci srana ")
                                  ,("po'e ","poi jinzi ke se steci srana ")
                                  ,("po'u ","poi ke'a du ")
                                  ,("ne "  ,"noi ke'a srana ")
                                  ,("no'u ","noi ke'a du ")
                                  ,("goi " ,"poi ke'a du ")]

sumtiNoi :: Syntax Atom
sumtiNoi = (kehaToSesku . sndToState 1 ||| id) . reorder
        <<< sumti &&& optional (noiP <+> noiShort <+> goiP)
    where reorder = mkIso f g
          f (a,Just n )    = Left (a,[n])
          f (a,Nothing)    = Right a
          g (Left (a,[n])) = (a,Just n )
          g (Right a)      = (a,Nothing)
          --If we have a relative clause
          --this will switch the ke'a with the actually concept
          --TODO: Do we always have a ke'a in a relative clause?
          kehaToSesku :: SynIso Atom Atom
          kehaToSesku = Iso f g
              where f c = do
                        atoms <- gets sAtoms
                        let nl = map (switch kea c) atoms
                        if nl /= atoms
                         then modify (\s -> s {sAtoms = nl}) >> pure c
                         else lift $ Left "No ke'a in relative clause."
                    g c = do
                        atoms <- gets sAtoms
                        let nl = map (switch c kea) atoms
                        if nl /= atoms
                         then modify (\s -> s {sAtoms = nl}) >> pure c
                         else lift $ Left "No ke'a in relative clause."
                    kea = Node "ConceptNode" "ke'a" noTv
                    switch a b l@(InhL [x,c] tv) = if c == a
                                                  then cInhL tv x b
                                                  else l
                    switch _ _ l = l

sumtiLaiP :: Syntax Atom
sumtiLaiP = ptp (pa &&& selbri) id (ptp pa addLE sumtiLai) <+> sumtiLai
    where addLE = mkIso f g
          f s = s ++ " lo "
          g s = take (length s - 4) s

sumtiLai :: Syntax Atom
sumtiLai = ((handleSubSet .> setWithSize) ||| maybeinof)
           . reorder1 <<< (optional pa &&& sumtiNoi)
    where reorder1 :: SynIso (Maybe Atom,Atom) (Either (Atom,(Atom,Atom)) Atom)
          reorder1 = Iso f g where
              f (Just pa,sumti) = do
                        atoms <- gets sAtoms
                        case findSetType sumti atoms of
                            Just t  -> pure $ Left (sumti,(pa,t    ))
                            Nothing -> pure $ Left (sumti,(pa,sumti))
              f (Nothing,sumti) = pure $ Right sumti
              g (Left (sumti,(pa,t))) = pure (Just pa,sumti)
              g (Right sumti)         = pure (Nothing,sumti)

          handleSubSet = sndToState 1 . second (tolist1 . subsetL) . reorder2

          maybeinof = Iso f g where
              f a = do
                  atoms <- gets sAtoms
                  case getDefinitions [a] atoms of
                      [] -> apply instanceOf a
                      _  -> pure a              --We have a lep which is already an instance
              g a = error "Check for lep somehow"

          reorder2 = mkIso f g where
              f (t,s)     = (s,(s,t))
              g (s,(_,t)) = (t,s)



setWithSize :: SynIso (Atom,Atom) Atom
setWithSize = sndToState 2 . second (tolist2 . (sizeL *** setTypeL)) . makeSet
    where makeSet = Iso f g
          f (a,b) = do
              name <- randName $ show a ++ show b
              let set = cCN name noTv
              pure (set,([set,a],[set,b]))
          g (_,([_,a],[_,b])) = pure (a,b)

--Connective

_A :: Syntax LCON
_A = optional (word "na") &&& selmaho "A" &&& optional (word "nai")

_A_BO :: Syntax Con
_A_BO = wrapA <<< _A &&& optional _BO
    where wrapA = mkIso f g
          f (a,b) = (Just a,b)
          g (Just a,b) = (a,b)

--AndLink
--  AndLink
--  EvaluationLink
--      PredicateNode "tense"
--      ListLink

--This Handels Sumti connected by logical connectives
sumtiC :: Syntax Atom
sumtiC = (handleCon . reorder ||| id) . ifJustB
        <<< sumtiLaiP &&& optional (_A_BO &&& sumtiC)
    where reorder = mkIso f g
          f (a1,(con,a2)) = (con,[a1,a2])
          g (con,[a1,a2]) = (a1,(con,a2))

--HandleCon Connectes the Atoms with 2 possible connectives
--Since booth connectives are in a Maybe we first lift the Atoms into the Maybe
--Then we fmap the isos for each Connective Type over the Maybes
--Finally we merge the results together or pick one
handleCon :: SynIso (Con,[Atom]) Atom
handleCon = merge . (mapIso conLink *** mapIso (_frames .> toSumti)) . reorder
    where reorder = mkIso f g where
              f ((s,ts),as)                 = (eM (s,as),eM (ts,as))
              g (Just (s,as) ,Just (ts,_))  = ((Just s,Just ts),as)
              g (Nothing     ,Just (ts,as)) = ((Nothing,Just ts),as)
              g (Just (s,as) ,Nothing)      = ((Just s,Nothing),as)
              eM (Just a,b)  = Just (a,b) --expand Maybe
              eM (Nothing,b) = Nothing

          toSumti = mkIso f g where
              f = map (\x -> (x,Nothing))
              g = map fst

          merge = mkIso f g where
              f (Just a,Just b) = Link "AndLink" [a,b] highTv
              f (Nothing,Just b) = b
              f (Just a,Nothing) = a
              f (Nothing,Nothing) = error "not allowed to happen."
              g l@EvalL{} = (Nothing,Just l)
              g (AL [a,b@EvalL{}]) = (Just a,Just b)
              g l = (Just l,Nothing)

--For parsing placeholder like "x1" as a sumti
--The 1 will be used as the place tag
placeholder :: Syntax Sumti
placeholder = first instanceOf . handle
           <<< letter &&& digit <&& sepSpace
    where handle = mkIso f g
          f (c,d)               = (cCN [c,d] noTv,Just [d])
          g (CN [c,_],Just [d]) = (c,d)

--Finds the place tag FA if it exists and turns it into a number
sumtiT :: Syntax Sumti
sumtiT = (handleFA <<< optional _FA &&& sumtiC) <+> placeholder
    where handleFA :: SynIso (Maybe String,Atom) Sumti
          handleFA = commute . first (mapIso faToPlace)

          _FA = selmaho "FA"

          faToPlace :: SynIso String String
          faToPlace = mkSynonymIso [("fa","1")
                                   ,("fe","2")
                                   ,("fi","3")
                                   ,("fo","4")
                                   ,("fu","5")
                                   ,("fi'a","?")]

--New Places Can be added from other Predicates
--fihoP <+> baiP figures out the name
--SumitT the actually sumti that will be put in this place
--Handle fi'o creates an Eval link that applies the sumti
--to the fi'o/bai Predicate
modalSumti :: Syntax Sumti
modalSumti = handleFIhO <<< (fihoP <+> baiP) &&& sumtiT
    where handleFIhO :: SynIso (Tagged Selbri,Sumti) Sumti
          handleFIhO = sndToState 1 . (fi'otag &&& tolist1 . _frame)
                     . second (inverse tolist1) . handleTAG . second tolist1

          fi'otag :: SynIso (Selbri,(Atom,Tag)) Sumti
          fi'otag = mkIso f g where
              f ((_,PN name),(s,tag)) = (s,Just $ name++tag)
              g (s,Just nametag) =
                  let [name,tag,tv] = S.split (S.oneOf "12345") nametag
                  in ((read tv,cPN name lowTv),(s,tag))

          fihoP :: Syntax (Tagged Selbri)
          fihoP = sepSelmaho "FIhO" &&> selbriUI <&& optSelmaho "FEhU"

--bai are shorthands for fi'o selbri
--in case we find one we transfrom it to the full version
baiP :: Syntax (Tagged Selbri)
baiP = ptp _bai iso selbriUI
    where iso = Iso f g where
            f a = do
                btf <- asks bai
                apply btf a
            g b = do
                btf <- asks bai
                unapply btf b
          _bai = optional (selmaho "SE") &&& selmaho "BAI"

--Another kind of new place for a Predicate
--This time based on a tense
tenseSumti :: Syntax Sumti
tenseSumti = reorder <<< anytense &&& sumtiC <&& optSelmaho "KU"
    where reorder = mkIso f g
          f (t,s) = (s,Just $ nodeName t)
          g (s,Just n) = (cCN n noTv,s)

--Somtimes the tense is not followed by a sumti
--in this case we can just assume a ti
--but only if the tense is not followed by a predicate
--as that would just set the sentence tense
tenseSumtiP :: Syntax Sumti
tenseSumtiP = tenseSumti <+> ptp (anytense <&& notsyn selbriAll) addti tenseSumti

--Fails when the Syntax Succeds and the other way arround
--Either the syn succeds then we fail with the zeroArrow
--Or the right . insertc succeds because syn failed then we do nothing
notsyn :: Syntax a -> Syntax ()
notsyn x = (zeroArrow ||| id) . ((left <<< lookahead x) <+> (right . insert ()))

sumtiAll :: Syntax Sumti
sumtiAll = filterState <<< modalSumti <+> sumtiT <+> tenseSumtiP

sumtiAllUI :: Syntax Sumti
sumtiAllUI = withAttitude sumtiAll

-------------------------------------------------------------------------------
--Selbri
-------------------------------------------------------------------------------

gismuP :: Syntax Atom
gismuP = implicationOf . predicate . gismu


tanru :: Syntax Atom
tanru = isoFoldl handleTanru . (inverse cons) <<< some (gismuP <+> nuP)
  where handleTanru = sndToState 1 . second (tolist1 . _iimpl) . reorder
        reorder = mkIso f g
        f (g,t) = (t,(t,g))
        g(t,(_,g)) = (g,t)




--meP :: Syntax (State Atom)
--meP =  handleME <$> selmaho "ME" <*> sumtiAll <*> optSelmaho "MEhU"


nuP :: Syntax Atom
nuP = withEmptyState $ choice nuHandlers . (selmaho "NU" &&& bridiUI <&& optSelmaho "KEI")

nuHandlers = [handleNU "du'u" (mkNuEventLabel "du'u") . rmfst "du'u",
              handleNU "su'u" (mkNuEventLabel "su'u") . rmfst "su'u",
              handleNU "nu"   (mkNuEvent ["fasnu"]) . rmfst "nu",
              handleNU "mu'e" (mkNuEvent ["fasnu", "mokca"]) . rmfst "mu'e",
              handleNU "zu'o" (mkNuEvent ["zumfau"]) . rmfst "zu'o",
              handleNU "za'i" (mkNuEvent ["tcini"]) . rmfst "za'i",
              handleNU "ka"   (mkNuEvent ["ckaji"]) . rmfst "ka",
              handleNU "ni"   (mkNuEvent ["klani"]) . rmfst "ni",
              handleNU "si'o" (mkNuEvent ["sidbo"]) . rmfst "si'o",
              handleNU "li'i" (mkNuEventLabel "is_experience") . rmfst "li'i",
              handleNU "pu'u" (mkNuEventLabel "is_point_event") . rmfst "pu'u",
              handleNU "jei"  (mkNuEventLabel "is_truth_value") . rmfst "jei"]
  where
    -- Functions that specify the type of the abstraction
    -- Note: atomNub may leave AndLink with only one atom
    mkNuEventLabel :: String -> String -> SynIso [Atom] [Atom]
    mkNuEventLabel eventName _ = cons . first (mkEval . atomNub . mkEvent) . reorder
      where mkEval = _eval . addfst (cPN eventName highTv) . tolist2 . addfstAny (cCN "$2" highTv)
    mkNuEvent :: [String] -> String -> SynIso [Atom] [Atom]
    mkNuEvent (nuType:nts) name = isoConcat2 . tolist2 . addfstAny nuImps
                                          . cons . first wrapNuVars . reorder
     where
       nuPred = cPN (nuType ++ "_" ++ name) highTv
       nuImps = (cImpL highTv nuPred (cPN nuType highTv))
            :(case nts of
                [] -> []
                [nts] -> let nuSec = (cPN (nts ++ "_" ++ name) highTv)
                         in [(cIImpL highTv nuPred nuSec), (cImpL highTv nuSec (cPN nts highTv))])
       wrapNuVars = andl . tolist2 . first (mkEval "1") . second (mkEval "2")
                         . addfstAny (cCN "$2" highTv) . atomNub . mkEvent
       mkEval num = _evalTv . addfstAny highTv . addfstAny (cPN (nuType ++ "_sumti" ++ num) highTv)
                            . tolist2 . addfstAny nuPred
    mkEvent = atomIsoMap (mkIso f id) where
     f (EvalL _ (PN _) (LL [vn@(VN _), _])) = vn -- can be PN:CN, PN:WN, anything else?
     f a = a
    reorder = mkIso f g where
     f (a:as) = (a, a:as)
     g (_,as) = as

handleNU :: String -> (String -> SynIso [Atom] [Atom]) -> SynIso Atom Atom
handleNU abstractor nuTypeMarker = Iso f g where
  f atom = do
    state <- gets sAtoms
    rname <- randName $ (show atom)
    let name = rname ++ "___" ++ abstractor
        pred = cPN name highTv
    link <- apply (mkLink pred name) (atom, state)
    setAtoms [link]
    pure pred
  g (pred@(PN name)) = do
    state <- gets sAtoms -- FIX: Have to get correct atom
    let link = F.find (atomElem pred) state
    (atom, nuState) <- case link of -- remove "is_event" atoms
        Just l -> unapply (mkLink pred name) l
        _ -> lift $ Left $ (show pred) ++ " can't be found in state."
    pushAtoms nuState -- : instantiate VNs?
    pure atom -- should only be one. Check?
  mkLink :: Atom -> String -> SynIso (Atom, [Atom]) Atom
  mkLink pred name = mkLink' . addfstAny pred
                    . second (nuTypeMarker name)
                    . mkNuState . getPredVars
  -- Extract predicateNodes from the atom and state
  getPredVars :: SynIso (Atom, [Atom]) (([Atom], [Atom]), [Atom])
  getPredVars = mkIso f g where
    f (atom, state) =
      let predicateNodes =
            nub $ (atomFold (\ns a -> case a of (EvalL _ _ (LL (pn@(PN _):_))) -> pn:ns
                                                a -> ns) [] atom)
                  ++ (foldl (\ns a -> case a of (ImpL [pn@(PN _), (PN _)] _) -> pn:ns
                                                (InhL [pn@(PN _), (PN _)] _) -> pn:ns
                                                a -> ns) [] state)
          predicateVars = map (cVN.("$"++).show) [3..(length predicateNodes) + 2]
      in ((predicateVars,predicateNodes), atom:state)
    g (_, atom:state) = (atom, state) -- FIX, can't assume first atom is the atom
  mkNuState :: SynIso (([Atom], [Atom]), [Atom]) ([Atom], [Atom])
  mkNuState = Iso f g where
    f ((predicateVars, predicateNodes), astate) = do
      nuState <- apply (mapIso (replacePredicatesIso (zip predicateNodes predicateVars))) astate
      pure (predicateVars, nuState)
    g (predicateVars, nuState) = pure ((predicateVars, predicateVars), nuState)
    replacePredicatesIso :: [(Atom, Atom)] -> SynIso Atom Atom
    replacePredicatesIso nodeVarMap =  atomIsoMap $ mkIso f g where
      f pn@(PN _) = case lookup pn nodeVarMap of
          Just vn -> vn
          Nothing -> pn
      f a = a
      g a = a -- i.e., don't instantiate vars for now
  -- (pred, (typedPredicateVars, eventAtom:state')
  mkLink' :: SynIso (Atom, ([Atom], [Atom])) Atom
  mkLink' = _equivl
           . first  (_evalTv . addfst highTv  . addsnd [cVN "$1"])
           . second
               (_meml . addfst (cVN "$1") . ssl . tolist2 . addfst (cVN "$2")
                 . _exl . first (varll . mapIso (_typedvarl . addsnd (cTN "PredicateNode")))
                        . second andl)


_MOI :: Syntax String
_MOI = selmaho "MOI"

moiP :: Syntax Atom
moiP = implicationOf . predicate . handleMOI
     <<< (pa &&& _MOI)
    where handleMOI = mkIso f g
          f (a,s) = nodeName a ++ '-':s
          g name  = let nn = takeWhile (/= '-') name
                        s  = drop 1 $ dropWhile (/= '-') name
                    in if isNumeric nn
                          then (Node "NumberNode"  nn noTv,s)
                          else (Node "ConceptNode" nn noTv,s)

_ME :: Syntax ()
_ME = mytext "me"

meP :: Syntax Atom
meP = implicationOf . predicate . handleME
     <<< (_ME &&& sumtiC)
    where handleME = mkIso f g
          f (s,a) = show a
          g name  = ((),read name)

_NAhE :: Syntax TruthVal
_NAhE = naheToTV <<< (selmaho "NAhE" <+> insert "")
    where naheToTV = mkSynonymIso [("je'a",stv 1    0.9)
                                  ,(""    ,stv 0.75 0.9)
                                  ,("no'e",stv 0.5  0.9)
                                  ,("na'e",stv 0.25 0.9)
                                  ,("to'e",stv 0    0.9)]


_MO :: Syntax Atom
_MO = varnode . word "mo"

_GOhA :: Syntax Atom
_GOhA = implicationOf . predicate <<< selmaho "GOhA"

gohaP = _MO <+> _GOhA

selbri :: Syntax (Tagged Atom)
selbri = filterState . commute  <<< optional (selmaho "SE")
                                &&& (tanru  <+> meP
                                            <+> moiP
                                            <+> gohaP)

--Selbris can be tagged with a strenght modifier
selbriP :: Syntax (Tagged Selbri)
selbriP = associate <<< _NAhE &&& selbri

selbriUI :: Syntax (Tagged Selbri)
selbriUI = (first (second handleSOS) ||| id) . reorder
        <<< (selbriP &&& optional _SOS)
    where reorder = mkIso f g
              where f (((tv,p),t),Just ui)    = Left  ((tv,(ui,p)),t)
                    f (p,Nothing)             = Right p
                    g (Left  ((tv,(ui,p)),t)) = (((tv,p),t),Just ui)
                    g (Right p)               = (p,Nothing)

_PU :: Syntax String
_PU = selmaho "PU"

_ZI :: Syntax String
_ZI = selmaho "ZI"

_VA :: Syntax String
_VA = selmaho "VA"

_VIhA :: Syntax String
_VIhA = selmaho "VIhA"

_FAhA :: Syntax String
_FAhA = selmaho "FAhA"

_ZAhO :: Syntax String
_ZAhO = selmaho "ZAhO"

_ZEhA :: Syntax String
_ZEhA = selmaho "ZEhA"

_CAhA :: Syntax String
_CAhA = selmaho "CAhA"

anytense :: Syntax Atom
anytense = concept <<< (_PU   <+> _ZI   <+> _VA <+> _FAhA <+> _ZAhO <+>
                        _ZEhA <+> _VIhA <+> _CAhA)

_trati :: Syntax (Maybe Atom)
_trati = handleTrati
       . many anytense
    where handleTrati = mkIso f g
          f [] = Nothing
          f [x]= Just x
          f xs = Just (Link "AndLink" xs noTv)
          g  Nothing       = []
          g (Just (AL xs)) = xs
          g (Just x)       = [x]

_NA :: Syntax String
_NA = selmaho "NA"

selbriAll :: Syntax (Maybe Atom, (Maybe String, Tagged Selbri))
selbriAll =  _trati
         &&& optional _NA
         &&& selbriUI

-------------------------------------------------------------------------------
--bacru
-------------------------------------------------------------------------------

--                            trati        na
bridiTail :: Syntax ((Maybe Atom, (Maybe String, Tagged Selbri)),[Sumti])
bridiTail = (((second.second.first.second) handleKO . selbriAll) &&& many sumtiAllUI) . ifNotFlag "onlyBE"

bridiBETail :: Syntax ((Maybe Atom, (Maybe String, Tagged Selbri)),[Sumti])
bridiBETail = ((second.second.first.second) handleKO . selbriAll) &&& beP
    where beP :: Syntax [Sumti]
          beP = (cons <<<       sepSelmaho "BE"  &&> sumtiAllUI
                      &&& many (sepSelmaho "BEI" &&> sumtiAllUI)
                      <&& optSelmaho "BEhO"
                ) <+> insert []

handleKO :: SynIso Atom Atom
handleKO = (sndToState 1 . second (tolist1 . impl) . reorder . ifFlag "ko") <+> id
    where reorder = mkIso f g where
            f selbri = (selbri,[selbri,cPN "Imperative" lowTv])
            g (selbri,_) = selbri

-- (a,(mp,(ma,(s,aa))))
-- (mp,(ma,(s,a++aa)))
-- ((mp,(ma,(s,a))),as)
-- (bridi,as)

_GIhA ::  Syntax LCON
_GIhA = optional (word "na") &&& _GIhA' &&& optional (word "nai")
    where _GIhA' = _GIhAtoA <<< selmaho "GIhA"

_GIhABO :: Syntax Con
_GIhABO = first just <<< _GIhA &&& optional _BO

--(s,(bt,(giha,bt)))
--((s,bt),[(giha,(s,bt))])

_bridi :: Syntax Atom
_bridi = isoFoldl handleGIhA . (handleBRIDI *** distribute) . reorder
        <<< (some sumtiAllUI <+> zohe)
        <&& optext "cu"
        &&& (bridiTail <+> bridiBETail) --FIXME: what if it is a mix
        &&& many (_GIhABO &&& bridiTail)

    where reorder = mkIso f g where
            f (s,(bt,ls)) = ((s,bt),(s,ls))
            g ((s,bt),(_,ls)) = (s,(bt,ls))

          zohe = tolist1 . insert (Node "ConceptNode" "zo'e" noTv,Nothing)

          distribute = mkIso f g where
            f (s,[]) = []
            f (s,(giha,bt):xs) = (giha,(s,bt)): f (s,xs)
            --g [] = Nothing -- let this fail with a bang so we notice
            g ((giha,(s,bt)):xs) = (s,(giha,bt):g' xs)
            g' [] = []
            g' ((giha,(s,bt)):xs) = (giha,bt):g' xs

--Connect a Atom (already converted bridi) with a Bridi
--the bridi get's turned into an Atom by handleBRIDI
--then both atoms get added to the Connective
handleGIhA :: SynIso (Atom,(Con,Bridi)) Atom
handleGIhA = handleCon . second (tolist2 .> handleBRIDI) . reorder
    where reorder = mkIso f g where
            f (a,(giha,br)) = (giha,(a,br))
            g (giha,(a,br)) = (a,(giha,br))

-- ((ma,(ms,ts)),[a])
-- (((ma,ms),ts),[a])
-- ((ma,ms),(ts,[a]))

--MPU = Maybe PU
--MNA = Maybe NA
handleBRIDI :: SynIso Bridi Atom
handleBRIDI = handleNA
            -- ((MNA,MCtxL)
            . second _ctx
            -- ((MNA,(MPU,frames))
            . inverse associate
            -- (((MNA,MPU),frames)
            . first commute
            -- (((MPU,MNA),frames)
            . second _frames
            -- ((MPU,MNA),(Tagged Selbri,sumti))
            . inverse associate
            -- (((MPU,MNA),Tagged Selbri),sumti)
            . first associate
            -- ((MPU,(MNA,Tagged Selbri)),sumti)
            . mergeSumti
            -- (sumit1,((MPU,(MNA,Tagged Selbri)),sumti2))

handleNA :: SynIso (Maybe String,Atom) Atom
handleNA = Iso f g where
    f (Nothing,a)    = pure a
    f (Just n, a)    = apply _eval (cGPN n lowTv,[a])
    g (EvalL _ (GPN n) a) = pure (Just n,a)
    g a                   = pure (Nothing,a)

--For mergin sumties before and after the selbri into a single list
mergeSumti :: (a ~ aa) => SynIso ([a],(s,[aa])) (s,[a])
mergeSumti = Iso f g where
    f ([],(_,[])) = lift $ Left "No Sumti to merge."
    f (a1,(s,a2)) = pure (s,a1++a2)
    g     (s,a)   = case a of
                       [] -> lift $ Left "No Sumti to reverse merge."
                       (x:xs) -> pure ([x],(s,xs))

_GA :: Syntax (Maybe LCON)
_GA = wrapGA . _GAtoA . selmaho "GA"
    where wrapGA = mkIso f g
          f a = Just (Nothing,(a,Nothing))
          g (Just (_,(a,_))) = a

bridiGA :: Syntax Atom
bridiGA = handleGA . (_GA &&& _bridi <&& sepSelmaho "GI" &&& _bridi)
    where handleGA = handleCon . (addsnd Nothing *** tolist2)

bridi = bridiGA <+> _bridi

--Deal with a bridi that has a Second Order Statment Attached
bridiUI :: Syntax Atom
bridiUI = (handleSOS ||| id) . ifJustA
       <<< (optional _SOS &&& bridi)

addti :: SynIso String String
statment :: Syntax Atom
statment = (handleSOS ||| id) . ifJustA
  <<< optSelmaho "I" &&> optional _SOS &&& statment2

statment2 :: Syntax Atom
statment2 = isoFoldl (handleCon . reorder)
    <<< bridi &&& many (sepSelmaho "I" &&> _JA_BO &&& bridiUI)
    where reorder = Iso f g where
            f (b1,(con,b2)) = pure (con,[b1,b2])
            g (con,[b1,b2]) = pure (b1,(con,b2))

addti = mkIso f g
    where f s = s ++ "ti "
          g s = s --TODO: Maybe remoe ti again

--Handles questions
--A SatisfactionLink is used for this
--The second with a VarNode in the Statement is a fill the blank question
--we wrap the statment in a (Put s (Get s)) that when excuted should fill the blank
preti :: Syntax Atom
preti = handleMa <<< handleXu
    where handleMa :: SynIso Atom Atom
          handleMa = Iso f g
          f a = do
              atoms <- gets sAtoms
              let x = atomFold (\r a -> r || isMa a) False a
                  isMa (Node "VariableNode" x noTv) = x /= "$var"
                  isMa _ = False
                  all = Link "ListLink" (a:atoms) noTv
                  na = Link "PutLink" [all,Link "GetLink" [all] noTv] noTv
              pure (x ? na $ all)
          g (Link "PutLink"  [LL (a:s),_] _) = setAtoms s >> pure a
          g (Link "ListLink" (a:s) _)        = setAtoms s >> pure a

          --If
          handleXu = Iso f g where
              f () = do
                  state0 <- get
                  res <- apply statment ()
                  state1 <- get
                  flags <- gets sFlags
                  if "xu" `elem` flags
                     then do
                         put state0
                         res2 <- apply (_exl . addfst var . withFlag "handleXu" statment) ()
                         put state1
                         case res2 of --If it's the whole sentence ingore
                             (ExL noTv (VN "xu")(VN "xu")) -> pure ()
                             _ -> pushAtom res2
                         apply _satl res
                    else pure res
              var = Node "VariableNode" "xu" noTv
              g a = unapply statment a



------------------------------------
--Free
-----------------------------------

_COI :: Syntax Atom
_COI = instanceOf . concept . selmaho "COI"

vocative :: Syntax Atom
vocative = listl . appendAtoms 2 . tolist2 <<< _COI &&& sumtiC
    where ui = ptp uiP addti (dropTag <<< sumtiAllUI)
          dropTag = mkIso f g where
              f (s,_) = s
              g s = (s,Nothing)


freeuiP :: Syntax Atom
freeuiP = reorder <<< _UI &&& caiP
    where reorder = mkIso f g
          f (a,tv) = cCN (nodeName a) tv
          g c@(Node "ConceptNode" name tv) = (c,tv)

--Sumti that stands free (not part of a statement)
--We can remove it's tag
--And pack it with the State into a list
freeSumti ::Syntax Atom
freeSumti = listl . consAtoms . rmsndAny Nothing . sumtiAllUI

free :: Syntax Atom
free = vocative <+> freeuiP <+> freeSumti <+> luP'

--jufra = listl <$> many1 (sepSelmaho "I" *> preti)

_JA ::  Syntax LCON
_JA = optional (word "na") &&& _JA' &&& optional (word "nai")
    where _JA' = _JAtoA <<< selmaho "JA"

_JA_BO :: Syntax Con
_JA_BO = handle <<< optional _JA &&& optional _BO
    where handle = Iso f g where
            f (Nothing,Nothing) = lift $ Left "_JA_BO empty"
            f a = pure a
            g = pure

{-_BAI_BO :: Syntax Atom
_BAI_BO = <$> baiP <* sepSelmaho "BO"
-}

selbriToEval :: SynIso Selbri Atom
selbriToEval = mkIso f g where
    f (tv,atom) = cEvalL tv atom (cLL [])
    g (EvalL tv p _) = (tv,p)

_jufra :: Syntax (Maybe Con,[Atom])
_jufra = second ((handle ||| tolist1) . ifJustB)
     <<< sepSelmaho "I" &&> optional _JA_BO &&& preti &&& optional _jufra
    where handle = Iso f g where
              f (p,(mc,a:as)) = case mc of
                                    Just c -> do
                                        x <- apply handleCon (c,[p,a])
                                        pure (x:as)
                                    Nothing -> pure $ p:a:as
              g (x:as) = (do
                  (c,[p,a]) <- unapply handleCon x
                  pure (p,(Just c, a : as))
                  ) <|> pure (x,(Nothing,as))

jufra :: Syntax Atom
jufra = listl <<< many preti

jufmei = listl . reorder <<< sepSelmaho "NIhO" &&> preti
                        &&& some (sepSelmaho "I" &&> preti)
    where reorder = mkIso f g where
            f (a,as) = a:as
            g (a:as) = (a,as)

lojban = finalCheck <<< jufmei <+> jufra <+> preti <+> free

finalCheck :: SynIso a a
finalCheck = Iso f g where
    f a = do
        text <- gets getText
        if text == ""
           then pure a
           else lift $ Left $ "Incomplete parse: " ++ text
    g = pure

------------------------------------
--Second Order Statments
-----------------------------------

--SEI

_SEI :: Syntax ()
_SEI = sepSelmaho "SEI"

_SEhU :: Syntax ()
_SEhU = optSelmaho "SEhU"

--SEIs are second order Statments that can appear almost anywhere
--This is also why they are only allowed to have BEtails
-- sei mi jimpe do gerku == sei mi jimpe se'u do gerku
seiP :: Syntax Atom
seiP = _SEI &&> withFlag "onlyBE" bridiUI <&& _SEhU

type SEI = Atom

--Attitude

_UI :: Syntax Atom
_UI = concept <<< (xu <+> selmaho "UI")
    where xu = setFlagIso "xu" <<< word "xu"

_CAI :: Syntax String
_CAI = selmaho "CAI"

_NAI :: Syntax String
_NAI = selmaho "NAI"

caiP :: Syntax TruthVal
caiP = naicaiToTV . isoConcat "|" . tolist2 <<< (_NAI <+> insert "")
                                            &&& (_CAI <+> insert "")

naicaiToTV :: SynIso String TruthVal
naicaiToTV = mkSynonymIso [("|cai"    ,stv 1     0.9)
                          ,("|sai"    ,stv 0.833 0.9)
                          ,("|"       ,stv 0.75  0.9)
                          ,("|ru'e"   ,stv 0.666 0.9)
                          ,("|cu'i"   ,stv 0.5   0.9)
                          ,("nai|ru'e",stv 0.333 0.9)
                          ,("nai|"    ,stv 0.250 0.9)
                          ,("nai|sai" ,stv 0.166 0.9)
                          ,("nai|cai" ,stv 0     0.9)]


uiP :: Syntax (Atom,TruthVal)
uiP = _UI &&& caiP

type UI = (Atom,TruthVal)

--TODO: What if you have booth?
_SOS :: Syntax (Either SEI UI)
_SOS = (left . seiP) <+> (right . uiP)

withAttitude :: Syntax Sumti -> Syntax Sumti
withAttitude syn = (first handleSOS ||| id) . reorder
                <<< (syn &&& optional _SOS)
    where reorder = mkIso f g where
              f ((a,mt),Just sos)  = Left ((sos,a),mt)
              f ((a,mt),Nothing )  = Right (a,mt)
              g (Left ((ui,a),mt)) = ((a,mt),Just ui)
              g (Right (a,mt))     = ((a,mt),Nothing)

--A UI phrase can be considered an implicit
--statment with the predicate 'cinmo'
--the "manage" iso add all necesary info to creat the statement
--We just have to create instances of this with (selbri &&& mapIso toSumti)
--before we can create the statement with _frames
--and then add it to the State
handleUI :: SynIso ((Atom,TruthVal),Atom) Atom
handleUI = (handleXu ||| (rmfstAny (xu,tv) ||| handleUI') . switchOnFlag "xu") . switchOnFlag "handleXu"
    where --We also call handleUI' to keep the random seed consistent
          handleXu = Iso f g -- . inverse (sndToState 1) . handleUI'
          f _ = pure $ Node "VariableNode" "xu" noTv
          g _ = lift $ Left "Printing with handleXu flag is not allowed."
          xu = Node "ConceptNode" "xu" noTv
          tv = stv 0.75  0.9

handleUI' :: SynIso ((Atom,TruthVal),Atom) Atom
handleUI' = sndToState 1
         . second (tolist1 . _frames . (selbri *** tolist1 . toSumti)
                  )
         . manage
    where manage = mkIso f g where
              f ((ui,tv),a) = (a,((tv,ui),(a,"1")))
              g (a,((tv,ui),_)) = ((ui,tv),a)

          toSumti = id *** just
          selbri  = addsnd Nothing . second instanceOf

handleSEI :: SynIso (Atom,Atom) Atom
handleSEI = fstToState 1 . first tolist1

--SOS Second Order Sentence
handleSOS :: SynIso (Either SEI UI,Atom) Atom
handleSOS = (handleSEI ||| handleUI) . expandEither
