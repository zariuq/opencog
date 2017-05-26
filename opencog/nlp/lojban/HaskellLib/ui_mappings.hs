{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables#-}

import OpenCog.AtomSpace
import OpenCog.Lojban
import OpenCog.Lojban.Util

import qualified Data.Csv as CSV
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS

cmavoSrc = "cmavo.csv"

  {- Code used to generate Atoms (in uiAtoms) describing the equivalence:

   "sei mi cinmo be lo ka gleki bei lo nu mi jimpe se'u mi jimpe" = ui mi jimpe

    ui X = sei mi cinmo be lo ka gleki bei lo nu X se'u X

    X is represented with "lo nu" for statement/bridi, but not for gismu/pro-sumti

    --

    The code below reads the UI1 links from the cmavoSrc file; however, some links are for UI1 + nai, so some manual processing was done.

    The Atoms were defined using a template rather than by manipulating the parser output for the lojban sentences. Which version is more easy to maintain is not clear to me (Zar) as automatically converting the specific Atomese to abstract Atomese could be messed up by small changes to the format as well.
  -}

data Cmavo = Cmavo {cName :: !String, cClass :: !String, cGismu :: !String}

instance CSV.FromNamedRecord Cmavo where
    parseNamedRecord r = Cmavo <$> r CSV..: C.pack "particle"
                               <*> r CSV..: C.pack "S"
                               <*> r CSV..: C.pack "4 (links)"

getUI :: String -> IO [(String, String)]
getUI csv = do
    csvdata <- LBS.readFile csv
    case CSV.decodeByName csvdata of
        Left s -> error s
        Right (_,v) -> pure . (map mkPairs) . filterUI
                          $ ((,,) <$> cClass <*> cName <*> cGismu) <$> V.toList v
    where filterUI = filter (\(a,_,_) -> a == "UI1")

-- Decided to just use this to get the core list, and to filter manually!
mkPairs :: (String, String, String) -> (String, String)
mkPairs (uiClass, uiName, uiGismu) = (uiName, cleanGismu uiGismu)
  where cleanGismu = filter (\c -> not $ c `elem` ['{','}',',']) -- . optHead . words
        optHead [] = ""
        optHead xs = head xs

ui1 :: [(String, [(Int, String)])]
ui1 = [("a'a",[(1,"jundi")]),("a'e",[(1,"sanji"),(1,"tatpi")]),("a'i",[(1,"troci"),(1,"snada"),(1,"gunka")]),("a'o",[(1,"pacna")]),("ai",[(3,"te mukti"),(1,"cuxna"),(1,"zukte")]),("au",[(1,"djica")]),("e'a",[(1,"e'ande")]),("e'e",[(1,"kakne"),(1,"certu")]),("e'i",[(1,"bapli"),(1,"rinju")]),("e'o",[(1,"cpedu"),(1,"pikci")]),("e'u",[(1,"stidi")]),("ei",[(1,"bilga")]),("i'a",[(1,"mansa")]),("i'e",[(1,"zanru")]),("i'i",[(1,"kansa"),(1,"gunma")]),("i'o",[(1,"ckire")]),("i'u",[(1,"slabu")]),("ia",[(1,"krici"),(1,"jinvi")]),("ie",[(1,"tugni")]),("ii",[(1,"terpa")]),("io",[(1,"sinma")]),("iu",[(1,"prami")]),("ne'au",[(0,"")]),("o'a",[(1,"jgira")]),("o'e",[(1,"kansa")]),("o'i",[(1,"kajde"),(1,"ckape")]),("o'o",[(1,"denpa")]),("o'u",[(1,"surla")]),("oi",[(1,"pante")]),("u'a",[(1,"jinga"),(1,"cnemu"),(1,"prali")]),("u'e",[(2,"se marvele")]),("u'i",[(2,"se zdile"),(2,"se xajmi"),(1,"xalbo")]),("u'o",[(1,"virnu")]),("u'u",[(1,"xenru"),(1,"zungi")]),("ua",[(1,"facki")]),("ue",[(1,"spaji")]),("ui",[(1,"gleki")]),("uo",[(1,"mulno"),(1,"mansa"),(1,"fanmo"),(1,"snada")]),("uu",[(1,"kecti")])]


uiAtoms = map (uncurry uimap) ui1

{-ignored
("ne'au",(1,"")), ("a'u",(1,"lo se cinri"))
-}

uimap :: String -> [(Int, String)] -> Atom
uimap ui gs =
  let ui' = '$':ui
      mkEval v s g = cEvalL highTv
                        (cPN (g++"_sumti"++(show s)) highTv)
                        (cLL [
                           (cVN $ "$"++(show v))
                          ,(cVN "$mi")
                          ]
                        )
      mkImpl v g = cImpL highTv
                    (cVN $ "$"++(show v))
                    (cPN g highTv)
      mkTVL v = cTVL highTv
                  (cVN $ "$"++(show v))
                  (cTN "PredicateNode")
      gismuLinkKa =
        case gs of
          [(s, gismuLink)] -> -- Just one
            [mkEval 3 s gismuLink
            ,mkImpl 3 gismuLink
            ]
          xs ->
            let vxs = (zip [3..] xs) in
            [cOL highTv
              (map (\(v, (s, g)) -> mkEval v s g) vxs)
            ] ++ (map (\(v, (_, g)) -> mkImpl v g) vxs)
      vns = [3..(2 + length gs)]
  in
  cLamdaL highTv
    (cVN "$X")
    (cEquivL highTv
      -- UI
      (cExL highTv
        (cTVL highTv
         (cVN ui')
         (cTN "ConceptNode"))
        (cAL highTv
          [
           cEvalL highTv
            (cCN "ui_sumti1" highTv)
            (cLL [
                  (cVN ui')
                 ,(cVN "$X")
                 ]
            )
          ,cInhL highTv
            (cVN ui')
            (cCN ui highTv)
          ]
        )
      )
      -- SEI
      (cExL highTv
        (cVL
         [
           cTVL highTv
            (cVN "$cinmo")
            (cTN "PredicateNode")
         , cTVL highTv
             (cVN "$mi")
             (cTN "ConceptNode")
         ])
        (cAL highTv
          [
           cAL highTv
            [
             cEvalL highTv
              (cCN "cinmo_sumti1" highTv)
              (cLL [
                    (cVN "$cinmo")
                   ,(cVN "$mi")
                   ]
              )
            ,cEvalL highTv
              (cCN "cinmo_sumti2" highTv)
              (cLL [
                    (cVN "$cinmo")
                   ,(cCN "lo___ka" highTv)
                   ]
              )
            ,cEvalL highTv
              (cCN "cinmo_sumti3" highTv)
              (cLL [
                    (cVN "$cinmo")
                   ,(cVN "$X")
                   ]
              )
            ,cImpL highTv
              (cVN "$cinmo")
              (cCN "cinmo" highTv)
            ,cInhL highTv
              (cVN "$mi")
              (cCN "mi" highTv)
            ,cInhL highTv
              (cCN "lo___ka" highTv)
              (cSSL highTv
                [
                 (cVN "$var")
                ,cEvalL highTv
                  (cPN "ka_sumti1" highTv)
                  (cLL [
                        (cPN "___ka" highTv)
                       ,(cVN "$var")
                       ]
                  )
                ]
              )
            ,cEquivL highTv
              (cEvalL highTv
                (cPN "___ka" highTv)
                (cLL [cVN "$1"])
              )
              (cMemL highTv
                (cVN "$1")
                (cSSL highTv
                  [
                   (cVN "$2")
                  ,(cExL highTv
                    (cVL
                      (map mkTVL vns)
                    )
                    (cAL highTv
                      ([
                       cImpL highTv
                        (cPN "ckaji___ka" highTv)
                        (cPN "ckaji" highTv)
                      ,cAL highTv
                        [
                         cEvalL highTv
                           (cPN "ckaji_sumti1" highTv)
                           (cLL [
                                (cPN "ckaji___ka" highTv)
                               ,(cCN "$2" highTv)
                               ]
                           )
                        ,cEvalL highTv
                          (cPN "ckaji_sumti2" highTv)
                          (cLL ([(cPN "ckaji___ka" highTv)]
                                ++ (case vns of
                                      [v] -> [cVN "$3"]
                                      vs -> [cOL highTv
                                                (map (\v -> cVN $ "$" ++ show v) vns)
                                              ]
                                   )
                              )
                          )
                        ]
                       ] ++ gismuLinkKa
                      )
                    )
                   )
                  ]
                )
              )
            ]
          ]
        )
      )
    )
