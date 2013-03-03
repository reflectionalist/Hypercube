import Prelude hiding (all)
import UTT


tm1  = all ("x", typ 0) (var "x")
tps1 = [ all ("x", typ 0) (typ 0)
       , typ 0 ]

tm2  = als [("x", typ 0), ("y", typ 0)] (var "y")
tps2 = [ als [("x", typ 0), ("y", typ 0)] (typ 0)
       , all ("x", typ 0) (typ 0)
       , typ 0 ]

tm3  = als [ ("x1", typ 0), ("x2", all ("y", nat) (typ 0))
           , ("x3", nat), ("x4", app (var "x2") (var "x3")) ]
           (app (var "x2") (var "x3"))
tps3 = [ als [ ("x1", typ 0), ("x2", all ("y", nat) (typ 0))
             , ("x3", nat), ("x4", app (var "x2") (var "x3")) ]
             (typ 0)
       , als [("x1", typ 0), ("x2", all ("y", nat) (typ 0)), ("x3", nat)] (typ 0)
       , als [("x1", typ 0), ("x2", all ("y", nat) (typ 0))] (typ 0)
       , all ("x1", typ 0) (typ 0)
       , typ 0 ]

