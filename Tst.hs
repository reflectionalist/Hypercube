import UTT


tm1  = als [("x", typ 0)] (var "x")
tps1 = [ als [("x", typ 0)] (typ 0)
       , typ 0 ]

tm2  = als [("x", typ 0), ("y", typ 0)] (var "y")
tps2 = [ als [("x", typ 0), ("y", typ 0)] (typ 0)
       , als [("x", typ 0)] (typ 0)
       , typ 0 ]

tm3  = als [ ("x1", typ 0), ("x2", als [("y", tpu)] (typ 0))
           , ("x3", tpu), ("x4", aps (var "x2") [var "x3"]) ]
           (aps (var "x2") [(var "x3")])
tps3 = [ als [ ("x1", typ 0), ("x2", als [("y", tpu)] (typ 0))
             , ("x3", tpu), ("x4", aps (var "x2") [var "x3"]) ]
             (typ 0)
       , als [("x1", typ 0), ("x2", als [("y", tpu)] (typ 0)), ("x3", tpu)] (typ 0)
       , als [("x1", typ 0), ("x2", als [("y", tpu)] (typ 0))] (typ 0)
       , als [("x1", typ 0)] (typ 0)
       , typ 0 ]

