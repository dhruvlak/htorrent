import Network.Info

main = do
         ni <- getNetworkInterfaces
         putStr ( show (checkinterface ni ) )


checkinterface (n:ns) = if ((name n) == "eth0")
                            then Just ( show (ipv4 n) )
                          else checkinterface ns

checkinterface [] = Nothing
