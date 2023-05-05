
open Tirocinio.Nets;;
open Examples.Net1;;
open Examples.Net2;;
open Examples.Net3;;

assert(CN.is_pCN c1);;
assert(CN.is_CN c2);;
assert(ReversibleCN.is_rCN c3);;
