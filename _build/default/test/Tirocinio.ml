
open Tirocinio.Nets;;
open Examples.Net1;;
open Examples.Net2;;
open Examples.Net3;;

assert(CN.is_pCN c1);;
assert(CN.is_CN c2);;
assert(ReversibleCN.forward_subnet_is_pCN c3);;
assert(ReversibleCN.exaclty_one_reverse_transition c3);;