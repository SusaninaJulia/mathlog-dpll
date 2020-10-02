module dpll.DPLL

type Formula = 
    | PV of int 
    | NOT of Formula
    | OR of Formula * Formula
    | AND of Formula * Formula
    
let rec VariablesNumber (f : Formula)  =
    let max a b = if a > b then a else b
    match f with 
    | PV p -> abs p
    | NOT phi -> abs (VariablesNumber phi)
    | AND (phi1, phi2) -> max (VariablesNumber phi1) (VariablesNumber phi2)
    | OR (phi1, phi2) -> max (VariablesNumber phi1) (VariablesNumber phi2)

let rec CNF(f: Formula, conjs: list<list<int>>, newp: int) =
    match f with
    | PV p -> 
        (p, conjs, newp)
    | NOT phi -> 
        let (nf, nconjs, newp) = CNF(phi, conjs, newp)
        (-nf, nconjs, newp)
    | AND (phi1, phi2) ->   
        let (nf1, nconjs1, newp1) = CNF(phi1, conjs, newp)
        let (nf2, nconjs2, newp2) = CNF(phi2, nconjs1, newp1)
        let np = newp2 + 1
        let nconjs = nconjs2 @ [[-np; nf1]; [-np; nf2]; [-nf1; -nf2; np]]
        (np, nconjs, np)
    | OR (phi1, phi2) ->
        let (nf1, nconjs1, newp1) = CNF(phi1, conjs, newp)
        let (nf2, nconjs2, newp2) = CNF(phi2, nconjs1, newp1)
        let np = newp2 + 1
        let nconjs = nconjs2 @ [[np; -nf1]; [np; -nf2]; [nf1; nf2; -np]]
        (np, nconjs, np)
    
let TSEYTIN(f: Formula) = 
    let (pv, cnjs, _) = CNF(f, [], VariablesNumber(f))
    [pv] :: cnjs
    
type Flag = 
    | SAT
    | UNSAT
    | NEXT

let EliminatePureLiteral(S: list<list<int>>, l : int) = 
     List.filter (fun (el : list<int>) -> not (List.length(el) = 1 && el.[0] = l )) S
     |> List.map (fun el -> List.filter (fun el2 -> el2 <> l) el) 
    
let UnitPropagate(S: list<list<int>>, l : int) =
    List.filter (fun C -> not (List.exists ((=) l) C)) S
    |> List.map (fun el -> List.filter (fun el2 -> el2 <> -l) el) 
    
let rec DPLL (S: list<list<int>>, M : Set<int>) = 
    let units = List.filter (fun el -> List.length(el) = 1) S 
                |> List.concat |> Set.ofList
    let S1 = List.fold (fun S1 el -> UnitPropagate(S1, el)) S (Set.toList units)
    let M1 = Set.union M units
    if List.isEmpty S1
        then  (SAT, M1)
        else
            if Set.exists (fun el -> Set.exists ((=) -el) M1) M1
            then (NEXT, Set.empty)
            else
                let pvs = List.concat S1 |> Set.ofList
                let pures = Set.filter (fun el -> not (Set.exists ((=) -el) pvs)) pvs 
                let S2 = List.fold (fun S2 el -> EliminatePureLiteral(S2, el)) S1 (Set.toList pures) 
                let M2 = Set.union M1 pures
                if List.isEmpty S2
                then (SAT, M2)
                else
                    if List.exists (List.isEmpty) S2
                    then (UNSAT, Set.empty)
                    else 
                        let l = S2.[0].[0]
                        let (flag, m) = DPLL([l] :: S2, M2.Add(l))
                        if flag = SAT
                        then (flag, m)
                        else DPLL([-l] :: S2, M2.Add(-l))
                    