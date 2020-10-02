module dpll.Main

open dpll.DPLL

[<EntryPoint>]
let main args = 
    let fs = [
                NOT(OR(AND(PV 1, PV 2), NOT (PV 3)));
                AND(PV 1, PV -1);
                PV -1;
                AND(AND(PV 1, PV 2), AND(PV 3, PV -4));
                OR(AND(PV 1, PV 2), AND(PV 3, PV -4));
                NOT(OR(AND(PV -1, PV 1), AND(PV 2, PV -2)));
             ]
    for f in fs do
        let nconjs = TSEYTIN(f)
        let (sat, model) = DPLL(nconjs, Set.empty) 
        printfn "Formula:"
        System.Console.WriteLine(f)
        printfn "Tseityn transformation:"
        for el in nconjs do
            List.iter (fun i -> printf "%i " i) el
            printfn ""
        if sat = SAT
        then 
            printfn "DPLL result: SAT"
            printfn "Model: %A" model
        else printfn "DPLL result: UNSAT"
    0

