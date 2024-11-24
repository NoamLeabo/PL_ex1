type bool_expr = 
        |  Var of string
        |  Not of bool_expr
        |  And of bool_expr * bool_expr
        |  Or of bool_expr * bool_expr;; 
        
let rec eval = fun vars expr -> match expr with
        | Var v -> (try List.assoc v vars with Not_found -> failwith "Unknown variable")
        | Not e -> not (eval vars e)
        | And (e1, e2) -> (eval vars e1) && (eval vars e2)
        | Or (e1, e2) -> (eval vars e1) || (eval vars e2);;


let rec all_combinations = fun vars -> match vars with
        | [] -> [[]]
        | v :: rest -> let sub_combinations = all_combinations rest in
                List.concat [
                    List.map (fun comb -> (v, true) :: comb) sub_combinations;
                    List.map (fun comb -> (v, false) :: comb) sub_combinations
                ];;

let table vars expr =
        let combinations = all_combinations vars in
        List.map (fun assignment -> (assignment, eval assignment expr)) combinations;;