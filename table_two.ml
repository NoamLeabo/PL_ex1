type bool_expr = 
        |  Var of string
        |  Not of bool_expr
        |  And of bool_expr * bool_expr
        |  Or of bool_expr * bool_expr;; 

let rec eval = fun a_ass b_ass expr -> match expr with
        | Var "a" -> a_ass
        | Var "b" -> b_ass
        | Not e -> not (eval a_ass b_ass e)
        | And (e1, e2) -> (eval a_ass b_ass e1) && (eval a_ass b_ass e2)
        | Or  (e1, e2) -> (eval a_ass b_ass e1) || (eval a_ass b_ass e2)
        | Var _ -> a_ass;;


let table_two = fun a b expr ->  
        let rows = [ (true, true); (true, false); (false, true); (false, false) ] 
        in List.map (
          fun (a_val, b_val) -> (a_val, b_val, eval a_val b_val expr)
        ) 
        rows;;

