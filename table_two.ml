type bool_expr = 
        |  Var of string
        |  Not of bool_expr
        |  And of bool_expr * bool_expr
        |  Or of bool_expr * bool_expr;; 

let rec eval = fun a_var a_ass b_var b_ass expr -> match expr with
        | Var v when v = a_var -> a_ass
        | Var v when v = b_var -> b_ass
        | Not e -> not (eval a_var a_ass b_var b_ass e)
        | And (e1, e2) -> (eval a_var a_ass b_var b_ass e1) && (eval a_var a_ass b_var b_ass e2)
        | Or  (e1, e2) -> (eval a_var a_ass b_var b_ass e1) || (eval a_var a_ass b_var b_ass e2)
        | Var _ -> failwith "Unknown Var";;


let table_two = fun a b expr ->  
        let rows = [ (true, true); (true, false); (false, true); (false, false) ] 
        in List.map (
          fun (a_val, b_val) -> (a_val, b_val, eval a a_val b b_val expr)
        ) 
        rows;;

