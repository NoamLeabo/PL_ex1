let rec insert_at = fun e i l -> match i, l with
        |0,_ -> e::l
        |_, h::t -> h::insert_at e (i-1) t
        |_, [] -> l;;

