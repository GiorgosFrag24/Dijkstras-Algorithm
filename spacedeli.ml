let list_vertices graph =
  List.fold_left (fun acc ((a, b), _) ->
    let acc = if List.mem b acc then acc else b::acc in
    let acc = if List.mem a acc then acc else a::acc in
    acc
  ) [] graph
 
let neighbors v =
  List.fold_left (fun acc ((a, b), d) ->
    if a = v then (b, d)::acc else acc
  ) []
 
let remove_from v lst =
  let rec aux acc = function [] -> failwith "remove_from"
  | x::xs -> if x = v then List.rev_append acc xs else aux (x::acc) xs
  in aux [] lst
 
let with_smallest_distance q dist =
  match q with
  | [] -> assert false
  | x::xs ->
      let rec aux distance v = function
      | x::xs ->
          let d = Hashtbl.find dist x in
          if d < distance
          then aux d x xs
          else aux distance v xs
      | [] -> (v, distance)
      in
      aux (Hashtbl.find dist x) x xs
 
let dijkstra max_val zero add graph source target =
  let vertices = list_vertices graph in
  let dist_between u v =
    try List.assoc (u, v) graph
    with _ -> zero
  in
  let dist = Hashtbl.create 1 in
  let previous = Hashtbl.create 1 in
  List.iter (fun v ->                  (* initializations *)
    Hashtbl.add dist v max_val        (* unknown distance function from source to v *)
  ) vertices;
  Hashtbl.replace dist source zero;    (* distance from source to source *)
  let rec loop = function [] -> ()
  | q ->
      let u, dist_u =
        with_smallest_distance q dist in   (* vertex in q with smallest distance in dist *)
      if dist_u = max_val then
        failwith "vertices inaccessible";  (* all remaining vertices are inaccessible from source *)
      if u = target then () else begin
        let q = remove_from u q in
        List.iter (fun (v, d) ->
          if List.mem v q then begin
            let alt = add dist_u (dist_between u v) in
            let dist_v = Hashtbl.find dist v in
            if alt < dist_v then begin       (* relax (u,v,a) *)
              Hashtbl.replace dist v alt;
              Hashtbl.replace previous v u;  (* previous node in optimal path from source *)
            end
          end
        ) (neighbors u graph);
        loop q
      end
  in
  loop vertices;
  let s = ref [] in
  let u = ref target in
  while Hashtbl.mem previous !u do
    s := !u :: !s;
    u := Hashtbl.find previous !u
  done;
  (source :: !s)
;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [] ;;

let read_file filename = 
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines ;;  

let connect t1 l1 c1 t2 l2 c2   =
    match t1  with    
    |'X' -> []
    |'\r'-> []
    |'W' -> (match t2 with
                |'X' -> []
                |'\r'-> []
                | _ -> 	let  n1 = (t1,(l1,c1),1)
                        and  n2 = (t2,(l2,c2),1)
                        and  n3 = (t1,(l1,c1),0)
                        and  n4 = (t2,(l2,c2),0) in 
                        [(n1,n3),1]@[(n3,n1),1] @[(n1,n2),2]@[(n2,n1),2]@[(n3,n4),1]@[(n4,n3),1] 
            )
    | _ ->(	match t2 with
            |'X' -> []
            |'\r'-> []
            |'W' -> 	let  n1 = (t1,(l1,c1),1) 
                        and  n2 = (t2,(l2,c2),1) 
                        and  n3 = (t1,(l1,c1),0) 
                        and  n4 = (t2,(l2,c2),0) in 
                        [(n1,n2),2]@[(n2,n1),2]@[(n2,n4),1]@[(n4,n2),1]@[(n4,n3),1]@[(n3,n4),1]
                        
            |_ -> 	let  n1 = (t1,(l1,c1),1)
                    and  n2 = (t2,(l2,c2),1)
                    and  n3 = (t1,(l1,c1),0)
                    and  n4 = (t2,(l2,c2),0) in
                        [(n1,n2),2]@[(n2,n1),2]@[(n3,n4),1]@[(n4,n3),1]
        );;		  	
            
let rec connect_line line x y =
    match line with 
    |[] -> []
    |[a] -> []
    |a::b::c -> (connect a x y b x (y+1))@(connect_line (b::c) x (y+1));;

let rec connect_up_down a b line column = 
    match a with 
    |[] -> []
    |hda::tla -> (	match b with
                | [] -> []
                | hdb::tlb  -> (connect hda line column hdb (line+1) column)@(connect_up_down tla tlb line (column+1)));;

let rec create_graph map_list line column = (*graph is empty list*)
    match map_list with 
    |[] -> []
    |[a] -> connect_line a line column
    |a :: b-> (connect_line a line column)@(connect_up_down a (List.hd b) line column)@(create_graph b (line+1) column);;

let input = Sys.argv.(1);;

let graph = create_graph (List.map explode (read_file input)) 0 0 ;;
    
let rec find_element string_name tuples_list =
       match tuples_list with
            [] -> raise Not_found
            |(((s,(x,y),_),_), _)::tl -> 	if s = string_name then (x, y) 

                                  			else find_element string_name tl
;;


let rec print_path p cost  =
        match p  with
        | [] -> []
        | [(x,(a,b),p1)] -> [string_of_int cost] 
        | (x,(a,b),p1)::(y,(c,d),p2)::rest-> 
           (match (a-c) with
            |  1 -> if p1 == p2 then ["U"]@print_path ([(y,(c,d),p2)]@rest) (cost + 1 + p2)  else ["W"]@["U"]@print_path ([(y,(c,d),p2)]@rest) (cost + 2 +p2)   
            | -1 -> if p1 == p2 then ["D"]@print_path ([(y,(c,d),p2)]@rest) (cost + 1 + p2)  else ["W"]@["D"]@print_path ([(y,(c,d),p2)]@rest) (cost + 2 +p2) 
            |  0 -> (	match (b-d) with
            			|  1 -> if p1 == p2 then ["L"]@print_path ([(y,(c,d),p2)]@rest) (cost + 1 + p2)  else ["W"]@["L"]@print_path ([(y,(c,d),p2)]@rest) (cost + 2 + p2)
            			| -1 -> if p1 == p2 then ["R"]@print_path ([(y,(c,d),p2)]@rest) (cost + 1 + p2)  else ["W"]@["R"]@print_path ([(y,(c,d),p2)]@rest) (cost + 2 + p2)
            			|  0 ->["W"]@print_path ([(y,(c,d),p2)]@rest) (cost + 1 )
            			|_ -> ["false"]
            			

            		)
            |_ -> ["false"]
            )	;;
let p = dijkstra max_int 0 (+) graph  ('S',find_element 'S' graph,1) ('E',find_element 'E' graph,1)  ;;

let rec print_list list =  
match list with
|[] -> print_string ""
|[a] -> print_string ""
|e::l -> print_string e ; print_list l ;;

let final_answer p = print_string (List.nth (print_path p 0) ((List.length (print_path p 0) ) -1));
                        print_string (" ");
                        print_list(print_path p 0) ;;
final_answer p ;        