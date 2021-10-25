  open Printf;;
  let input = "map10.in";;(*Sys.argv.(1);;*)
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

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [] ;;

  let map = Array.of_list(List.map Array.of_list (List.map explode (read_file input)));;

  let rec find map element i j =
    if map.(i).(j) = element then (i,j) 
    else if  j = (Array.length map.(0)-1) then find map element (i+1) 0
    else find map element i (j+1);;

  let append_state queue state cost  = 
    [state,cost]@queue;;

  let in_bounds map (i,j)   = 
    i >= 0 && 
    j >= 0 && 
    i < Array.length map && 
    j < Array.length map.(0) && 
    map.(i).(j) != 'X'&&
    map.(i).(j) != '\r'&&
    map.(i).(j) != '\n' 
  ;;

  let rec print_path path = 
  match path with
    |[] -> ()
    | e::l -> print_char e ; print_path l
  ;;

  let rec create_path  prev node = 
    match (Hashtbl.find prev node) with
    | (i,j,m,cost) ->  
                if (i == -1) then [] else (create_path prev (i,j))@[m];;
    

  let print_node node = 
  match node with 
   | (i,j) -> print_int i ;print_int j;; 

  let rec print_queue queue = 
  match queue with
   | [] -> print_char '_'
   | ((i,j),m)::t -> print_char ' ';print_int i ;print_int j;print_char ' ';print_queue t;; 


  let get_frontier map i j move =
    match move with 
    |'R' ->  (i,j+1)  
    |'L' ->  (i,j-1)   
    |'U' ->  (i-1,j)  
    |'D' ->  (i+1,j)
    | _  ->  (i,j)   
    ;;

  let init = find map 'S' 0 0 ;; (*• start = θέση του "S" στο χάρτη*)(*init is tuple*)

  let move_list = ['R';'L';'D';'U'];;
  let q0 = [];; (*ουρά προτεραιότητας καταστάσεων,Q = κενή*)
  let q1 = [];; 
  let q2 = [];;
  let q3 = [];;

  let q0 = append_state q0 init 0 ;; (*• βάλε στην Q την init με προτεραιότητα 0*)(* q have tuples *)

  let prev  = Hashtbl.create 1;; 

  Hashtbl.add prev init (-1,-1,'W',0);; (* • πίνακας καταστάσεων που έχουμε επισκεφθεί, prev = κενός*)

  let assign_cost move = 
    match move with
      | 'R' -> 1
      | 'L' -> 2
      | 'D' -> 1
      | 'U' -> 3 
      |  _  -> 0;;


         let rec my_func q0 q1 q2 q3 prev map tempCost move_list  = 
            match q0 with
            |[]->  let move_list = ['R';'L';'D';'U';] in my_func q1 q2 q3 [] prev map (tempCost + 1) move_list
            |((i,j),cost)::t -> if map.(i).(j) == 'E' 
              then 
              (print_int cost;*print_char ' ';(print_path (create_path prev ((i,j),tempCost))) ) 
              else 
              (match move_list with
              | [] -> let move_list = ['R';'L';'D';'U';] in
                        my_func t q1 q2 q3 prev map tempCost move_list
              | hd::tl->    if (in_bounds map (get_frontier map i j hd)) 
                                then( 
                                  let cost = assign_cost hd
                                          in
                                            match cost with
                                            | 1 ->  
                                                      let next = get_frontier map i j hd in
                                                      if (not (Hashtbl.mem prev next)) 
                                                        then(
                                                        Hashtbl.add prev next (i,j,hd,tempCost);
                                                        let q1 = append_state q1 next (tempCost+cost) in 
                                                        my_func q0 q1 q2 q3 prev map tempCost tl)
                                                      else
                                                        my_func q0 q1 q2 q3 prev map tempCost tl  
                                            | 2 ->  
                                                      let next = get_frontier map i j hd in
                                                      if (not (Hashtbl.mem prev next)) 
                                                        then(
                                                        Hashtbl.add prev next (i,j,hd,tempCost);
                                                        let q2 = append_state q2 next (tempCost+cost) in 
                                                        my_func q0 q1 q2 q3 prev map tempCost tl)
                                                      else
                                                        my_func q0 q1 q2 q3 prev map tempCost tl   
                                            | 3 ->   
                                                      let next = get_frontier map i j hd in
                                                      if (not (Hashtbl.mem prev next)) 
                                                        then(
                                                        Hashtbl.add prev next (i,j,hd,tempCost);
                                                        let q3 = append_state q3 next (tempCost+cost) in 
                                                        my_func q0 q1 q2 q3 prev map tempCost tl)
                                                      else
                                                        my_func q0 q1 q2 q3 prev map tempCost tl   
                                            | _ ->   my_func q0 q1 q2 q3 prev map tempCost tl        
                                  )
                                else my_func q0 q1 q2 q3 prev map tempCost tl 
              );;


  my_func q0 q1 q2 q3 prev map 0 move_list;;

