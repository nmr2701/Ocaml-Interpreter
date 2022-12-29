let write_file_example (file_path: string) : unit =
  let fp = open_out file_path in
  Printf.fprintf fp "writing this line!";
  close_out fp

type const = 
| Int of int 
| String of string 
| ConstError 
| NoQuit 
| Left of const 
| Right of const 
| Closure of const * const * com list

and com = 
| Quit 
| Push of const 
| Pop 
| Add 
| Sub 
| Mul 
| Div 
| Swap 
| Neg 
| Concat 
| NoQuit 
| And 
| Or 
| Not 
| Equal 
| Lte 
| Local of const 
| Global of const 
| ComError 
| Begin 
| End 
| IfThen 
| Else 
| EndBegin 
| InjL 
| InjR 
| CaseLeft 
| Right 
| Tuple of int 
| Get of int 
| Fun of const * const 
| Mut of const * const 
| Call 
| Return  


let split_string (src : string) : (string list) = 
  let x = String.split_on_char '\n' src in
    let z = List.fold_right (fun y a -> (String.split_on_char ' ' y) :: a) x [] in
      List.concat z
      
let string_to_const (str : string) : const =
  try int_of_string str |> ignore; Int(int_of_string str)
  with Failure _ -> String(str)

let is_integer (str : string) : bool =
  try int_of_string str |> ignore; true
  with Failure _ -> false

let rec character_checker (char_list : int list) : bool = 
  match char_list with
  |[] -> true
  |a :: tl -> if a >= 65 && a <= 90 || a = 95 || a >= 97 && a <= 122 || 48 <= a && a <= 57 then true && character_checker tl else false 

let check_name (str : string) : bool = 
  let char_list = String.fold_right (fun x a -> Char.code x :: a) str [] in 
  match char_list with 
  |lower :: tl -> if lower >= 97 && lower <= 122 then character_checker tl else false
  |_ -> false

let extract_main (src : com list * com list) : com list =
  match src with
  |(a,b) -> a

let extract_fun (src : com list * com list) : com list =
  match src with
  |(a,b) -> b

let extract_stack (src : const list * const list) : const list = 
  match src with
  |(a,b) -> a

let extract_global (src : const list * const list) : const list = 
  match src with
  |(a,b) -> b

let rec remove_const (src : const) : string  =
  match src with
  |Int(x) -> string_of_int x
  |String(x)-> x
  |Left(x) -> "Left " ^ remove_const x
  |Right(x)-> "Right " ^ remove_const x
  |Closure(a , b, c) -> ("Clo (" ^ remove_const a ^ ", " ^ remove_const b ^ ")") 
  |_ -> "Error"


let rec com_list (src : string list): com list = 
  match src with 
  | "Push" :: a :: tl -> Push(string_to_const a) :: com_list tl
  | "Pop" :: tl -> Pop :: com_list tl
  | "Add" :: tl -> Add :: com_list tl
  | "Sub" :: tl -> Sub :: com_list tl
  | "Mul" :: tl -> Mul :: com_list tl
  | "Div" :: tl -> Div :: com_list tl
  | "Swap" :: tl -> Swap :: com_list tl
  | "Neg" :: tl -> Neg :: com_list tl
  | "Concat" :: tl -> Concat :: com_list tl
  | "And" :: tl -> And :: com_list tl
  | "Or" :: tl -> Or :: com_list tl
  | "Not" :: tl -> Not :: com_list tl
  | "Equal" :: tl -> Equal :: com_list tl
  | "Lte" :: tl -> Lte :: com_list tl
  | "Local" :: a :: tl -> if check_name a then Local(string_to_const a) :: com_list tl else [ComError] 
  | "Global" :: a :: tl -> if check_name a then Global(string_to_const a) :: com_list tl else [ComError] 
  | "Begin" :: tl -> Begin :: com_list tl
  | "End" :: tl -> End :: com_list tl
  | "IfThen" ::  tl -> IfThen :: com_list tl
  | "Else" :: tl -> Else :: com_list tl
  | "InjL" :: tl -> InjL :: com_list tl
  | "InjR" :: tl -> InjR :: com_list tl
  | "CaseLeft" :: tl -> CaseLeft :: com_list tl
  | "Right" :: tl -> Right :: com_list tl
  | "Quit" :: tl -> [Quit] 
  | "Tuple" :: a :: tl -> if is_integer a then Tuple(int_of_string a) :: com_list tl else [ComError]
  | "Get" :: a :: tl -> if is_integer a then Get(int_of_string a) :: com_list tl else [ComError]
  | "Fun" :: a :: b :: tl-> if check_name a && check_name b then Fun(string_to_const a, string_to_const b) :: com_list tl else [ComError]
  | "Mut" :: a :: b :: tl-> if check_name a && check_name b then Mut(string_to_const a, string_to_const b) :: com_list tl else [ComError]
  | "Call" :: tl -> Call :: com_list tl
  | "Return" :: tl -> Return :: com_list tl
  | [] -> [NoQuit] 
  | _ -> [ComError]

let rec push_from_store (a : const) (src : const list) : const = 
  match src with 
  |[] -> ConstError
  |x :: y :: tl -> if a = x then y else push_from_store a tl
  |_ -> ConstError
  
let push (a : const) (stack : const list) (local : const list) (global : const list): const list = (*How to push CLO from here*)
  match a with
  |String(x) -> if (check_name x) then if (push_from_store a local=ConstError) then push_from_store a global :: stack else push_from_store a local :: stack else a :: stack 
  |_ -> a :: stack

let pop (stack : const list) : const list = 
  match stack with 
  |hd :: tl ->if hd != ConstError then tl else [ConstError]
  |_ -> [ConstError]

let add (stack : const list) : const list= 
  match stack with 
  |Int(a) :: Int(b) :: tl -> Int(( + ) a b) :: tl
  |_ -> [ConstError] 

let sub (stack : const list) : const list= 
  match stack with 
  |Int(a) :: Int(b) :: tl -> Int(( - ) a b) :: tl
  |_  -> [ConstError] 

let mul (stack : const list) : const list= 
  match stack with 
  |Int(a) :: Int(b) :: tl -> Int(( * ) a b) :: tl
  |_ -> [ConstError] 

let div (stack : const list) : const list= 
  match stack with 
  |Int(a) :: Int(b) :: tl -> if b != 0 then Int(( / ) a b) :: tl else [ConstError]
  |_  -> [ConstError] 

let swap (stack : const list) : const list = 
  match stack with
  |a :: b :: tl -> b :: a :: tl
  |_ -> [ConstError] 

let neg (stack : const list) : const list = 
  match stack with 
  |Int(a) :: tl -> Int(( * ) a (-1)) :: tl
  |_  -> [ConstError] 

let concat (stack : const list) : const list =
  match stack with 
  |String(a) :: String(b) :: tl -> String((String.sub a 0 (String.length a -1)) ^ (String.sub b 1 (String.length b -1))) :: tl
  |_  -> [ConstError] 

let pand (stack : const list) : const list =
  match stack with
  |Int(0) :: Int(0) :: tl -> Int(0) :: tl
  |Int(0) :: Int(1) :: tl -> Int(0) :: tl
  |Int(1) :: Int(0) :: tl -> Int(0) :: tl
  |Int(1) :: Int(1) :: tl -> Int(1) :: tl
  |_ ->  [ConstError]

let por (stack : const list) : const list =
  match stack with
  |Int(0) :: Int(0) :: tl -> Int(0) :: tl
  |Int(0) :: Int(1) :: tl -> Int(1) :: tl
  |Int(1) :: Int(0) :: tl -> Int(1) :: tl
  |Int(1) :: Int(1) :: tl -> Int(1) :: tl
  |_ ->  [ConstError] 

let pnot (stack : const list) : const list =
  match stack with
  |Int(0) :: tl -> Int(1) :: tl
  |Int(1) :: tl -> Int(0) :: tl
  |_ ->  [ConstError]

let pequal (stack : const list) : const list =
  match stack with
  |Int(a) :: Int(b) :: tl -> if a != b then Int(0) :: tl else Int(1) :: tl
  |_ ->  [ConstError]

let lte (stack : const list) : const list = 
  match stack with
  |Int(a) :: Int(b) :: tl -> if a<=b then Int(1) :: tl else Int(0) :: tl
  |_ :: tl -> ConstError :: tl
  |_ -> [ConstError]

let rec replace_store (a : const) (src : const list) : const list =
  match src with 
  |x :: y :: tl -> if x = a then tl else x :: y :: (replace_store a tl)
  |_ -> [ConstError]

let exec_store (a : const) (stack : const list) (src : const list) : const list = 
  match stack with 
  |b :: tl -> if List.mem a src then a :: b :: replace_store a src else a :: b :: src
  |_ -> [ConstError]

let rec find_end (src : com list) (count : int) : com list =
  match src with
  |Begin :: tl -> find_end tl ((+) count 1)
  |Fun(a, b) :: tl -> find_end tl ((+) count 1)
  |IfThen :: tl -> find_end tl ((+) count 1)
  |CaseLeft :: tl -> find_end tl ((+) count 1)
  |End :: tl ->  if count = 0 then tl else find_end tl ((-) count 1)
  |_ :: tl -> find_end tl count
  |_ -> [ComError]

let rec find_else (src : com list) (count : int): com list =
  match src with 
  |IfThen :: tl -> IfThen :: find_else tl ((+) count 1)
  |Else :: tl -> if count = 0 then find_end tl 0 else Else :: find_else tl ((-) count 1)
  |x :: tl -> x :: find_else tl count
  |_ -> [ComError]

let rec remove_end (src : com list) (count : int) : com list =
  match src with
  |IfThen :: tl -> IfThen :: (remove_end tl ((+) count 1))
  |Fun(a,b) :: tl -> Fun(a,b) :: (remove_end tl) ((+) count 1)
  |Begin :: tl -> Begin :: (remove_end tl ((+) count 1))
  |CaseLeft :: tl -> CaseLeft :: (remove_end tl ((+) count 1))
  |End :: tl -> if count = 0 then tl else End :: remove_end tl ((-) count 1)
  |x :: tl -> x :: remove_end tl count
  |_ -> [ComError]


let rec remove_ifthen (src : com list) (count : int) : com list =
  match src with
  |IfThen :: tl -> remove_ifthen tl ((+) count 1)
  |Else :: tl ->  if count = 0 then remove_end tl 0 else remove_ifthen tl ((-) count 1)
  |x :: tl -> remove_ifthen tl count
  |_ -> [ComError]

let rec begin_coms (src_com : com list) (count : int) (acc : com list): com list =
  match src_com with
  |End :: tl -> if count = 0 then EndBegin :: acc else begin_coms tl ((-) count 1) (End :: acc)
  |EndBegin :: tl -> EndBegin :: acc
  |IfThen :: tl -> begin_coms tl ((+) count 1) (IfThen :: acc)
  |Begin :: tl -> begin_coms tl ((+) count 1) (Begin :: acc)
  |CaseLeft :: tl -> begin_coms tl ((+) count 1) (CaseLeft :: acc)
  |x :: tl -> begin_coms tl count (x :: acc)
  |_ -> [ComError]



let comerror (stack : const list) : const list = 
  [ConstError] 

let first_elem (stack : const list) : int =
  match stack with
  |Int(1) :: tl -> 1
  |Int(0) :: tl -> 0
  |_ -> 2

let check_error (src_com : const list) : bool = 
  List.fold_left (fun a x -> match x with 
  |ConstError -> false && a
  |_ -> true && a
  ) true src_com

let injl (stack : const list) : const list = 
  match stack with
  |x :: tl -> Left(x) :: tl
  |_ -> [ConstError]

let injr (stack : const list) : const list = 
  match stack with
  |x :: tl -> Right(x) :: tl
  |_ -> [ConstError]

let first_union (stack : const list) : int = 
  match stack with
  |Left(x) :: tl-> 0
  |Right(x) :: tl -> 1
  |_ -> 2

let rec remove_left_branch (src : com list ) (count : int) : com list =
  match src with 
  |Right :: tl -> if count = 0 then remove_end tl 0 else remove_left_branch tl ((-) count 1) 
  |CaseLeft :: tl -> remove_left_branch tl ((+) count 1)
  |x :: tl -> remove_left_branch tl count
  |_ -> [ComError]

let remove_union (src : const list) : const list =
  match src with
  |Left(x) :: tl -> x :: tl
  |Right(x) :: tl -> x :: tl
  |_ -> [ConstError]

let rec find_right (src : com list) (count : int) : com list =
  match src with
  |CaseLeft :: tl -> CaseLeft :: find_right tl ((+) count 1)
  |Right :: tl -> if count = 0 then find_end tl 0 else Right :: find_right tl ((-) count 1)
  |x :: tl -> x :: find_right tl count
  |_ -> [ComError]

let rec cont_tuple (src : const list) (count : int) = 
  if count != 0 && check_error src then match src with
  |a :: b :: tl -> cont_tuple (String(remove_const b ^ ", " ^ remove_const a) :: tl) ((-) count 1) 
  |_ -> [ConstError]
  else if check_error src then match src with
  |a :: tl -> String("(" ^ remove_const a) :: tl
  |_ -> [ConstError]
  else
  [ConstError]
   

let init_tuple (src : const list) (count : int) =
  if count = 0 then String("()") :: src else
  match src with
  |a :: tl -> cont_tuple (String(remove_const a ^ ")") :: tl) ((-) count 1)
  |_ -> [ConstError]

let tuple_to_list (src : string) : string list =
  let q = (String.sub src 0 (String.length src - 1)) in
    String.split_on_char ',' q
      
let check_tuple (src : const list) : bool =
  match src with
  |String(x) :: tl -> if String.get x 0 = '(' then true else false
  |_ -> false

let get (src : const list) (id : int) : const list =
  match src with
  |String(a) :: tl -> if List.length (tuple_to_list a) > id then string_to_const (String.sub (List.nth (tuple_to_list a) id) 1 (String.length (List.nth (tuple_to_list a) id) - 1)) :: src else [ConstError]
  |_ -> [ConstError]

let rec remove_upto_next (src : com list) (count : int): com list =
  match src with
  |IfThen :: tl -> remove_upto_next tl ((+) count 1)
  |Begin :: tl -> remove_upto_next tl ((+) count 1)
  |CaseLeft :: tl -> remove_upto_next tl ((+) count 1)
  |End :: tl -> if count = 0 then tl else remove_upto_next tl ((-) count 1)
  |Mut(a, b) :: tl -> Mut(a,b) :: tl 
  |x :: tl -> remove_upto_next tl count
  |_ ->[ComError]

let rec remove_fun (src : com list) (count_ret : int) (acc : com list) (count_end : int): com list * com list  = 
  if count_ret = 0 && count_end = 0 then (remove_upto_next src count_end , List.rev (EndBegin :: acc)) else
  match src with
  |Fun(a,b) :: tl -> remove_fun tl ((+) count_ret 1) (Fun(a, b) :: acc) ((+) count_end 1)
  |Mut(a,b) :: tl -> remove_fun tl ((+) count_ret 1) (Mut(a,  b) :: acc) count_end
  |Return :: tl -> remove_fun tl ((-) count_ret 1) (Return :: acc) count_end
  |IfThen :: tl -> remove_fun tl ((+) count_ret 1) (IfThen :: acc) ((+) count_end 1)
  |CaseLeft :: tl -> remove_fun tl ((+) count_ret 1) (Begin :: acc) ((+) count_end 1)
  |Begin :: tl -> remove_fun tl (count_ret) (Begin :: acc) ((+) count_end 1)
  |End :: tl -> if count_end = 0 then (tl, List.rev(EndBegin :: acc)) else remove_fun tl count_ret (End :: acc) ((-) count_end 1)
  |x :: tl -> remove_fun tl count_ret (x :: acc) count_end
  |_ -> ([ComError], acc)

let no_mut (src : com list) : bool = 
  List.fold_left (fun acc x -> match x with
  |Mut(a, b) -> false && acc
  |_ -> true && acc
  ) true src 

let rec remove_mut (src : com list) (count : int) (acc : com list): com list * com list  = 
  match src with
  |Fun(a,b) :: tl -> remove_mut tl ((+) count 1) (Fun(a, b) :: acc)
  |Mut(a,b) :: tl -> remove_mut tl ((+) count 1) (Mut(a, b) :: acc)
  |Return :: tl -> if count = 0 then (if no_mut tl then (find_end tl 0, List.rev (End :: acc)) else (find_end tl 0, List.rev (End :: acc)))  else remove_mut tl ((-) count 1) (Return :: acc)
  |IfThen :: tl -> remove_mut tl ((+) count 1 ) (IfThen :: acc)
  |CaseLeft :: tl -> remove_mut tl ((+) count 1) (CaseLeft :: acc)
  |x :: tl -> remove_mut tl count (x :: acc)
  |_ -> ([ComError], [])
  
let add_fun (src : com list) (local : const list) (fun_name : const) (fun_arg : const) : const list =
  if List.mem fun_name local then fun_name :: Closure(fun_name, fun_arg,(extract_fun (remove_fun src 1 [] 0))) :: replace_store fun_name local else fun_name :: Closure(fun_name, fun_arg,(extract_fun (remove_fun src 1 [] 0))) :: local

let rec exec_begin (src_com : com list) (stack : const list) (local : const list) (global : const list): const list * const list=
  match (exec_com_list (List.rev (begin_coms src_com 0 [])) [] local global) with
  |(x :: y, z) -> if check_error (x::y) then (x :: stack, z) else ([ConstError],z)
  |_ -> ([ConstError], global)

and call (stack : const list) (local : const list) (global : const list) : const list * const list = 
  match stack with
  |Closure(a, b, c) :: x :: tl -> exec_begin c tl (exec_store b [x] local) global
  |_ -> ([ConstError], [])

and exec_com_list (src_com : com list) (stack : const list) (local : const list) (global : const list) : const list * const list = 
  match src_com with
  |End :: tl -> exec_com_list tl stack local global
  |_ -> match src_com with
  
  | Push(a) :: tl-> exec_com_list tl (push a stack local global) local global
  | Pop :: tl -> exec_com_list tl (pop stack) local global
  | Add :: tl -> exec_com_list tl (add stack) local global 
  | Sub :: tl -> exec_com_list tl (sub stack) local global
  | Mul :: tl -> exec_com_list tl (mul stack) local global
  | Div :: tl -> exec_com_list tl (div stack) local global
  | Swap :: tl -> exec_com_list tl (swap stack) local global
  | Neg :: tl -> exec_com_list tl (neg stack) local global
  | Concat :: tl -> exec_com_list tl (concat stack) local global
  | And :: tl -> exec_com_list tl (pand stack) local global
  | Or :: tl -> exec_com_list tl (por stack) local global
  | Not :: tl -> exec_com_list tl (pnot stack) local global
  | Equal :: tl -> exec_com_list tl (pequal stack) local global
  | Lte :: tl -> exec_com_list tl (lte stack) local global
  | ComError :: tl -> exec_com_list tl (comerror stack) local global
  | Local(a) :: tl -> exec_com_list tl (pop stack) (exec_store a stack local) global
  | Global(a) :: tl -> exec_com_list tl (pop stack) local (exec_store a stack global)
  | Begin :: tl -> exec_com_list (find_end tl 0) (extract_stack (exec_begin tl stack local global)) local (extract_global (exec_begin tl stack local global))
  | IfThen :: tl -> if first_elem stack = 1 then exec_com_list (find_else tl 0) (pop stack) local global else if first_elem stack = 0 then exec_com_list (remove_ifthen tl 0) (pop stack) local global else ([ConstError], global)
  | End :: tl -> (stack , global)
  | EndBegin :: [] -> (stack, global)
  | InjL :: tl -> exec_com_list tl (injl stack) local global
  | InjR :: tl -> exec_com_list tl (injr stack) local global
  | CaseLeft :: tl -> if first_union stack = 0 then exec_com_list (find_right tl 0) (remove_union stack) local global else if first_union stack= 1 then exec_com_list (remove_left_branch tl 0) (remove_union stack) local global else ([ConstError], global)
  | Tuple(a) :: tl -> if a > List.length stack then ([ConstError], global) else exec_com_list tl (init_tuple stack a) local global
  | Get(a) :: tl -> if check_tuple stack then exec_com_list tl (get stack a) local global else ([ConstError], global)
  | Fun(a, b) :: tl -> exec_com_list (extract_main (remove_fun tl 1 [] 0)) (stack) (add_fun tl local a b) global
  | Mut(a, b) :: tl -> exec_com_list (extract_main (remove_mut tl 0 [])) (stack) (add_fun tl local a b) global
  | Call :: tl -> exec_com_list tl (extract_stack (call stack local global)) local (extract_global (call stack local global))
  | Return :: tl -> exec_com_list tl stack local global
  | Quit :: _ -> (stack, global) 
  | _ -> (NoQuit :: stack, global)
     
let rec const_to_string (src_const : const list) : string list =
  match src_const with 
  |[] -> []
  |String(x) :: y -> x :: const_to_string y
  |Int(x) :: y -> string_of_int x :: const_to_string y
  |Left(x) :: y -> ("Left " ^ remove_const x) :: const_to_string y 
  |Right(x) :: y -> ("Right " ^ remove_const x ) :: const_to_string y
  |Closure(a , b, c) :: y -> ("Clo (" ^ remove_const a ^ ", " ^ remove_const b ^ ")") :: const_to_string y
  |_ -> ["Error"]
 

let write_file_with_log (file_path: string) (log: string list) : unit =
  let fp = open_out file_path in
  let (),_ = List.fold_left(fun ((), num_ele) x -> match num_ele with
  |1 -> ((Printf.fprintf (fp) "%s" x), num_ele - 1)
  |_ -> ((Printf.fprintf (fp) "%s\n" x), num_ele - 1)
  ) ((), List.length log) log
  in close_out fp

let interpreter (src : string) (output_file_path: string): unit =
  match (extract_stack (exec_com_list (com_list (split_string src)) [] [] [])) with 
  |NoQuit :: y -> if check_error y then write_file_with_log output_file_path [] else write_file_with_log output_file_path ["\"Error\""]
  |x -> if check_error x then write_file_with_log output_file_path (const_to_string x) else write_file_with_log output_file_path ["\"Error\""]
  




