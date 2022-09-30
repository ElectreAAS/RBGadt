type black
type red
type z
type _ s = S
(* type _ color = Black : black color | Red : red color
   type _ child = Only : black color -> red child | Any : 'c color -> black child *)

type (_, _, _) node =
  | Empty : (_, black, z) node
  (* | Node : 'b color * _ * 'a * _ -> ('a, 'b, _) node *)
  | BNode : ('a, _, 'n) node * 'a * ('a, _, 'n) node -> ('a, black, 'n s) node
  | RNode :
      ('a, black, 'n) node * 'a * ('a, black, 'n) node
      -> ('a, red, 'n) node

(* type (_, _) skew = Left : (red, black) skew | Right : (black, red) skew *)

type (_, _, _) result =
  | Same : ('a, 'b, 'n) node -> ('a, 'b, 'n) result
  | Spawn : ('a, red, 'n) node -> ('a, black, 'n) result
  | Overflow :
      ('a, black, 'n) node
      * 'a
      * ('a, black, 'n) node
      * 'a
      * ('a, black, 'n) node
      -> ('a, red, 'n) result

let rec mem : type b n. 'a -> ('a, b, n) node -> bool =
 fun e n ->
  match n with
  | Empty -> false
  | BNode (l, x, r) -> (
      match compare e x with -1 -> mem e l | 0 -> true | _ -> mem e r)
  | RNode (l, x, r) -> (
      match compare e x with -1 -> mem e l | 0 -> true | _ -> mem e r)

let rec cons : type b n. 'a -> ('a, b, n) node -> ('a, b, n) result =
 fun elem t ->
  match t with
  | Empty -> Spawn (RNode (Empty, elem, Empty))
  | BNode (l, x, r) -> (
      match cons elem l with
      | Same new_l -> Same (BNode (new_l, x, r))
      | Spawn new_l -> Same (BNode (new_l, x, r))
      | Overflow (a, b, c, d, e) ->
          Spawn (RNode (BNode (a, b, c), d, BNode (e, x, r))))
  | RNode (l, x, r) -> (
      match cons elem l with
      | Same new_l -> Same (RNode (new_l, x, r))
      | Spawn (RNode (a, b, c)) -> Overflow (a, b, c, x, r))

let rec snoc : type b n. 'a -> ('a, b, n) node -> ('a, b, n) result =
 fun e t ->
  match t with
  | Empty -> Spawn (RNode (Empty, e, Empty))
  | BNode (l, x, r) -> (
      match snoc e r with
      | Same new_r -> Same (BNode (l, x, new_r))
      | Spawn new_r -> Same (BNode (l, x, new_r))
      | Overflow (a, b, c, d, e) ->
          Spawn (RNode (BNode (l, x, a), b, BNode (c, d, e))))
  | RNode (l, x, r) -> (
      match snoc e r with
      | Same new_r -> Same (RNode (l, x, new_r))
      | Spawn (RNode (a, b, c)) -> Overflow (l, x, a, b, c))

let rec add : type b n. 'a -> ('a, b, n) node -> ('a, b, n) result =
 fun e t ->
  match t with
  | Empty -> Spawn (RNode (Empty, e, Empty))
  | BNode (l, x, r) -> (
      match compare e x with
      | 0 -> Same t
      | -1 -> (
          match add e l with
          | Same new_l -> Same (BNode (new_l, x, r))
          | Spawn new_l -> Same (BNode (new_l, x, r))
          | Overflow (a, b, c, d, e) ->
              Spawn (RNode (BNode (a, b, c), d, BNode (e, x, r))))
      | _ -> (
          match add e r with
          | Same new_r -> Same (BNode (l, x, new_r))
          | Spawn new_r -> Same (BNode (l, x, new_r))
          | Overflow (a, b, c, d, e) ->
              Spawn (RNode (BNode (l, x, a), b, BNode (c, d, e)))))
  | RNode (l, x, r) -> (
      match compare e x with
      | 0 -> Same t
      | -1 -> (
          match add e l with
          | Same new_l -> Same (RNode (new_l, x, r))
          | Spawn (RNode (a, b, c)) -> Overflow (a, b, c, x, r))
      | _ -> (
          match add e r with
          | Same new_r -> Same (RNode (l, x, new_r))
          | Spawn (RNode (a, b, c)) -> Overflow (l, x, a, b, c)))

let save_to_dot string_of_elem t filename =
  let oc = open_out filename in
  let rec pr_node : type b n. ('a, b, n) node -> unit = function
    | Empty -> ()
    | BNode (l, x, r) -> (
        Printf.fprintf oc
          "\t%s [style=filled shape=circle color=black fontcolor=white];\n"
          (string_of_elem x);
        (match l with
        | Empty -> ()
        | BNode (_, lx, _) | RNode (_, lx, _) ->
            Printf.fprintf oc "\t%s -> %s\n" (string_of_elem x)
              (string_of_elem lx);
            pr_node l);
        match r with
        | Empty -> ()
        | BNode (_, rx, _) | RNode (_, rx, _) ->
            Printf.fprintf oc "\t%s -> %s\n" (string_of_elem x)
              (string_of_elem rx);
            pr_node r)
    | RNode (l, x, r) -> (
        Printf.fprintf oc
          "\t%s [style=filled shape=circle color=red fontcolor=white];\n"
          (string_of_elem x);
        (match l with
        | Empty -> ()
        | BNode (_, lx, _) ->
            Printf.fprintf oc "\t%s -> %s\n" (string_of_elem x)
              (string_of_elem lx);
            pr_node l);
        match r with
        | Empty -> ()
        | BNode (_, rx, _) ->
            Printf.fprintf oc "\t%s -> %s\n" (string_of_elem x)
              (string_of_elem rx);
            pr_node r)
  in
  Printf.fprintf oc "digraph {\n";
  pr_node t;
  Printf.fprintf oc "}\n%!";
  close_out oc

type _ t = T : ('a, _, _) node -> 'a t

let mem e (T t) = mem e t

let add e (T t) =
  match add e t with
  | Spawn n -> T n
  | Same n -> T n
  | Overflow (a, b, c, d, e) -> T (BNode (RNode (a, b, c), d, e))

let cons e (T t) =
  match cons e t with
  | Same n -> T n
  | Spawn n -> T n
  | Overflow (a, b, c, d, e) -> T (BNode (RNode (a, b, c), d, e))

let save_to_dot string_of_elem (T t) filename =
  save_to_dot string_of_elem t filename

let () =
  let t = ref (T Empty) in
  let l = [ 15; 13; 11; 9; 7; 5; 3; 1; 14; 10; 6; 2; 12; 4; 8 ] in
  List.iter (fun i -> t := add i !t) l;
  save_to_dot string_of_int !t "output.dot"