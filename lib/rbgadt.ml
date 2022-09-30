type black
type red
type z
type _ s = S

type (_, _, _, _, _) color =
  | Red : (red, black, black, 'n, 'n) color
  | Black : (black, 'c, 'd, 'n, 'n s) color

type (_, _, _) node =
  | Empty : (_, black, z) node
  | Node :
      ('b, 'c, 'd, 'n, 'm) color * ('a, 'c, 'n) node * 'a * ('a, 'd, 'n) node
      -> ('a, 'b, 'm) node

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
  | Node (_, l, x, r) -> (
      match compare e x with -1 -> mem e l | 0 -> true | _ -> mem e r)

let rec cons : type b n. 'a -> ('a, b, n) node -> ('a, b, n) result =
 fun elem t ->
  match t with
  | Empty -> Spawn (Node (Red, Empty, elem, Empty))
  | Node (Black, l, x, r) -> (
      match cons elem l with
      | Same new_l -> Same (Node (Black, new_l, x, r))
      | Spawn new_l -> Same (Node (Black, new_l, x, r))
      | Overflow (a, b, c, d, e) ->
          Spawn (Node (Red, Node (Black, a, b, c), d, Node (Black, e, x, r))))
  | Node (Red, l, x, r) -> (
      match cons elem l with
      | Same new_l -> Same (Node (Red, new_l, x, r))
      | Spawn (Node (Red, a, b, c)) -> Overflow (a, b, c, x, r))

let rec snoc : type b n. 'a -> ('a, b, n) node -> ('a, b, n) result =
 fun e t ->
  match t with
  | Empty -> Spawn (Node (Red, Empty, e, Empty))
  | Node (Black, l, x, r) -> (
      match snoc e r with
      | Same new_r -> Same (Node (Black, l, x, new_r))
      | Spawn new_r -> Same (Node (Black, l, x, new_r))
      | Overflow (a, b, c, d, e) ->
          Spawn (Node (Red, Node (Black, l, x, a), b, Node (Black, c, d, e))))
  | Node (Red, l, x, r) -> (
      match snoc e r with
      | Same new_r -> Same (Node (Red, l, x, new_r))
      | Spawn (Node (Red, a, b, c)) -> Overflow (l, x, a, b, c))

let rec add : type b n. 'a -> ('a, b, n) node -> ('a, b, n) result =
 fun e t ->
  match t with
  | Empty -> Spawn (Node (Red, Empty, e, Empty))
  | Node (Black, l, x, r) -> (
      match compare e x with
      | 0 -> Same t
      | -1 -> (
          match add e l with
          | Same new_l -> Same (Node (Black, new_l, x, r))
          | Spawn new_l -> Same (Node (Black, new_l, x, r))
          | Overflow (a, b, c, d, e) ->
              Spawn
                (Node (Red, Node (Black, a, b, c), d, Node (Black, e, x, r))))
      | _ -> (
          match add e r with
          | Same new_r -> Same (Node (Black, l, x, new_r))
          | Spawn new_r -> Same (Node (Black, l, x, new_r))
          | Overflow (a, b, c, d, e) ->
              Spawn
                (Node (Red, Node (Black, l, x, a), b, Node (Black, c, d, e)))))
  | Node (Red, l, x, r) -> (
      match compare e x with
      | 0 -> Same t
      | -1 -> (
          match add e l with
          | Same new_l -> Same (Node (Red, new_l, x, r))
          | Spawn (Node (Red, a, b, c)) -> Overflow (a, b, c, x, r))
      | _ -> (
          match add e r with
          | Same new_r -> Same (Node (Red, l, x, new_r))
          | Spawn (Node (Red, a, b, c)) -> Overflow (l, x, a, b, c)))

let save_to_dot string_of_elem t filename =
  let oc = open_out filename in
  let rec pr_node : type b n. ('a, b, n) node -> unit = function
    | Empty -> ()
    | Node (color, l, x, r) -> (
        let string_of_color =
          match color with Black -> "black" | Red -> "red"
        in
        Printf.fprintf oc
          "\t%s [style=filled shape=circle color=%s fontcolor=white];\n"
          (string_of_elem x) string_of_color;
        (match l with
        | Empty -> ()
        | Node (Black, _, lx, _) | Node (Red, _, lx, _) ->
            Printf.fprintf oc "\t%s -> %s\n" (string_of_elem x)
              (string_of_elem lx);
            pr_node l);
        match r with
        | Empty -> ()
        | Node (Black, _, rx, _) | Node (Red, _, rx, _) ->
            Printf.fprintf oc "\t%s -> %s\n" (string_of_elem x)
              (string_of_elem rx);
            pr_node r)
  in
  Printf.fprintf oc "digraph {\n";
  pr_node t;
  Printf.fprintf oc "}\n%!";
  close_out oc

type _ t = T : ('a, black, _) node -> 'a t

let mem e (T t) = mem e t

let destruct : type b. ('a, black, b) result -> 'a t = function
  | Same n -> T n
  | Spawn (Node (Red, l, x, r)) -> T (Node (Black, l, x, r))

let add e (T t) = destruct @@ add e t
let cons e (T t) = destruct @@ cons e t

let save_to_dot string_of_elem (T t) filename =
  save_to_dot string_of_elem t filename

let () =
  let t = ref (T Empty) in
  let l = [ 15; 13; 11; 9; 7; 5; 3; 1; 14; 10; 6; 2; 12; 4; 8 ] in
  List.iter (fun i -> t := add i !t) l;
  save_to_dot string_of_int !t "output.dot"