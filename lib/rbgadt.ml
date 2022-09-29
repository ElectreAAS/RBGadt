type black
type red
type z = Z
type _ s = S : 'n -> 'n s

(* type _ color = Black : black color | Red : red color *)
(* type (_, _) child = Black : (red, black) child | Any : (black, 'c) child *)

type (_, _, _) node =
  | Empty : (_, black, z) node
  | BNode : ('a, 'b, 'n) node * 'a * ('a, 'c, 'n) node -> ('a, black, 'n s) node
  | RNode :
      ('a, black, 'n) node * 'a * ('a, black, 'n) node
      -> ('a, red, 'n) node

type (_, _, _) either =
  | Left : ('a, 'b, 'n) node -> ('a, 'b, 'n) either
  | Right : ('a, 'b, 'n s) node -> ('a, 'b, 'n) either

let rec mem : type b n. 'a -> ('a, b, n) node -> bool =
 fun e n ->
  match n with
  | Empty -> false
  | BNode (l, x, r) -> (
      match compare e x with -1 -> mem e l | 0 -> true | _ -> mem e r)
  | RNode (l, x, r) -> (
      match compare e x with -1 -> mem e l | 0 -> true | _ -> mem e r)

let rec add : type b n. 'a -> ('a, b, n) node -> ('a, b, n) either =
 fun e t ->
  match t with
  | Empty ->
      let rnode = RNode (Empty, e, Empty) in
      Left rnode
  | BNode (l, x, r) -> (
      match compare e x with
      | 0 -> Left t
      | -1 -> (
          match add e l with Left _ -> failwith "" | Right _ -> failwith "")
      | _ -> (
          match add e r with Left _ -> failwith "" | Right _ -> failwith ""))
  | RNode _ -> failwith ""

type _ t = T : ('a, _, _) node -> 'a t

let mem e (T t) = mem e t
let add e (T t) = match add e t with Left n -> T n | Right n -> T n
let empty = T Empty