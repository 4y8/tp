open Trie

let mem s trie =
  let n = String.length s in
  let rec aux i t =
      match t with
        Empty -> false
      | Node (b, t) ->
        if i = n then
          b
        else
          match s.[i] with
            'A' -> aux (i + 1) t.a
          | 'C' -> aux (i + 1) t.c
          | 'G' -> aux (i + 1) t.g
          | 'T' -> aux (i + 1) t.t
          | _ -> failwith "impossible"
  in aux 0 (Node (false, trie))

let q1 u0 =
  print_endline (string_of_bool (mem (Trie.read_text (Printf.sprintf "%d/chaine_2.txt" u0)) Trie.simple_trie));
  print_endline (string_of_bool (mem (Trie.read_text (Printf.sprintf "%d/chaine_3.txt" u0)) Trie.simple_trie))

let base4 = function
    'a' -> 0
  | 'c' -> 1
  | 'g' -> 2
  | 't' -> 3
  | _ -> failwith "impossible"

let find_first s t =
  let n = String.length s in
  let rec aux i = function
      Empty -> None
    | Node (true, _) -> Some i
    | Node (_, t) ->
      if i >= n then None
        else
          match s.[i] with
            'A' -> aux (i + 1) t.a
          | 'C' -> aux (i + 1) t.c
          | 'G' -> aux (i + 1) t.g
          | 'T' -> aux (i + 1) t.t
          | _ -> failwith "impossible"
  in
  let rec loop i =
    if i = n then None
    else
      match aux i (Node (false, t)) with
        None -> loop (i + 1)
      | Some j -> Some (i, String.sub s i (j - i))
  in
  loop 0

let q2 u0 =
  let print l =
    match find_first (read_text (Printf.sprintf "%d/chaine_%d.txt" u0 l)) simple_trie with
      None -> print_endline "None"
    | Some (pos, w) -> Printf.printf "Some (%d, %s)\n" pos w
  in
  print 10;
  print 100;
  print 1000;
  print 10000000

let size t =
  let rec size = function
    Empty -> 0
  | Node (_, t) ->
    1 + (size t.a) + (size t.c) + (size t.g) + (size t.t)
  in size (Node (false, t)) - 1

let emptrie = { a = Empty ; c = Empty ; g = Empty ; t = Empty }

let node t = function
    'a' -> t.a
  | 'c' -> t.c
  | 'g' -> t.g
  | 't' -> t.t
  | _ -> failwith "impossible"

let insert s t =
  let n = String.length s in
  let rec aux i t =
    if i = n then
      match t with
        Empty -> Node (true, emptrie)
      | Node (_, t) -> Node (true, t)
    else
      match t with
        Empty -> begin
          match s.[i] with
            'A' -> Node (false, {emptrie with a = aux (i + 1) (Node (false, emptrie))})
          | 'C' -> Node (false, {emptrie with c = aux (i + 1) (Node (false, emptrie))})
          | 'G' -> Node (false, {emptrie with g = aux (i + 1) (Node (false, emptrie))})
          | 'T' -> Node (false, {emptrie with t = aux (i + 1) (Node (false, emptrie))})
          | _ -> failwith "impossible"
        end
      | Node (b, t) ->  begin
          match s.[i] with
            'A' -> Node (b, {t with a = aux (i + 1) t.a})
          | 'C' -> Node (b, {t with c = aux (i + 1) t.c})
          | 'G' -> Node (b, {t with g = aux (i + 1) t.g})
          | 'T' -> Node (b, {t with t = aux (i + 1) t.t})
          | _ -> failwith "impossible"
        end
  in
  match aux 0 (Node (false, t)) with
    Node (_, t) -> t
  | _ -> failwith "impossible"

let make_trie l =
  let rec aux acc = function
    [] -> acc
  | hd :: tl -> aux (insert hd acc) tl
  in aux emptrie l

let q3 u0 =
  let print n =
    Printf.printf "%d\n" (size (make_trie (read_motif (Printf.sprintf "%d/motif_%d.txt" u0 n))) mod 10000)
  in
  print 5;
  print 10;
  print 100;
  print 1000

let q4 u0 =
  let print n l =
    match find_first (read_text (Printf.sprintf "%d/chaine_%d.txt" u0 l)) (make_trie (read_motif (Printf.sprintf "%d/motif_%d.txt" u0 n))) with
      None -> print_endline "None"
    | Some (pos, w) -> Printf.printf "Some (%d, %s)\n" pos w
  in
  print 10 1000;
  print 10 10000000


let find_first s t =
  let n = String.length s in
  let rec aux i = function
      Empty -> None
    | Node (true, _) -> Some i
    | Node (_, t) ->
      if i >= n then None
        else
          match s.[i] with
            'A' -> aux (i + 1) t.a
          | 'C' -> aux (i + 1) t.c
          | 'G' -> aux (i + 1) t.g
          | 'T' -> aux (i + 1) t.t
          | _ -> failwith "impossible"
  in
  let rec loop i =
    if i = n then None
    else
      match aux i (Node (false, t)) with
        None -> loop (i + 1)
      | Some j -> Some (i, String.sub s i (j - i))
  in
  loop 0

let find_all s t =
  let l = ref [] in
  let c = ref 0 in
  let n = String.length s in
  let rec aux pos i = function
      Empty -> ()
    | Node (b, t) ->
      (*if b then l := (pos, String.sub s pos (i - pos)) :: !l; decommenter cette ligne si on veut l*)
      if b then incr c;
      if i >= n then ()
      else
        match s.[i] with
          'A' -> aux pos (i + 1) t.a
        | 'C' -> aux pos (i + 1) t.c
        | 'G' -> aux pos (i + 1) t.g
        | 'T' -> aux pos (i + 1) t.t
        | _ -> failwith "impossible"
  in
  for i = 0 to n - 1 do
    aux i i (Node (false, t))
  done;
  !c

let trieN n u0 =
  make_trie (read_motif (Printf.sprintf "%d/motif_%d.txt" u0 n))

let q5 u0 =
  let print n l =
    Printf.printf "%d\n" (find_all (read_text (Printf.sprintf "%d/chaine_%d.txt" u0 l)) (trieN n u0) mod 10000)
  in
  print 10 100;
  print 10 1000;
  print 10 5000000;
  print 1000 1000000

let _ =
  q5 2000
