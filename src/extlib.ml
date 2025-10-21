let[@inline] min3 (x:int) (y:int) (z:int) = min (min x y) z

module List = struct
  include List
  
  (** Compute the Levenstein distance of two lists. *)
  let levenstein s t =
    let n = List.length t in
    let rec aux d' d s i =
      match s with
      | x::s ->
         d.(0) <- i;
         List.iteri
           (fun j y ->
             let c = if x = y then 0 else 1 in
             d.(j+1) <- min3 (d'.(j+1)+1) (d.(j)+1) (d'.(j)+c)
           ) t;
         aux d d' s (i+1)
      | [] -> d'.(n)
    in
    let d' = Array.init (n+1) (fun j -> j) in
    let d = Array.make (n+1) 0 in
    aux d' d s 1

  let similarity s t =
    let n = max (List.length s) (List.length t) in
    float (n - levenstein s t) /. float n

  (** All ordered pairs. *)
  let rec pairs = function
    | x::l -> (List.map (fun y -> (x,y)) l)@(pairs l)
    | [] -> []
end

module String = struct
  include String

  (** Compute the Levenstein distance of two strings. *)
  (* See https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance *)
  let levenstein ?(limit=max_int) s t =
    let m = String.length s in
    let n = String.length t in
    (* Compute next row d from previous row d'. *)
    let rec aux d' d i =
      if i > m then d'.(n) else
        (
          d.(0) <- i;
          (* minimum over the row *)
          let dmin = ref i in
          for j = 1 to n do
            let c = if s.[i-1] = t.[j-1] then 0 else 1 in
            d.(j) <- min3
                       (d'.(j)+1)    (* deletion *)
                       (d.(j-1)+1)   (* insertion *)
                       (d'.(j-1)+c); (* substitution *)
            dmin := min !dmin d.(j)
          done;
          if !dmin >= limit then limit else aux d d' (i+1)
        )
    in
    let d' = Array.init (n+1) (fun j -> j) in
    let d = Array.make (n+1) 0 in
    aux d' d 1

  let () = assert (levenstein "kitten" "sitting" = 3)
  let () = assert (levenstein "ca" "abc" = 3)

  (** Similarity ratio of two strings. *)
  let similarity ?(kind=`Levenstein) s t =
    let f =
      match kind with
      | `Levenstein -> levenstein
      | `OSA -> edit_distance
    in
    let n = max (String.length s) (String.length t) in
    let k = f ~limit:(n / 2) s t in
    float (n - k) /. float n
end

module File = struct
  (** Read a whole file. *)
  let read fname =
    let ic = open_in fname in
    let len = in_channel_length ic in
    let ans = really_input_string ic len in
    close_in ic;
    ans
end
