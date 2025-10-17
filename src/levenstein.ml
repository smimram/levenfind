(* Former by hand implementation of the algorithm *)

let min3 (x:int) (y:int) (z:int) = min (min x y) z

module List = struct
  include List

  (** Compute the Levenstein distance of two lists. *)
  let levenstein s t =
    let m = List.length s in
    let n = List.length t in
    let d = Array.make_matrix (m+1) (n+1) 0 in
    for i = 1 to m do d.(i).(0) <- i done;
    for j = 1 to n do d.(0).(j) <- j done;
    iteri (fun j y ->
        iteri (fun i x ->
            let c = if x = y then 0 else 1 in
            d.(i+1).(j+1) <- min3 (d.(i).(j+1)+1) (d.(i+1).(j)+1) (d.(i).(j)+c)
          ) s
      ) t;
    d.(m).(n)

  let similarity s t =
    let n = max (List.length s) (List.length t) in
    float (n - levenstein s t) /. float n
end

module String = struct
  include String

  (** Compute the Levenstein distance of two strings. *)
  let levenstein s t =
    let m = String.length s in
    let n = String.length t in
    let d = Array.make_matrix (m+1) (n+1) 0 in
    for i = 1 to m do d.(i).(0) <- i done;
    for j = 1 to n do d.(0).(j) <- j done;
    for j = 1 to n do
      for i = 1 to m do
        let c = if s.[i-1] = t.[j-1] then 0 else 1 in
        d.(i).(j) <- min3 (d.(i-1).(j)+1) (d.(i).(j-1)+1) (d.(i-1).(j-1)+c)
      done
    done;
    d.(m).(n)

  (** Similarity ratio of two strings. *)
  let similarity s t =
    let n = max (String.length s) (String.length t) in
    float (n - levenstein s t) /. float n
end
