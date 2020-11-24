module List = struct
  include List

  (** Iterate a function on ordered pairs. *)
  let rec iter_pairs f = function
    | x::l -> List.iter (f x) l; iter_pairs f l
    | [] -> ()
end

let min3 (x:int) (y:int) (z:int) = min (min x y) z

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

let similarity s t =
  let n = max (String.length s) (String.length t) in
  float (n - levenstein s t) /. float n

exception Error of string

let read_all fname =
  let ic = open_in fname in
  let len = in_channel_length ic in
  if len > 100000 then (close_in ic; raise (Error ("File too big: "^fname)));
  let ans = really_input_string ic len in
  close_in ic;
  ans

let () = assert (levenstein "kitten" "sitting" = 3)

let verbose = ref true
let threshold = ref 0.6
let extension = ref ""
let directories = ref []
let recursive = ref true

let warning f = Printf.ksprintf (fun s -> if !verbose then (print_string s; flush stdout)) f

let rec find_files ?(recursive=false) dir =
  (* Printf.printf "find in %s\n%!" dir; *)
  let d, f = Array.to_list (Sys.readdir dir) |> List.map (fun d -> dir ^ "/" ^ d) |> List.partition Sys.is_directory in
  let f = List.filter (fun f -> Filename.check_suffix f !extension) f in
  if recursive then f@(List.flatten (List.map (find_files ~recursive) d))
  else f

let () =
  Arg.parse
    [
      "--extension", Arg.Set_string extension, "Consider only files with given extension.";
      "--non-recursive", Arg.Unit (fun () -> recursive := false), "Do not recurse into folders.";
      "--threshold", Arg.Float (fun x -> threshold := x /. 100.), "Threshold above which matching files are displayed (between 0 and 100%)."
    ] (fun s -> directories := s :: !directories) "afind [options] [directory]";
  let directories = if !directories = [] then ["."] else !directories in
  let files = List.map (find_files ~recursive:!recursive) directories |> List.flatten in
  List.iter (Printf.printf "Considering %s\n%!") files;
  let k = ref 0 in
  let kmax =
    let n = List.length files in
    n * (n-1) / 2
  in
  List.iter_pairs
    (fun fs ft ->
       try
         incr k;
         Printf.printf "\r%.02f%%%!" (float (!k * 100) /. float kmax);
         let s = read_all fs in
         let t = read_all ft in
         let d = similarity s t in
         if d >= !threshold then Printf.printf "\n%s / %s: %.02f%%\n%!" fs ft (100. *. d);
       with
       | Error e -> warning "%s\n%!" e
    ) files;
  print_newline ()
