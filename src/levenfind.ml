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
  
  (** Iterate a function on ordered pairs. *)
  let rec iter_pairs f = function
    | x::l -> List.iter (f x) l; iter_pairs f l
    | [] -> ()

  (** All ordered pairs. *)
  let rec pairs = function
    | x::l -> (List.map (fun y -> (x,y)) l)@(pairs l)
    | [] -> []
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

exception Error of string

(** Read a whole file. *)
let read_all fname =
  let ic = open_in fname in
  let len = in_channel_length ic in
  let ans = really_input_string ic len in
  close_in ic;
  ans

let () = assert (String.levenstein "kitten" "sitting" = 3)

let domains = ref (max 1 (Domain.recommended_domain_count () - 1))
let lines = ref false
let verbose = ref true
let threshold = ref 0.6
let extensions = ref []
let directories = ref []
let recursive = ref true
let max_file_size = ref (32 * 1024)
let exclude = ref [] (* regexp for filenames to exclude *)
let log_file = ref ""

let warning f = Printf.ksprintf (fun s -> if !verbose then (print_string s; flush stdout)) f

let rec find_files ?(recursive=false) dir =
  (* Printf.printf "find in %s\n%!" dir; *)
  let f = Sys.readdir dir |> Array.to_list |> List.map (fun d -> dir ^ "/" ^ d) in
  let f = List.filter (fun f -> not (List.exists (fun re -> Str.string_match re (Filename.basename f) 0) !exclude)) f in
  let d, f = f |> List.partition Sys.is_directory in
  let f = List.filter (fun f -> !extensions = [] || List.exists (Filename.check_suffix f) !extensions) f in
  if recursive then f@(List.flatten (List.map (find_files ~recursive) d))
  else f

let () =
  Arg.parse
    (Arg.align [
        "--extension", Arg.String (fun ext -> extensions := ext :: !extensions), " Consider only files with given extension.";
        "--exclude", Arg.String (fun e -> exclude := Str.regexp (e^"$") :: !exclude), " Exclude files whose name match the given regular expression.";
        "--lines", Arg.Set lines, " Compare lines instead of characters (faster but less precise).";
        "--log", Arg.Set_string log_file, " Save output in given log file.";
        "--non-recursive", Arg.Unit (fun () -> recursive := false), " Do not recurse into folders.";
        "--parallelism", Arg.Set_int domains, " Number of threads to be run concurrently.";
        "--quiet", Arg.Unit (fun () -> verbose := false), " Do not display warnings.";
        "--size", Arg.Set_int max_file_size, Printf.sprintf " Maximum file size in octets (default: %d)." !max_file_size;
        "--threshold", Arg.Float (fun x -> threshold := x /. 100.), (Printf.sprintf " Threshold above which matching files are displayed (between 0 and 100%%, default is %.00f%%)." (!threshold *. 100.))
      ]) (fun s -> directories := s :: !directories) "levenfind [options] [directory]";
  let directories = if !directories = [] then ["."] else !directories in
  let files = List.map (find_files ~recursive:!recursive) directories |> List.flatten in
  let files =
    let p fname =
      try
        if (Unix.stat fname).Unix.st_size > !max_file_size then
          (
            Printf.printf "Too big: %s\n" fname;
            false
          )
        else true
      with _ -> false
    in
    List.filter p files
  in
  let num_domains = !domains in
  if !verbose then
    (
      Printf.printf "Using %d domains\n%!" num_domains;
      List.iter (Printf.printf "Considering %s\n%!") files
    );
  let files2 = List.pairs files |> Array.of_list in
  let k = Atomic.make 0 in
  let kmax = Array.length files2 in
  let log =
    let m = Mutex.create () in
    let oc = if !log_file = "" then None else Some (open_out !log_file) in
    fun fmt ->
      Printf.ksprintf
        (fun s ->
           Mutex.lock m;
           print_string s;
           Option.iter (fun oc -> output_string oc s; flush oc) oc;
           Mutex.unlock m
        ) fmt
  in
  let check i =
    let fs,ft = files2.(i) in
    try
      let k = Atomic.fetch_and_add k 1 in
      (* Printf.printf "\r%.02f%% (%d / %d): %s vs %s%!" (float (k * 100) /. float kmax) k kmax fs ft; *)
      Printf.printf "\r%.02f%% (%d / %d)%!" (float (k * 100) /. float kmax) k kmax;
      let s = read_all fs in
      let t = read_all ft in
      let d =
        if !lines then List.similarity (String.split_on_char '\n' s) (String.split_on_char '\n' t)
        else String.similarity s t
      in
      if d >= !threshold then log "\nFound %s / %s: %.02f%%\n" fs ft (100. *. d)
        (* log "\n%.02f%% similarity:\n- %s\n- %s\n" (100. *. d) fs ft *)
    with
    | Error e -> warning "%s\n%!" e

  in
  let task =
    let i = Atomic.make 0 in
    let l = Array.length files2 in
      fun () ->
        while Atomic.get i < l do
          let i = Atomic.fetch_and_add i 1 in
          if i < l then check i;
          if i mod 10 = 0 then Gc.full_major ();
          Domain.cpu_relax ()
        done
  in
  let t = Sys.time () in
  let domains = List.init num_domains (fun _ -> Domain.spawn task) in
  List.iter Domain.join domains;
  print_newline ();
  Printf.printf "Compared %d files in %.02f seconds.\n%!" (List.length files) (Sys.time () -. t)
