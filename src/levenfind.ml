(* See https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance *)

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
           ) s;
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

  (** Similarity ratio of two strings. *)
  let similarity s t =
    let n = max (String.length s) (String.length t) in
    let k = edit_distance ~limit:(n / 2) s t in
    float (n - k) /. float n
end

(** Read a whole file. *)
let read_all fname =
  let ic = open_in fname in
  let len = in_channel_length ic in
  let ans = really_input_string ic len in
  close_in ic;
  ans

let () = assert (String.levenstein "kitten" "sitting" = 3)
let () = assert (String.levenstein "ca" "abc" = 3)

type verbosity = Quiet | Normal | Verbose

let domains = ref (max 1 (Domain.recommended_domain_count () - 1))
let lines = ref false
let verbosity = ref Normal
let threshold = ref 0.6
let extensions = ref []
let directories = ref []
let recursive = ref true
let max_file_size = ref (32 * 1024)
let exclude = ref [] (* regexp for filenames to exclude *)
let log_file = ref ""
let filename = ref false

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
         "--filename", Arg.Set filename, " Only compare file with the same name.";
         "--lines", Arg.Set lines, " Compare lines instead of characters (faster but less precise).";
         "--log", Arg.Set_string log_file, " Save output in given log file.";
         "--non-recursive", Arg.Unit (fun () -> recursive := false), " Do not recurse into folders.";
         "--parallelism", Arg.Set_int domains, " Number of threads to be run concurrently.";
         "--quiet", Arg.Unit (fun () -> verbosity := Quiet), " Do not display warnings.";
         "--size", Arg.Set_int max_file_size, Printf.sprintf " Maximum file size in octets (default: %d)." !max_file_size;
         "--threshold", Arg.Float (fun x -> threshold := x /. 100.), (Printf.sprintf " Threshold above which matching files are displayed (between 0 and 100%%, default is %.00f%%)." (!threshold *. 100.));
         "--verbose", Arg.Unit (fun () -> verbosity := Verbose), " Display more messages";
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
  if !verbosity >= Normal then
    (
      Printf.printf "Using %d domains\n%!" num_domains;
      Printf.printf "Considering %d files\n%!" (List.length files);
      List.iter (Printf.printf "Considering %s\n%!") files
    );
  let files2 = List.pairs files |> Array.of_list in
  Printf.printf "Considering %d pairs of files\n%!" (Array.length files2);
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
  let t0 = Unix.time () in
  let eta = ref 0 in
  let check i =
    let fs,ft = files2.(i) in
    let k = Atomic.fetch_and_add k 1 in
    if (not !filename) || (Filename.basename fs = Filename.basename ft) then
      let t =
        if k = 0 then "???" else
          let t = int_of_float (Unix.time () -. t0) in
          let t = t * (kmax - k) / k in
          let t = (9 * !eta + t) / 10 in
          eta := t;
          if t >= 3600 then
            let t = t / 60 in
            Printf.sprintf "%dh%02d" (t / 60) (t mod 60)
          else if t >= 60 then
            Printf.sprintf "%dm%02d" (t / 60) (t mod 60)
          else
            Printf.sprintf "%ds" t
      in
      if !verbosity >= Normal then Printf.printf "\r%.02f%% (%d / %d, ETA: %s)%!" (float (k * 100) /. float kmax) k kmax t;
      if !verbosity >= Verbose then Printf.printf ": %s vs %s%!" fs ft;
      let s = read_all fs in
      let t = read_all ft in
      let d =
        if !threshold >= 1. then (if s = t then 1. else 0.)
        else if !lines then List.similarity (String.split_on_char '\n' s) (String.split_on_char '\n' t)
        else String.similarity s t
      in
      if d >= !threshold then log "\nFound %s / %s: %.02f%%\n" fs ft (100. *. d)
                                  (* log "\n%.02f%% similarity:\n- %s\n- %s\n" (100. *. d) fs ft *)

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
