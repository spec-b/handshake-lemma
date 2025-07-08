(* gradual_verification.ml *)

type graph_property =
  | Unknown
  | Acyclic
  | Connected
  | Tree
  | HasCycle
  | Cycle of int list

type proof_step =
  | Proved of graph_property * string
  | Assumed of graph_property

type vertex = int
type edge = vertex * vertex

type graph = {
  vertices: vertex list;
  edges: edge list;
}

type verified_graph = {
  raw: graph;
  mutable props: graph_property list;
  mutable trace: proof_step list;
}

external simulate_heavy_check : int -> bool = "simulate_heavy_check"

let log msg =
  let oc = open_out_gen [Open_append; Open_creat] 0o666 "log.txt" in
  output_string oc (msg ^ "\n");
  close_out oc

let check_connected graph =
  let visited = Hashtbl.create (List.length graph.vertices) in
  let rec bfs queue =
    match queue with
    | [] -> ()
    | v :: vs ->
      if not (Hashtbl.mem visited v) then begin
        Hashtbl.add visited v true;
        let neighbors =
          List.fold_left (fun acc (a, b) ->
            if a = v then b :: acc
            else if b = v then a :: acc
            else acc
          ) [] graph.edges
        in
        bfs (vs @ neighbors)
      end else bfs vs
  in
  match graph.vertices with
  | [] -> true
  | v0 :: _ -> bfs [v0];
    List.for_all (fun v -> Hashtbl.mem visited v) graph.vertices

let require_connected g =
  if List.mem Connected g.props then ()
  else if check_connected g.raw then begin
    g.props <- Connected :: g.props;
    g.trace <- Proved (Connected, "BFS check") :: g.trace
  end else
    failwith "Graph not connected"

let check_acyclic graph =
  let visited = Hashtbl.create (List.length graph.vertices) in
  let rec dfs v parent =
    Hashtbl.add visited v true;
    List.iter (fun (a, b) ->
      let neighbor = if a = v then b else if b = v then a else -1 in
      if neighbor <> -1 && neighbor <> parent then
        if Hashtbl.mem visited neighbor then raise Exit
        else dfs neighbor v
    ) graph.edges
  in
  try
    List.iter (fun v -> if not (Hashtbl.mem visited v) then dfs v (-1)) graph.vertices;
    true
  with Exit -> false

let require_acyclic g =
  if List.mem Acyclic g.props then ()
  else if check_acyclic g.raw then begin
    g.props <- Acyclic :: g.props;
    g.trace <- Proved (Acyclic, "DFS check") :: g.trace
  end else
    failwith "Graph has a cycle"

let prove_handshake_lemma g =
  let edge_count = List.length g.raw.edges in
  let degrees = Hashtbl.create (List.length g.raw.vertices) in
  List.iter (fun (u, v) ->
    Hashtbl.replace degrees u (1 + (try Hashtbl.find degrees u with Not_found -> 0));
    Hashtbl.replace degrees v (1 + (try Hashtbl.find degrees v with Not_found -> 0));
  ) g.raw.edges;
  let degree_sum = Hashtbl.fold (fun _ d acc -> acc + d) degrees 0 in
  let valid = (degree_sum = 2 * edge_count) in
  let msg = Printf.sprintf "Handshake Lemma: degree_sum = %d vs 2*edges = %d" degree_sum (2 * edge_count) in
  log msg;
  msg

let parallel_verify g =
  Printf.printf "Starting parallel verification...\n"; flush stdout;
  let open Domainslib in
  let pool = Task.setup_pool ~num_domains:2 () in
  Task.run pool (fun () ->
    let c = Task.async pool (fun () -> require_connected g) in
    let a = Task.async pool (fun () -> require_acyclic g) in
    Task.await pool c;
    Task.await pool a;
    ignore (simulate_heavy_check (List.length g.raw.edges));
    ignore (prove_handshake_lemma g)
  );
  Task.teardown_pool pool 



let g = {
  raw = {
    vertices = List.init 100 (fun i -> i);
    edges = List.init 99 (fun i -> (i, i+1));
  };
  props = [];
  trace = [];
}

let () = print_endline (parallel_verify g)
