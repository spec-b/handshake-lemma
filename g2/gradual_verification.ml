(* gradual_verification.ml *)

(* Graph property annotations for gradual verification *)
type graph_property =
  | Unknown
  | Acyclic
  | Connected
  | Tree
  | HasCycle
  | Cycle of int list

(* Verified graph structure *)
type vertex = int
type edge = vertex * vertex

type graph = {
  vertices: vertex list;
  edges: edge list;
}

type verified_graph = {
  raw: graph;
  mutable props: graph_property list;
}

(* Utility: check if graph is connected - basic BFS traversal *)
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

(* Require that a graph has a certain property *)
let require_connected (g: verified_graph) : unit =
  if List.mem Connected g.props then ()
  else if check_connected g.raw then
    g.props <- Connected :: g.props
  else
    failwith "Graph is not connected"

(* Require acyclicity - naive DFS *)
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

let require_acyclic (g: verified_graph) : unit =
  if List.mem Acyclic g.props then ()
  else if check_acyclic g.raw then
    g.props <- Acyclic :: g.props
  else
    failwith "Graph has a cycle"

(* Require that a graph is a tree: connected + acyclic *)
let require_tree (g: verified_graph) : unit =
  require_connected g;
  require_acyclic g;
  g.props <- Tree :: g.props

(* Gradually verified handshake lemma proof *)
let prove_handshake_lemma (g: verified_graph) =
  let open Printf in
  let edge_count = List.length g.raw.edges in
  let degrees = Hashtbl.create (List.length g.raw.vertices) in
  List.iter (fun (u, v) ->
    Hashtbl.replace degrees u (1 + (try Hashtbl.find degrees u with Not_found -> 0));
    Hashtbl.replace degrees v (1 + (try Hashtbl.find degrees v with Not_found -> 0));
  ) g.raw.edges;
  let degree_sum =
    Hashtbl.fold (fun _ deg acc -> acc + deg) degrees 0
  in
  if degree_sum = 2 * edge_count then
    printf "Handshake Lemma holds: sum of degrees = %d = 2 * %d\n" degree_sum edge_count
  else
    printf "Handshake Lemma fails: sum of degrees = %d, 2 * edges = %d\n" degree_sum (2 * edge_count)

(* Example usage *)
let example_graph = {
  vertices = [1; 2; 3];
  edges = [(1, 2); (2, 3)];
}

let verified = {
  raw = example_graph;
  props = [];
}

let () =
  require_connected verified;
  require_acyclic verified;
  prove_handshake_lemma verified


let () =
  let g = {
    raw = {
      vertices = [1; 2; 3];
      edges = [(1, 2); (2, 3)]
    };
    props = []
  } in

  require_connected g;
  require_acyclic g;
  require_tree g;
  prove_handshake_lemma g