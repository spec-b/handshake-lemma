type variable = string

type term =
  | Var of variable
  | Const of string
  | App of term * term
  | Abs of variable * term

type formula =
  | Atom of string * term list
  | True
  | False
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Implies of formula * formula
  | Forall of variable * formula
  | Exists of variable * formula


(*type system*)
type typ =
  | TyVar of string
  | TyConst of string
  | TyArrow of typ * typ

type type_env = (variable * typ) list

let rec type_of_term env = function
  | Var v -> List.assoc v env
  | Const c -> TyConst c
  | App(t1, t2) ->
      (match type_of_term env t1 with
       | TyArrow(arg_ty, ret_ty) when type_of_term env t2 = arg_ty -> ret_ty
       | _ -> failwith "Type error in application")
  | Abs(v, body) ->
      let arg_ty = TyVar v in
      let body_ty = type_of_term ((v, arg_ty)::env) body in
      TyArrow(arg_ty, body_ty)

(*inference rules*)
type theorem = {
  hypotheses : formula list;
  conclusion : formula;
}

let assume f = { hypotheses = [f]; conclusion = f }

let modus_ponens th1 th2 =
  match th1.conclusion with
  | Implies(a, b) when th2.conclusion = a ->
      { hypotheses = th1.hypotheses @ th2.hypotheses; conclusion = b }
  | _ -> failwith "Modus Ponens not applicable"


(*parallel search*)
type proof_goal = {
  target: formula;
  context: formula list;
  depth_limit: int;
}

type proof_result =
  | ProofFound of theorem
  | ProofNotFound
  | ResourceExhausted

let parallel_prove_goal goals max_workers =
  let results = Array.make (List.length goals) ProofNotFound in
  let workers = ref [] in

  let worker_function idx goal =
    try
      let proof = assume goal.target in  (* mock proof *)
      results.(idx) <- ProofFound proof
    with _ -> results.(idx) <- ProofNotFound
  in

  List.iteri (fun i goal ->
    if i < max_workers then
      workers := Domain.spawn (fun () -> worker_function i goal) :: !workers
  ) goals;

  List.iter Domain.join !workers;
  Array.to_list results


(*graph theorems*)
let handshake_lemma_formula vertex_count edge_count =
  let lhs = Const (string_of_int (2 * edge_count)) in
  let rhs = Const (string_of_int vertex_count) in
  Atom("Eq", [lhs; rhs])
