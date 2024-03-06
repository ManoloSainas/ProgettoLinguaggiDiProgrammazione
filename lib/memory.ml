open Ast

exception IntrinsicOverride
exception UndeclaredVariable of string

type loc = int

type ide = string

type memval = int

type envval =
  | Loc of loc
  | Fun of (Ast.parameters * Ast.instruction)
  | Intrinsic of Ast.intrinsic

type memory = loc -> memval

type environment = ide -> envval

type stackval = environment

type env_stack = stackval list * loc

(* utility functions *)
let get_stackval_list (env : env_stack) : stackval list =
  match env with
  | st, _ -> st

let get_free_loc (env : env_stack) : loc =
  match env with
  | _, loc -> loc

let is_loc = function
  | Loc _ -> true
  | _ -> false
(* end of utility function *)

let init_memory () : memory = fun _ -> -1

(* initializes a stack with an environment defining the intrinsic functions *)
let init_stack () : env_stack =
  let initial_environment : environment = function
    | "scan" -> Intrinsic SCAN
    | "cannon" -> Intrinsic CANNON
    | "drive" -> Intrinsic DRIVE
    | "damage" -> Intrinsic DAMAGE
    | "speed" -> Intrinsic SPEED
    | "loc_x" -> Intrinsic LOC_X
    | "loc_y" -> Intrinsic LOC_Y
    | "rand" -> Intrinsic RAND
    | "sqrt" -> Intrinsic SQRT
    | "sin" -> Intrinsic SIN
    | "cos" -> Intrinsic COS
    | "tan" -> Intrinsic TAN
    | "atan" -> Intrinsic ATAN
    | _ -> raise (UndeclaredVariable "")
  in
  ([ initial_environment ], 0)

let find_mem (mem : memory) (loc : loc) : memval = mem loc

let add_mem (mem : memory) (loc : loc) (n : memval) : memory =
 fun x -> if x = loc then n else mem x

let update_mem (mem : memory) (loc : loc) (n : memval) : memory =
 fun x -> if x = loc then n else mem x

let find_env (env : env_stack) (id : ide) : envval =
  try (List.hd (get_stackval_list env)) id
  with UndeclaredVariable "" -> raise (UndeclaredVariable id)

let add_env (env : env_stack) (id : ide) (n : envval) : env_stack =
  let intrinsic_list =
    [
      "scan";
      "cannon";
      "drive";
      "damage";
      "speed";
      "loc_x";
      "loc_y";
      "rand";
      "sqrt";
      "sin";
      "cos";
      "tan";
      "atan";
    ]
  in
  if List.mem id intrinsic_list then raise IntrinsicOverride
  else
    match get_stackval_list env with
    | [] -> failwith "Empty environment stack"
    | current_env :: rest_of_env ->
        let new_environment : environment =
         fun x -> if x = id then n else current_env x
        in
        ( new_environment :: rest_of_env,
          if is_loc n then get_free_loc env + 1 else get_free_loc env )

let add_frame (env_st : env_stack) : env_stack =
  match get_stackval_list env_st with
  | [] -> failwith "Empty environment stack"
  | top :: rest_of_env -> (top :: top :: rest_of_env, get_free_loc env_st)

let pop_frame (env_st : env_stack) : env_stack =
  match get_stackval_list env_st with
  | [] -> failwith "Empty environment stack"
  | _ :: tl -> (tl, get_free_loc env_st)
