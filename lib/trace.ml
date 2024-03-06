open Ast
open Memory
open Robot

exception NoRuleApplies
exception WrongArguments of int * int

type state = Memory.env_stack * Memory.memory

type conf = St | Ret of int | Instr of Ast.instruction

let apply_intrinsic (vals : int list) (i : intrinsic) : int option =
  match i with
  | DAMAGE | SPEED | LOC_X | LOC_Y -> (
      match vals with
      | [] -> (
          match i with
          | DAMAGE -> Some (damage ())
          | SPEED -> Some (speed ())
          | LOC_X -> Some (loc_x ())
          | LOC_Y -> Some (loc_y ())
          | _ -> failwith "Unreachable")
      | _ -> raise (WrongArguments (List.length vals, 0)))
  | RAND | SQRT | SIN | COS | TAN | ATAN -> (
      match vals with
      | hd :: [] -> (
          match i with
          | RAND -> Some (rand hd)
          | SQRT -> Some (sqrt hd)
          | SIN -> Some (sin hd)
          | COS -> Some (cos hd)
          | TAN -> Some (tan hd)
          | ATAN -> Some (atan hd)
          | _ -> failwith "Unreachable")
      | _ -> raise (WrongArguments (List.length vals, 1)))
  | SCAN | CANNON | DRIVE -> (
      match vals with
      | [ st; nd ] -> (
          match i with
          | SCAN -> Some (scan st nd)
          | CANNON -> Some (cannon st nd)
          | DRIVE ->
              drive st nd;
              None
          | _ -> failwith "Unreachable")
      | _ -> raise (WrongArguments (List.length vals, 2)))

(* utility function *)
let get_env_stack (state : state) : env_stack =
  match state with
  | env, _ -> env

let get_memory (state : state) : memory =
  match state with
  | _, mem -> mem

let loc_of_envval (en : envval) : loc =
  match en with
  | Loc l -> l
  | _ -> failwith "error"

let fun_of_envval_instr (en : envval) : Ast.instruction =
  match en with
  | Fun (_, ins) -> ins
  | _ -> failwith "error"

let fun_of_envval_params (en : envval) : Ast.parameters =
  match en with
  | Fun (params, _) -> params
  | _ -> failwith "error"

let rec add_ret_if_missing instr =
  match instr with
  | RET _ -> instr
  | SEQ (i1, i2) -> SEQ (i1, add_ret_if_missing i2)
  | _ -> SEQ (instr, RET None)

let intrinsic_of_ide (id : ide) : intrinsic =
  match id with
  | "scan" -> SCAN
  | "cannon" -> CANNON
  | "drive" -> DRIVE
  | "damage" -> DAMAGE
  | "speed" -> SPEED
  | "loc_x" -> LOC_X
  | "loc_y" -> LOC_Y
  | "rand" -> RAND
  | "sqrt" -> SQRT
  | "sin" -> SIN
  | "cos" -> COS
  | "tan" -> TAN
  | "atan" -> ATAN
  | _ -> failwith "error"

let expression_list_of_trace_expr ((_, expr_list) : state * expression list) :
    expression list =
  match expr_list with
  | [] -> []
  | el -> el

let rec trace1_expr (state : state) (expr : expression) : state * expression =
  match expr with
  | NIL -> (state, NIL)
  | IDE x ->
      ( state,
        CONST
          (find_mem (get_memory state)
             (loc_of_envval (find_env (get_env_stack state) x))) )
  | ASSIGN (x, e) -> (
      match e with
      | CONST n -> (
          try
            let env_stack = get_env_stack state in
            let get_envval = find_env env_stack x in
            let get_memval = loc_of_envval get_envval in
            let upd_mem = update_mem (get_memory state) get_memval n in
            ((env_stack, upd_mem), CONST n)
          with UndeclaredVariable x -> raise (UndeclaredVariable x))
      | e ->
          let state', e' = trace1_expr state e in
          (state', ASSIGN (x, e')))
  | CALL (f, args) ->
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
      if List.mem f intrinsic_list (* call of intrinsic function *) then
        let intr = intrinsic_of_ide f in
        let rec int_list_of_args (state : state) (args : expression list) :
            state * int list =
          match args with
          | [] -> (state, [])
          | CONST n :: n' ->
              let state', ints = int_list_of_args state n' in
              (state', n :: ints)
          | e :: n' ->
              let state', e' = trace1_expr state e in
              int_list_of_args state' (e' :: n')
        in
        let state', args_list = int_list_of_args state args in
        match apply_intrinsic args_list intr with
        | Some n -> (state', CONST n)
        | None -> (state', NIL)
      else
        (* call of other functions *)
        let params = fun_of_envval_params (find_env (get_env_stack state) f) in
        let fun_instr =
          fun_of_envval_instr (find_env (get_env_stack state) f)
        in
        (* function body *)
        let dup_state = (add_frame (get_env_stack state), get_memory state) in
        let rec process_args (args : expression list) (params : parameters)
            (state : state) (upd_state : state) =
          (* args list process *)
          match (args, params) with
          | args, params when List.length args != List.length params ->
              raise (WrongArguments (List.length args, List.length params))
          | CONST n :: n', ide :: ide' ->
              let free_loc = get_free_loc (get_env_stack upd_state) in
              let new_mem = add_mem (get_memory upd_state) free_loc n in
              let new_envstack =
                add_env (get_env_stack upd_state) ide (Loc free_loc)
              in
              let new_state = (new_envstack, new_mem) in
              (* update_state with ide->n *)
              process_args n' ide' state new_state
          | [], [] -> upd_state (* actual output *)
          | e1 :: tl, ide_list ->
              let state', e1' = trace1_expr state e1 in
              process_args (e1' :: tl) ide_list state' upd_state
          | _ -> raise (WrongArguments (List.length args, List.length params))
        in
        let final_state = process_args args params state dup_state in
        (final_state, CALL_EXEC fun_instr)
  | CALL_EXEC instr -> (
      let state', conf = trace1_instr state (Instr instr) in
      match conf with
      | St -> (state', NIL)
      | Ret e -> (state', CONST e)
      | Instr i -> (state', CALL_EXEC i))
  | UNARY_EXPR (UMINUS, e) -> (
      match e with
      | CONST n -> (state, CONST (-n))
      | e ->
          let state', e' = trace1_expr state e in
          (state', UNARY_EXPR (UMINUS, e')))
  | BINARY_EXPR (e1, op, e2) -> (
      match op with
      | ADD -> (
          match (e1, e2) with
          | CONST n1, CONST n2 -> (state, CONST (n1 + n2))
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, ADD, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', ADD, e2)))
      | SUB -> (
          match (e1, e2) with
          | CONST n1, CONST n2 -> (state, CONST (n1 - n2))
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, SUB, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', SUB, e2)))
      | MUL -> (
          match (e1, e2) with
          | CONST n1, CONST n2 -> (state, CONST (n1 * n2))
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, MUL, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', MUL, e2)))
      | DIV -> (
          match (e1, e2) with
          | CONST n1, CONST n2 -> (state, CONST (n1 / n2))
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, DIV, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', DIV, e2)))
      | MOD -> (
          match (e1, e2) with
          | CONST n1, CONST n2 -> (state, CONST (n1 mod n2))
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, MOD, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', MOD, e2)))
      | EQ -> (
          match (e1, e2) with
          | CONST n1, CONST n2 ->
              if n1 = n2 then (state, CONST 1) else (state, CONST 0)
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, EQ, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', EQ, e2)))
      | NEQ -> (
          match (e1, e2) with
          | CONST n1, CONST n2 ->
              if n1 <> n2 then (state, CONST 1) else (state, CONST 0)
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, NEQ, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', NEQ, e2)))
      | GT -> (
          match (e1, e2) with
          | CONST n1, CONST n2 ->
              if n1 > n2 then (state, CONST 1) else (state, CONST 0)
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, GT, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', GT, e2)))
      | LT -> (
          match (e1, e2) with
          | CONST n1, CONST n2 ->
              if n1 < n2 then (state, CONST 1) else (state, CONST 0)
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, LT, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', LT, e2)))
      | GEQ -> (
          match (e1, e2) with
          | CONST n1, CONST n2 ->
              if n1 >= n2 then (state, CONST 1) else (state, CONST 0)
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, GEQ, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', GEQ, e2)))
      | LEQ -> (
          match (e1, e2) with
          | CONST n1, CONST n2 ->
              if n1 <= n2 then (state, CONST 1) else (state, CONST 0)
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, LEQ, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', LEQ, e2)))
      | LAND -> (
          match (e1, e2) with
          | CONST n1, CONST n2 ->
              if n1 <> 0 && n2 <> 0 then (state, CONST 1) else (state, CONST 0)
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, LAND, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', LAND, e2)))
      | LOR -> (
          match (e1, e2) with
          | CONST n1, CONST n2 ->
              if n1 <> 0 || n2 <> 0 then (state, CONST 1) else (state, CONST 0)
          | CONST _, e2 ->
              let state', e2' = trace1_expr state e2 in
              (state', BINARY_EXPR (e1, LOR, e2'))
          | e1, _ ->
              let state', e1' = trace1_expr state e1 in
              (state', BINARY_EXPR (e1', LOR, e2))))
  | CONST _ -> raise NoRuleApplies

and trace1_instr (state : state) (conf : conf) : state * conf =
  match conf with
  | St -> raise NoRuleApplies
  | Ret _ -> (state, St)
  | Instr i -> (
      match i with
      | EMPTY -> (state, St)
      | IF (e, s1) -> (
          match e with
          | CONST n -> if n > 0 then (state, Instr s1) else (state, St)
          | e ->
              let state', e' = trace1_expr state e in
              (state', Instr (IF (e', s1))))
      | IFE (e, s1, s2) -> (
          match e with
          | CONST n -> if n > 0 then (state, Instr s1) else (state, Instr s2)
          | e ->
              let state', e' = trace1_expr state e in
              (state', Instr (IFE (e', s1, s2))))
      | WHILE (e, i) -> (state, Instr (WHILE_EXEC (e, i, e)))
      | WHILE_EXEC (e1, i, e2) -> (
          match e1 with
          | CONST 0 -> (state, St)
          | CONST _ -> (state, Instr (SEQ (i, WHILE (e2, i))))
          | e ->
              let state', e' = trace1_expr state e in
              (state', Instr (WHILE_EXEC (e', i, e2))))
      | EXPR e -> (
          match e with
          | CONST _ -> (state, St)
          | NIL -> (state, St)
          | e ->
              let state', e' = trace1_expr state e in
              (state', Instr (EXPR e')))
      | RET opt -> (
          match opt with
          | Some e -> (
              match e with
              | CONST n ->
                  let pop_env_stack = pop_frame (get_env_stack state) in
                  let mem = get_memory state in
                  let state' = (pop_env_stack, mem) in
                  (state', Ret n)
              | e ->
                  let state', e' = trace1_expr state e in
                  (state', Instr (RET (Some e'))))
          | None -> (state, Ret 0))
      | BLOCK i ->
          let dup_state = (add_frame (get_env_stack state), get_memory state) in
          (dup_state, Instr (BLOCK_EXEC i))
      | BLOCK_EXEC i -> (
          match trace1_instr state (Instr i) with
          | state', St ->
              let pop_env_stack = pop_frame (get_env_stack state') in
              let mem = get_memory state' in
              let state'' = (pop_env_stack, mem) in
              (state'', St)
          | state', Ret n ->
              let pop_env_stack = pop_frame (get_env_stack state') in
              let mem = get_memory state' in
              let state'' = (pop_env_stack, mem) in
              (state'', Ret n)
          | state', Instr i' -> (state', Instr (BLOCK_EXEC i')))
      | VARDECL id ->
          let free_loc = get_free_loc (get_env_stack state) in
          let new_envstack = add_env (get_env_stack state) id (Loc free_loc) in
          let new_mem = add_mem (get_memory state) free_loc 0 in
          (* placeholder memval *)
          ((new_envstack, new_mem), St)
      | VARDECL_INIT (id, e) -> (
          match e with
          | CONST n ->
              let free_loc = get_free_loc (get_env_stack state) in
              let new_envstack =
                add_env (get_env_stack state) id (Loc free_loc)
              in
              let new_mem = add_mem (get_memory state) free_loc n in
              ((new_envstack, new_mem), St)
          | e ->
              let state', e' = trace1_expr state e in
              (state', Instr (VARDECL_INIT (id, e'))))
      | FUNDECL (id, args, instr) ->
          let instr = add_ret_if_missing instr in
          let new_envstack =
            add_env (get_env_stack state) id (Fun (args, instr))
          in
          ((new_envstack, get_memory state), St)
      | SEQ (i1, i2) -> (
          match trace1_instr state (Instr i1) with
          | state', St -> (state', Instr i2)
          | state', Ret n -> (state', Ret n) (* if return then skip Instr i2 *)
          | state', Instr i1' -> (state', Instr (SEQ (i1', i2)))))

let trace_expr (state : state) (expr : expression) : state * expression list =
  let rec helper (state : state) (expr : expression) (acc : expression list) :
      state * expression list =
    try
      let state', expr' = trace1_expr state expr in
      helper state' expr' (expr' :: acc)
    with NoRuleApplies -> (state, List.rev acc)
  in
  helper state expr []

let trace_instr (state : state) (conf : conf) : state * conf list =
  let rec helper (state : state) (conf : conf) (acc : conf list) :
      state * conf list =
    try
      let state', conf' = trace1_instr state conf in
      helper state' conf' (conf' :: acc)
    with NoRuleApplies -> (state, List.rev acc)
  in
  helper state conf []

let trace (prog : program) : expression list =
  let conf0 = Instr prog in
  let state0 : state = (init_stack (), init_memory ()) in
  let state_instr = trace_instr state0 conf0 in
  let traced = trace_expr (fst state_instr) (CALL ("main", [])) in
  expression_list_of_trace_expr traced
