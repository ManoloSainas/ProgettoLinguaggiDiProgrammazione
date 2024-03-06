%{
open Ast
%}

(* Tokens *)
%token MAIN


%token LPAREN 
%token RPAREN
%token LBRACE
%token RBRACE
%token ASSIGN 
%token SEQ  

%token SEP (* comma *)

%token GT
%token LT
%token GEQ
%token LEQ

%token ADD 
%token SUB
%token MUL
%token DIV
%token MOD

%token EQ
%token NEQ
%token LAND
%token LOR

%token INT
%token IF
%token ELSE
%token WHILE
%token DO
%token RETURN

%token <string> ID
%token <string> CONST

%token EOF

(* Operator priority *)
%left SEQ 
%nonassoc ELSE 
%nonassoc DO
%left LAND LOR
%left LT LEQ GT GEQ
%left EQ NEQ
%left ADD SUB
%left MUL DIV MOD


%start <program> main

%%

main:
  | MAIN; LPAREN; RPAREN; LBRACE; cn = connector; RBRACE; EOF; { FUNDECL("main", [], cn) }
  | gd = global_decl; MAIN; LPAREN; RPAREN; LBRACE; cn = connector; RBRACE; EOF; { SEQ(gd, FUNDECL("main", [], cn)) }
  | MAIN; LPAREN; RPAREN; LBRACE; cn = connector; RBRACE; gd = global_decl; EOF; { SEQ(gd, FUNDECL("main", [], cn)) }
  | gd1 = global_decl; MAIN; LPAREN; RPAREN; LBRACE; cn = connector; RBRACE; gd2 = global_decl; EOF; { SEQ(gd1, SEQ(gd2, FUNDECL("main", [], cn))) }
;

global_decl: 
  | INT; init = init_declarator_list; SEQ; { init } 
  | INT; init = init_declarator_list; SEQ; con = global_decl; { SEQ(init, con) }
  | fdef = function_definition; { fdef }
;

(* similar to a BLOCK without braces *)
connector:
  | dec = declaration; { dec }
  | stm = statement_helper; { stm }
;

function_definition:
  | f = ID; LPAREN; p = parameter_list; RPAREN; LBRACE; con = connector; RBRACE; { FUNDECL(f, p, BLOCK(con)) }
  | f = ID; LPAREN; p = parameter_list; RPAREN; LBRACE; con1 = connector; RBRACE; con2 = global_decl; { SEQ(FUNDECL(f, p, BLOCK(con1)), con2) }
; 
  
parameter_list:
  | { [] }
  | p = ID; { [p] }
  | p = ID; SEP; par = parameter_list { p :: par }
;

compound_statement:
  | LBRACE; RBRACE; { EMPTY }
  | LBRACE; con = connector; RBRACE; { BLOCK(con) }
;

declaration:
  | INT; init = init_declarator_list; SEQ; { init } 
  | INT; init = init_declarator_list; SEQ; con = connector; { SEQ(init, con) }
;

init_declarator_list:
  | init_dec = init_declarator; { init_dec }
  | init_dec = init_declarator; SEP; init_dec_list = init_declarator_list; { SEQ(init_dec, init_dec_list) }
;

init_declarator:
  | x = ID; { VARDECL(x) }
  | x = ID; ASSIGN; expr = expression; { VARDECL_INIT(x, expr) }
;

(* possible forms for a statement in our implementation *)
statement_helper:
  | e = expression; SEQ; { EXPR(e) }
  | e = expression; SEQ; con = connector; { SEQ(EXPR(e), con) }
  | stm = statement; { stm }
  | stm = statement; con = connector; { SEQ(stm, con) }
;

statement:
  | SEQ; { EMPTY }
  | e = expression; SEQ; { EXPR(e) }
  | cond_stat = conditional_stat; { cond_stat }
  | iter_stat = iterative_stat; { iter_stat }
  | jump_stat = jump_stat; { jump_stat }
  | comp_stat = compound_statement; { comp_stat } 
;

expression:
  | bin_expr = binary_expr; { bin_expr }
  | x = ID; ASSIGN; e = expression; { ASSIGN(x, e) }
;

binary_expr:
  | una_expr = unary_expr; { una_expr }
  | e1 = binary_expr; SUB;  e2 = binary_expr; { BINARY_EXPR((e1, SUB, e2)) }
  | e1 = binary_expr; ADD;  e2 = binary_expr; { BINARY_EXPR((e1, ADD, e2)) }
  | e1 = binary_expr; MUL;  e2 = binary_expr; { BINARY_EXPR((e1, MUL, e2)) }
  | e1 = binary_expr; DIV;  e2 = binary_expr; { BINARY_EXPR((e1, DIV, e2)) }
  | e1 = binary_expr; MOD;  e2 = binary_expr; { BINARY_EXPR((e1, MOD, e2)) }
  | e1 = binary_expr; EQ;   e2 = binary_expr; { BINARY_EXPR((e1, EQ, e2)) }
  | e1 = binary_expr; NEQ;  e2 = binary_expr; { BINARY_EXPR((e1, NEQ, e2)) }
  | e1 = binary_expr; GT;   e2 = binary_expr; { BINARY_EXPR((e1, GT, e2)) }
  | e1 = binary_expr; LT;   e2 = binary_expr; { BINARY_EXPR((e1, LT, e2)) }
  | e1 = binary_expr; GEQ;  e2 = binary_expr; { BINARY_EXPR((e1, GEQ, e2)) }
  | e1 = binary_expr; LEQ;  e2 = binary_expr; { BINARY_EXPR((e1, LEQ, e2)) }
  | e1 = binary_expr; LAND; e2 = binary_expr; { BINARY_EXPR((e1, LAND, e2)) }
  | e1 = binary_expr; LOR;  e2 = binary_expr; { BINARY_EXPR((e1, LOR, e2)) }
;

unary_expr:
  | primary_e = primary_expr; { primary_e }
  | SUB; e = unary_expr; { UNARY_EXPR(UMINUS, e) }
;

primary_expr:
  | x = ID; { IDE(x) }
  | n = CONST; { CONST(int_of_string n) }
  | f = ID; LPAREN; args = argument_list; RPAREN; { CALL(f, args) }
  | LPAREN; e = expression; RPAREN; { e }
;

argument_list:
  | { [] }
  | e = expression; { [e] }
  | e = expression; SEP; args = argument_list { e :: args }
;

%inline if_no_else:
  | IF; LPAREN; e = expression; RPAREN; stm = statement; { IF(e, stm) }
  | IF; LPAREN; e = expression; RPAREN; stm = statement; con = connector; { SEQ( IF(e, stm), con ) }
;

conditional_stat:
  | ifne = if_no_else {ifne}
  | IF; LPAREN; e = expression; RPAREN; stm1 = statement; ELSE; stm2 = statement; con = connector; { SEQ(IFE(e, stm1, stm2), con) }
  | IF; LPAREN; e = expression; RPAREN; stm1 = statement; ELSE; stm2 = statement; { IFE(e, stm1, stm2) }
;

iterative_stat:
  | WHILE; LPAREN; e = expression; RPAREN; stm = statement; { WHILE(e, stm) }
  | WHILE; LPAREN; e = expression; RPAREN; stm = statement; con = connector; { SEQ( WHILE(e, stm), con) }
  | DO; LBRACE; con  = connector; RBRACE; WHILE; LPAREN; e = expression; RPAREN; SEQ; { SEQ( BLOCK(con), WHILE(e, con)) }
  | DO; LBRACE; con1 = connector; RBRACE; WHILE; LPAREN; e = expression; RPAREN; SEQ; con2 = connector; { SEQ( SEQ( BLOCK(con1), WHILE(e, con1)), con2) }
;

jump_stat:  
  | RETURN; SEQ; { RET(None) }
  | RETURN; SEQ; connector; { RET(None) }
  | RETURN; e = expression; SEQ; { RET(Some (e)) }
  | RETURN; e = expression; SEQ; connector; { RET(Some (e)) }  
;