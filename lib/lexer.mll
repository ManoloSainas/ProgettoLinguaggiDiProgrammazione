{
open Parser

exception Error of string
}


(* Pattern definition *)
let white = [' ' '\n' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let id = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let lineComment = "//" [^ '\n']*
let comments = "/*" ([^ '*'] | '*' [^ '/'])* "*/"

rule read_token =
  parse
  (* Ignore *)
  | white { read_token lexbuf }  
  | lineComment { read_token lexbuf }
  | comments { read_token lexbuf }


  | "main" { MAIN }

  (* Terminal symbols *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "=" { ASSIGN } 
  | ";"  { SEQ }  

  | "," { SEP } 

  (* Binary operator *)
  | ">" { GT }
  | "<" { LT }
  | ">=" { GEQ }
  | "<=" { LEQ }

  | "+" { ADD }
  | "-" { SUB } 
  | "*" { MUL }  
  | "/" { DIV }
  | "%" { MOD }

  | "==" { EQ } 
  | "!=" { NEQ } 
  | "&&" { LAND } 
  | "||" { LOR } 

  (* Keyword *)
  | "int" { INT }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "return" { RETURN }

  (* Identificator *)
  | id { ID (Lexing.lexeme lexbuf) }

  (* Numbers *)
  | num { CONST (Lexing.lexeme lexbuf) } 

  (* End of File*)
  | eof { EOF }

  (* Lexer.Error *)
  | _ { raise (Error "errore di prova") } 