%{
    open Ast
%}

%token EOF
%token <int> INT 
%token <string> ID
%token TRUE
%token FALSE
%token PLUS 
%token TIMES 
%token LEQ 
%token LPAREN 
%token RPAREN 
%token IF
%token THEN
%token ELSE
%token LET 
%token EQUALS
%token IN

%nonassoc IN 
%nonassoc ELSE
%left LEQ 
%left PLUS
%left TIMES

%start <Ast.expr> prog

%%

prog:
    | e = expr; EOF { e }
    ;

expr:
    | ID { Var $1 }
    | INT { Int $1 }
    | TRUE { Bool true }
    | FALSE { Bool false }
    | expr PLUS expr { Binop(Add, $1, $3) }
    | expr TIMES expr { Binop(Mul, $1, $3) }
    | expr LEQ expr { Binop(Leq, $1, $3) }
    | IF expr THEN expr ELSE expr { If($2, $4, $6) }
    | LET ID EQUALS expr IN expr { Let($2, $4, $6) }
    | LPAREN expr RPAREN { $2 }
    ;


