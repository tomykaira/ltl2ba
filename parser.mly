%{
  open Syntax
%}

%token <string> PROP

%token TOP
%token BOTTOM
%token NOT
%token AND
%token OR
%token NEXT
%token FINALLY
%token GLOBALLY
%token UNTIL
%token RELEASE
%token LPAREN
%token RPAREN

%token EOL

%left UNTIL RELEASE
%nonassoc NEXT FINALLY GLOBALLY
%left AND OR
%nonassoc NOT

%start main
%type <Syntax.ltl> main

%%

main:
| exp EOL
    { $1 }
;

exp:
| TOP
    { Top }
| BOTTOM
    { Bottom }
| PROP
    { Prop $1 }
| NOT exp
    { Not $2 }
| exp AND exp
    { And ($1, $3) }
| exp OR exp
    { Or ($1, $3) }

| NEXT exp
    { Next $2 }
| FINALLY exp
    { Finally $2 }
| GLOBALLY exp
    { Globally $2 }
| exp UNTIL exp
    { Until ($1, $3) }
| exp RELEASE exp
    { Release ($1, $3) }

| LPAREN exp RPAREN
    { $2 }
