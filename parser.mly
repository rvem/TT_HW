%token <string> VAR
%token LAMBDA
%token DOT
%token WS
%token OPAREN CPAREN
%token EOF

%nonassoc DOT
%left WS

%start parse
%type <Lambda.lambda> parse
%%

parse:
	| expr EOF		{ $1 }
;

expr:
	| VAR			{ Lambda.Var $1 }
	| LAMBDA VAR DOT expr  	{ Lambda.Abs ($2, $4) }
	| expr WS expr	  	{ Lambda.App ($1, $3) }
	| OPAREN expr CPAREN 	{ $2 }
;
