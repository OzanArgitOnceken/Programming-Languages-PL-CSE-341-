
%{
      #include <stdio.h>
	#include "y.tab.h"
	#include <math.h>
	int cOpen = 0;
      
      int token_of_quote_flag = 0;
%}
%%
"and "						return OP_AND;
"or "						return OP_OR;
"not "						return OP_NOT;
"eq " 						return OP_EQ;
"gt "						return OP_GT;
"nil "						return KW_NIL;
"set "						return OP_SET;
"deffun"					return DEFF;
"defvar"					return DEFV;
"while "					return KW_WHILE;
"if "						return KW_IF;
"exit"						return KW_EXIT;
"load"						return KW_LOAD;
"disp"						return KW_DISP;
"true"						return KW_TRUE;
"false"						return KW_FALSE;

"+ " 						return OP_PLUS;
"- " 						return OP_MINUS;
"/ " 						return OP_DIV;
"** " 						return OP_DBLMULT;
"* " 						return OP_MULT;
[(] 						return OP;
[)] 						return CP; 
[,] 						return OP_COMMA; 
[']						return OP_AP;
["] 								{	if(cOpen == -1)
										{++cOpen;	return OP_CC;}
									else
										{return OP_OC;		cOpen = -1;}
								}

[a-zA-Z_][a-zA-Z0-9_]*						{yylval.id=strdup(yytext);	return IDENTIFIER;}	
[0]|[1-9][0-9]*f[0]|[1-9][0-9]*					{yylval.number=atoi(yytext);	return VALUE;}
[;][;][ a-zA-Z0-9\/.+-]+					return COMMENT;

[ \t\n]+ 							{;}

.		{yyerror("invalid token\n");}
%%
