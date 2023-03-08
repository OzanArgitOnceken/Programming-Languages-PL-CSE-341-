
%{
      #include <stdio.h>
      void printForLex(const char *);
      void err_func(const char *);
      
      int token_of_quote_flag = 0;
%}

OP_OP			"("
OP_CP			")"
OP_PLUS		"+"
OP_MINUS		"-"
OP_DIV		"/"
OP_MULT		"*"
OP_DBLMULT		"**"
OP_OC_CC		"\""
OP_COMMA		","



KW_NOT		"not"
KW_EQUAL		"equal"
KW_LESS		"less"
KW_AND		"and"
KW_OR			"or"
KW_NIL		"nil"
KW_LIST		"list"
KW_CONCAT		"concat"
KW_APPEND		"append"
KW_SET		"set"
KW_DEFFUN		"deffun"
KW_FOR		"for"
KW_IF			"if"
KW_EXIT		"exit"
KW_LOAD		"load"
KW_DISP		"disp"
KW_TRUE		"true"
KW_FALSE		"false"

COMMENT		";;".*


VALUEI      "0"|[1-9][0-9]*
VALUEF      [1-9]+[a-zA-Z][0-9]+

SPACE			[[:space:]]
IDENTIFIER		[a-zA-Z][a-zA-Z0-9]*

SYN_ERR_1		[0][[:digit:]]+
SYN_ERR_3		[[:alnum:]]*[[:alnum:][:punct:]]{-}[()"]+
SYN_ERR_2		[[:digit:]]+[[:alpha:]]+

%%

{KW_AND}                {printForLex("KW_AND\n");}
{KW_OR}          	      {printForLex("KW_OR\n");}
{KW_NOT}                {printForLex("KW_NOT\n");}
{KW_EQUAL}              {printForLex("KW_EQUAL\n");}
{KW_LESS}               {printForLex("KW_LESS\n");}
{KW_NIL}                {printForLex("KW_NIL");}
{KW_LIST}               {printForLex("KW_LIST");}
{KW_APPEND}             {printForLex("KW_APPEND");}
{KW_CONCAT}             {printForLex("KW_CONCAT");}
{KW_SET}                {printForLex("KW_SET");}
{KW_DEFFUN}             {printForLex("KW_DEFFUN");}
{KW_FOR}                {printForLex("KW_FOR");}
{KW_IF}                 {printForLex("KW_IF");}
{KW_EXIT}               {printForLex("KW_EXIT");}
{KW_DISP}               {printForLex("KW_DISP");}
{KW_TRUE}               {printForLex("KW_TRUE");}
{KW_FALSE}              {printForLex("KW_FALSE");}
{KW_LOAD}               {printForLex("KW_LOAD");}
{OP_PLUS}               {printForLex("OP_PLUS");}
{OP_MULT}               {printForLex("OP_MULT");}
{OP_CP}                 {printForLex("OP_CP");}
{OP_OP}                 {printForLex("OP_OP");}
{OP_MINUS}              {printForLex("OP_MINUS");}
{OP_DBLMULT}            {printForLex("OP_DBLMULT");}
{OP_OC_CC}              {printForLex(token_of_quote_flag != 0 ? "OP_CC" : "OP_OC"); token_of_quote_flag = !token_of_quote_flag;}
{OP_DIV}                {printForLex("OP_DIV");}
{OP_COMMA}              {printForLex("OP_COMMA");}
{COMMENT}               {printForLex("COMMENT");}
{IDENTIFIER}            {printForLex("IDENTIFIER");}
{VALUEI}                { printForLex("VALUEI"); }
{VALUEF}                { printForLex("VALUEF"); }
{SPACE}                 /*ignored*/
                       	
.		 			  {err_func("SYNTAX ERROR\n"); return 0;}	
"(exit)" {printf( " PROGRAM IS TERMINATING ...\n"); return 0; } //for terminating program.   
%%

int yywrap(){}
void printForLex(const char *str)
{
      printf("%s : %s\n",yytext,str);
}
void err_func(const char *str)
{
      printf("SYNTAX ERROR:\n");
}
int main(int argc, char *argv[]){
    if (argc > 1) yyin = fopen(argv[1], "r");
	else yyin = stdin;
	yylex();  
}
