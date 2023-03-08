%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <math.h>
	#include <string.h>
	extern int yylex();
	extern  int yylineno;
	extern  char* yytext;

	struct var
	{ 	int type;
		char name[100];
		int val;
		int arr[1000];};
	struct fun
	{ 	int type;
		int argNumber;
		char name[100];}; 
	struct fun funs[1000];
	struct var vars[1000];
	int varsSize = 0;
	int funcCheck = 0;
	int tempArr[1000];
	int counter = 0;
	int idCounter;
	int funcSize = 0;
	int temp[2] = {-1,-1};
	struct var getFromvars(char* str){	
		int i=0;
		while( i < varsSize)
		{	if(strcmp(vars[i].name, str) == 0)
			return vars[i];
			++i;}
		return;}
	void addFunc(char* id, int num){	
		int i = 0;
		while( i < strlen(id))
		{	funs[funcSize].name[i] = id[i];	++i;	}
		funs[funcSize].type = 1;
		funs[funcSize].argNumber = num;
		++funcSize;
	}
	struct fun getFromfuns(char* str){
		int i = 0;
		while( i < funcSize){	if(strcmp(funs[i].name, str) == 0)
				return funs[i];
			++i;}
		return;}

	void addVarI(char* id , int v){
		int i = 0;
		while( i < varsSize)
		{
			if(strcmp(vars[i].name, id) == 0)
			{	
				vars[i].val = v;
				vars[i].type = 1;
				return;
			}
			++i;
		}
		i=0;
		while(i < strlen(id))
		{
			vars[varsSize].name[i] = id[i];
			++i;
		}
		vars[varsSize].val = v;
		vars[varsSize].type = 1;
		vars[varsSize].type = 1;varsSize++;
	}
	void addVarB(char id [], int v){
		int i = 0;
		while( i < varsSize)
		{
			if(strcmp(vars[i].name, id) == 0)
			{	
				vars[i].val = v;
				vars[i].type = 2;
				return;
			}
			++i;
		}
		i=0;
		while( i < strlen(id))
		{
			vars[varsSize].name[i] = id[i];
			++i;
		}
		vars[varsSize].type = 2;
		vars[varsSize].val = v;
		varsSize++;}
	void copyArr(int arr1[], int arr2[], int size){
		int i = 0;
		while( i < size){ 
			arr1[i] = arr2[i];	
			++i;}}
%}
%union {
	struct{
	int number;
	char* id;
	int valArr [1000];
	int output;
	};}

%token OP_AND OP_OR OP_NOT OP_EQ OP_GT KW_NIL KW_LIST KW_APPEND KW_CONCAT OP_SET DEFF DEFV KW_FOR KW_WHILE KW_IF KW_EXIT KW_LOAD KW_DISP KW_TRUE KW_FALSE 
%token OP_PLUS OP_MINUS OP_DIV OP_DBLMULT OP_MULT OP CP OP_AP OP_OC OP_CC OP_COMMA
%start START
%token <number>VALUE
%token COMMENT
%token <id>IDENTIFIER

%%
	START : | START INPUT	{printf("INPUT= ");}
	INPUT : EXPI {
		printf("NO PROBLEM ON SYNTAX\n");
		
		if($<output>1 == 2){
			if($<number>$ == 0) printf("RESULT= FALSE\n\n");
			else if($<number>$ == 1) printf("RESULT= TRUE\n\n");}
		else if($<output>1 == 8)	{printf("RESULT= "); for(int i = 0; i < temp[0]; i++)	printf("%d ",  $<valArr>$[i]); printf("\n"); temp[0] = -1;temp[1] = -1;}
		else if($<output>1 != 3)printf("RESULT= %d\n\n",  $<number>$);}
		  | EXPLISTI {printf("RESULT= "); for(int i = 0; i < temp[0]; i++)	printf("%d ",  $<valArr>$[i]); printf("\n"); temp[1] = -1;
			temp[0] = -1;}

	EXPI: OP OP_PLUS EXPI EXPI CP {$<number>$ = $<number>3 + $<number>4;}
		| OP OP_MINUS EXPI EXPI CP {$<number>$ = $<number>3 - $<number>4;}
		| OP OP_MULT EXPI EXPI CP {$<number>$ = $<number>3 * $<number>4;}
		| OP OP_DIV EXPI EXPI CP {$<number>$ = $<number>3 / $<number>4;}
		| OP OP_DBLMULT EXPI EXPI CP {
			int x = $<number>3;
			for(int i = 1; i<$<number>4; i++)
			x *= $<number>3;
			$<number>$ = x;}

	  | VALUE {$<number>$ = $<number>1;}
	  | IDENTIFIER {
	    	struct var tt = getFromvars($<id>1);
	    	if(tt.type == 0 && funcCheck == 0)	
	    		{printf("syntax error: variable does not have value\n"); exit(1);};
		    if(tt.type != 0 && funcCheck == 0 ) {
		       $<number>$ = tt.val;	}}

	  | OP OP_SET IDENTIFIER EXPI CP{
	  	if($<output>4 == 2){
	  		addVarB($<id>3, $<number>4);
	  		$<output>$ = 2;}
			else 
				addVarI($<id>3, $<number>4);
			$<number>$ = $<number>4;}

	  | OP DEFV IDENTIFIER EXPI CP{
			if($<output>4 == 2){
		  		addVarB($<id>3, $<number>4);
		  		$<output>$ = 2;}
			else 
				addVarI($<id>3, $<number>4);
			$<number>$ = $<number>4;	}

		| EXPB {$<output>$ = 2; $<number>$ == $<number>1; }

			| OP KW_IF EXPB EXPLISTI CP {		$<output>$ = 3;	}
			| OP KW_IF EXPB EXPLISTI EXPLISTI CP{		$<output>$ = 3;		}
			| OP KW_WHILE OP EXPB CP EXPLISTI CP {	$<output>$ = 3;	}
			| OP KW_FOR OP IDENTIFIER EXPI EXPI CP EXPLISTI CP {		$<output>$ = 3;	}
			| OP IDENTIFIER EXPLISTI CP {
				struct fun tf = getFromfuns($<id>2);
				if(tf.type == 0)	{printf("syntax error: function didn't defined1\n"); exit(1);};
	   			if(tf.type != 0 && tf.argNumber != temp[0])	{printf("syntax error: function is didn't defined2\n"); exit(1);};
				$<output>$ = 3;}
			| OP DEFF IDENTIFIER OP IDLIST CP EXPLISTI CP {
				addFunc($<id>3, idCounter); 
				idCounter = 0;
				$<output>$ = 3;}
			| OP KW_DISP EXPI CP 		{	$<number>$ = $<number>3; $<output>$ = $<output>3;	}
			| OP KW_DISP EXPLISTI CP 	{	copyArr($<valArr>$, $<valArr>3, temp[0]); $<output>$ = 8;	}
			| OP KW_EXIT CP		{	$<output>$ = 3;exit(0);	}	
			| COMMENT  			{	$<output>$ = 3;	}


	EXPB:
		  OP OP_AND EXPB EXPB CP 	{	$<number>$ = $<number>3 && $<number>4; $<output>$ = 2;	}
		| OP OP_OR EXPB EXPB CP 	{	$<number>$ = $<number>3 || $<number>4; $<output>$ = 2;	}
		| OP OP_NOT EXPB CP 		{	if($<number>3 == 0) $<number>$ = 1; else $<number>$ = 0; $<output>$ = 2;	}
		| OP OP_EQ EXPI EXPI CP 	{	if($<number>3 == $<number>4) $<number>$ = 1; 						else $<number>$ = 0; $<output>$ = 2;		}
		| OP OP_EQ EXPB EXPB CP 	{	if($<number>3 == $<number>4) $<number>$ = 1; 						else $<number>$ = 0; $<output>$ = 2;		}
		| OP OP_GT EXPI EXPI CP {if($<number>3 > $<number>4) $<number>$ = 1; 							else $<number>$ = 0; $<output>$ = 2;		}
		| IDENTIFIER 
		{	struct var tt = getFromvars($<id>1);
			if(tt.type == 0 && funcCheck == 0)	{printf("syntax error: Variable does not have value\n"); exit(1);};
			if(tt.type != 0 && funcCheck == 0 ) 	$<number>$ = tt.val;
		}
		| KW_TRUE		 	{	$<number>$ = 1; 	$<output>$ = 2;	}
		| KW_FALSE 			{	$<number>$ = 0;	$<output>$ = 2;	}
		| KW_NIL 			{	$<number>$ = 0;	$<output>$ = 2;	}
		
	EXPLISTI: 

	OP KW_CONCAT EXPLISTI EXPLISTI CP {
	 	int i = 0;
		while( i < temp[1])
		{ 	$<valArr>3[temp[0] + i] = $<valArr>4[i]; 
			++i; 	} 
		int u = temp[0] + temp[1]; 	copyArr($<valArr>$, $<valArr>3, u); 
		temp[1] = -1;
		temp[0] = u; }
	| OP KW_APPEND EXPI EXPLISTI CP {
				int a [100];
				struct var t = getFromvars($<id>3); 
				if(t.type == 0){
				 	int p = $<number>3;
					if(p != 0)
					{ 	a[0] = p;
						temp[1] = 1; 	} 	}
				else{
				 	a[0] = $<number>3;
					temp[1] = 1; 	}
				int i = 0;
				while( i < temp[0]){
				 	a[temp[1] + i] = $<valArr>4[i]; 
					++i; 	} 
				int u = temp[0] + temp[1]; 
				copyArr($<valArr>$, a, u);
				temp[1] = -1;
				temp[0] = u; 	}

			| LISTVALUE {	 		copyArr($<valArr>$, $<valArr>1, counter);
							if(temp[0] == -1)	temp[0] = counter;
							else temp[1] = counter;
							counter = 0;}

	LISTVALUE: OP_AP OP VALUES CP 		{		copyArr($<valArr>$, $<valArr>3, counter); 	}
			 | OP_AP OP CP 		{}
			 | OP KW_LIST VALUES CP	{ 	copyArr($<valArr>$, $<valArr>3, counter); 	}
	VALUES: VALUES VALUE 				{		$<valArr>$[counter] = $<number>2; counter+= 1;	}
		  | VALUE				{	$<valArr>$[counter] = $<number>1; counter += 1;	}
IDLIST:
	IDLIST IDENTIFIER	{	idCounter++;		}
	| IDENTIFIER		{	idCounter++;	}
%%
int yyerror(char *msg) {
  	printf("SYNTAX_ERROR: Expression not recognized\n");
  	exit(0);  	}
int yywrap(){ 	return 1;	} 

int main(void) {
 		printf("INPUT= ");
  		yyparse();
  		exit(1); 	}
