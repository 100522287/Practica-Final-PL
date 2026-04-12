/*515,Iván González Portero,Vanesa Elena Ionescu*/
/*100522281@alumnos.uc3m.es,100522287@alumnos.uc3m.es*/

%{                          // SECCION 1 Declaraciones de C-Yacc

#include <stdio.h>
#include <ctype.h>            // declaraciones para tolower
#include <string.h>           // declaraciones para cadenas
#include <stdlib.h>           // declaraciones para exit ()

#define FF fflush(stdout);    // para forzar la impresion inmediata

int yylex () ;
int yyerror () ;
char *mi_malloc (int) ;
char *gen_code (char *) ;
char *int_to_string (int) ;
char *char_to_string (char) ;
extern int is_local_scope;
void add_local_var(char *name);
int is_local_var(char *name);
char* resolve_var(char *name);
extern char current_func_name[256];
void clear_local_vars();

char temp [2048] ;

// Abstract Syntax Tree (AST) Node Structure

typedef struct ASTnode t_node ;

struct ASTnode {
    char *op ;
    int type ;		// leaf, unary or binary nodes
    t_node *left ;
    t_node *right ;
} ;


// Definitions for explicit attributes

typedef struct s_attr {
    int value ;    // - Numeric value of a NUMBER 
    char *code ;   // - to pass IDENTIFIER names, and other translations 
    t_node *node ; // - for possible future use of AST
} t_attr ;

#define YYSTYPE t_attr

%}

// Definitions for explicit attributes

%token NUMBER        
%token IDENTIF       // Identificador=variable
%token INTEGER       // identifica el tipo entero
%token STRING
%token MAIN          // identifica el comienzo del proc main
%token WHILE         // identifica el bucle main
%token PUTS          // identifica la función de imprimir strings
%token PRINTF        // identifica 
%token AND OR EQ NEQ LEQ GEQ
%token IF
%token ELSE
%token FOR
%token INC
%token DEC
%token SWITCH
%token CASE
%token DEFAULT
%token BREAK


%right '='                    // asignación
%left OR                      // OR lógico (||)
%left AND                     // AND lógico (&&)
%left EQ NEQ                  // igualdad (==, !=)
%left '<' '>' LEQ GEQ         // relacionales (<, >, <=, >=)
%left '+' '-'                 // suma y resta
%left '*' '/' '%'             // multiplicación, división y módulo
%right '!' UNARY_SIGN         // operadores unarios (!, - unario, + unario)

%%                            // Seccion 3 Gramatica - Semantico

/*punto de entrada principal */
programa:       
                declaraciones_globales lista_funciones funcion_main 
                { 
                    printf ("%s\n\n%s\n\n%s\n\n(main)\n", $1.code, $2.code, $3.code) ;
                }
            ;

/* jerarquía de declaraciones globales*/
declaraciones_globales: 
                            declaracion_global declaraciones_globales   { sprintf (temp, "%s\n%s", $1.code, $2.code) ;
                                                                        $$.code = gen_code (temp) ; }
                                                    
                            |   /* vacio */                             { $$.code = gen_code ("") ; }
                        ;

declaracion_global:         
                            INTEGER lista_variables ';'     { $$ = $2 ; }
                        ;

lista_variables:            
                    variable                         { $$ = $1 ; }
                
                |    variable ',' lista_variables    { sprintf (temp, "%s\n%s", $1.code, $3.code) ;
                                                        $$.code = gen_code (temp) ; }
                ;

variable:       
                IDENTIF resto_variable   {char *resolved_name;
                                            if (is_local_scope) {
                                                add_local_var($1.code);
                                                resolved_name = resolve_var($1.code);
                                            } else {
                                                resolved_name = $1.code;
                                            }
                                            sprintf (temp, "(setq %s %s)", resolved_name, $2.code) ;
                                            $$.code = gen_code (temp) ; 
                                        }
            ;

resto_variable: 
                    /* vacio */              { $$.code = gen_code ("0") ; }
                |   
                    '=' NUMBER               { sprintf (temp, "%d", $2.value) ;
                                            $$.code = gen_code (temp) ; }
                ;

/* jerarquía de la fnción main*/
funcion_main:   
                MAIN '(' ')' '{' 
                { strcpy(current_func_name, "main"); clear_local_vars(); } 
                marcador_local declaraciones_locales lista_sentencias '}'   
                {
                    sprintf (temp, "(defun main ()\n%s%s)", $7.code, $8.code) ;
                    $$.code = gen_code (temp) ; 
                    is_local_scope = 0; 
                }         
            ;

lista_funciones:
                funcion lista_funciones  { sprintf (temp, "%s\n\n%s", $1.code, $2.code) ;
                                           $$.code = gen_code (temp) ; }
            |   /* vacio */              { $$.code = gen_code ("") ; }
            ;

nombre_funcion:
                IDENTIF {
                    strcpy(current_func_name, $1.code); // Actualiza el prefijo
                    clear_local_vars();                 // Limpia variables de otras funciones
                    $$ = $1;
                }
            ;

funcion:
                nombre_funcion '(' marcador_local lista_parametros ')' '{' declaraciones_locales lista_sentencias '}'
                {
                    sprintf (temp, "(defun %s (%s)\n%s%s)", $1.code, $4.code, $7.code, $8.code) ;
                    $$.code = gen_code (temp) ;
                    is_local_scope = 0;
                }
            ;

marcador_local:
                /* vacio */ { is_local_scope = 1; }
            ;

declaraciones_locales: 
                            INTEGER lista_variables ';' declaraciones_locales       { sprintf (temp, "\t%s\n%s", $2.code, $4.code) ;
                                                                                        $$.code = gen_code (temp) ; }
                        |   /* vacio */                                             { $$.code = gen_code ("") ; }
                        ;

lista_sentencias:       
                sentencia ';' lista_sentencias         
                                         { sprintf (temp, "\t%s\n%s", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   WHILE '(' expresion ')' '{' lista_sentencias '}' lista_sentencias
                                         { sprintf (temp, "\t(loop while %s do\n%s\t)\n%s", $3.code, $6.code, $8.code) ;
                                           $$.code = gen_code (temp) ; }
            |   FOR '(' sentencia ';' expresion ';' inc_dec ')' '{' lista_sentencias '}' lista_sentencias
                                         { sprintf (temp, "\t%s\n\t(loop while %s do\n%s\t\t%s\n\t)\n%s", $3.code, $5.code, $10.code, $7.code, $12.code) ;
                                           $$.code = gen_code (temp) ; }
            |   IF '(' expresion ')' '{' lista_sentencias '}' resto_if
                                         { sprintf (temp, "\t(if %s\n\t\t(progn %s)%s", $3.code, $6.code, $8.code) ;
                                           $$.code = gen_code (temp) ; }
            |   SWITCH '(' IDENTIF ')' '{' lista_cases '}' lista_sentencias
                                         { sprintf (temp, "\t(case %s\n%s\t)\n%s", resolve_var($3.code), $6.code, $8.code) ;
                                           $$.code = gen_code (temp) ; }
            |   /* vacio */              { $$.code = gen_code ("") ; }
            ;

lista_cases:
                case lista_cases         { sprintf (temp, "%s\n%s", $1.code, $2.code) ;
                                           $$.code = gen_code (temp) ; }
            |   default_case             { $$ = $1 ; }
            |   /* vacio */              { $$.code = gen_code ("") ; }
            ;

case:
                CASE NUMBER ':' lista_sentencias BREAK ';'
                                         { sprintf (temp, "\t\t(%d\n%s\t\t)", $2.value, $4.code) ;
                                           $$.code = gen_code (temp) ; }
            ;

default_case:
                DEFAULT ':' lista_sentencias BREAK ';'
                                         { sprintf (temp, "\t\t(otherwise\n%s\t\t)", $3.code) ;
                                           $$.code = gen_code (temp) ; }
            ;

resto_if:
                lista_sentencias                                { sprintf (temp, "\n\t)\n%s", $1.code) ;
                                                                    $$.code = gen_code (temp) ; }
            |   ELSE '{' lista_sentencias '}' lista_sentencias  { sprintf (temp, "\n\t\t(progn\n%s\t\t)\n\t)\n%s", $3.code, $5.code) ;
                                                                    $$.code = gen_code (temp) ; }
            ;


lista_parametros:
                parametro resto_parametros { sprintf(temp, "%s%s", $1.code, $2.code); 
                                             $$.code = gen_code(temp); }
            |   /* vacio */                { $$.code = gen_code(""); }
            ;

parametro:
                INTEGER IDENTIF {
                    add_local_var($2.code); // ¡Lo registramos como variable local!
                    $$.code = gen_code(resolve_var($2.code)); // Genera el nombre con prefijo
                }
            ;

resto_parametros:
                ',' parametro resto_parametros { sprintf(temp, " %s%s", $2.code, $3.code); 
                                                 $$.code = gen_code(temp); }
            |   /* vacio */                    { $$.code = gen_code(""); }
            ;

lista_argumentos:
                expresion resto_argumentos { sprintf(temp, "%s%s", $1.code, $2.code); 
                                             $$.code = gen_code(temp); }
            |   /* vacio */                { $$.code = gen_code(""); }
            ;

resto_argumentos:
                ',' expresion resto_argumentos { sprintf(temp, " %s%s", $2.code, $3.code); 
                                                 $$.code = gen_code(temp); }
            |   /* vacio */                    { $$.code = gen_code(""); }
            ;


// sentencias (solo válidas dentro de funciones)
sentencia:      
                IDENTIF resto_sentencia_identif     
                    {if ($2.value == 1) { 
                        // Camino 1: Era una asignación
                        sprintf (temp, "(setf %s %s)", resolve_var($1.code), $2.code) ;
                    } else {             
                        // Camino 2: Era una llamada a función
                        sprintf (temp, "(%s%s)", $1.code, $2.code) ; // <--- ¡AQUÍ ESTÁ EL ARREGLO!
                    }
                    $$.code = gen_code (temp) ;
                }
            |   PUTS '(' STRING ')'                             { sprintf (temp, "(print \"%s\")", $3.code) ;
                                                                    $$.code = gen_code (temp) ; }
            |   PRINTF '(' STRING ',' lista_impresion ')'       { $$ = $5 ; } // Ignoramos el string de formato ($3)
            ;


resto_sentencia_identif:
                '=' expresion            
                { 
                    $$.value = 1 ;            
                    $$.code = $2.code ;       
                }
            |   '(' lista_argumentos ')'                  
                { 
                    $$.value = 2 ;            
                    // Si hay argumentos, ponemos un espacio antes. Si no, lo dejamos vacío.
                    if (strlen($2.code) > 0) {
                        sprintf(temp, " %s", $2.code);
                        $$.code = gen_code(temp);
                    } else {
                        $$.code = gen_code("");
                    }
                }
            ;


inc_dec:
                INC '(' IDENTIF ')' {
                    sprintf (temp, "(setf %s (+ %s 1))", resolve_var($3.code), resolve_var($3.code)) ;
                    $$.code = gen_code (temp) ;
                }
            |   DEC '(' IDENTIF ')' {
                    sprintf (temp, "(setf %s (- %s 1))", resolve_var($3.code), resolve_var($3.code)) ;
                    $$.code = gen_code (temp) ;
                }
            ;

lista_impresion:
                elemento_impresion                          { $$ = $1 ; }
            |
                elemento_impresion ',' lista_impresion      { sprintf (temp, "%s\n\t%s", $1.code, $3.code) ;
                                                              $$.code = gen_code (temp) ; }
            ;

elemento_impresion:
                expresion                { sprintf (temp, "(princ %s)", $1.code) ;
                                           $$.code = gen_code (temp) ; }
            |   STRING                   { sprintf (temp, "(princ \"%s\")", $1.code) ;
                                           $$.code = gen_code (temp) ; }
            ;

expresion:      
                termino                  { $$ = $1 ; }
            |   expresion OR expresion   { sprintf (temp, "(or %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion AND expresion  { sprintf (temp, "(and %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion EQ expresion   { sprintf (temp, "(= %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion NEQ expresion  { sprintf (temp, "(/= %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion '<' expresion  { sprintf (temp, "(< %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion '>' expresion  { sprintf (temp, "(> %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion LEQ expresion  { sprintf (temp, "(<= %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion GEQ expresion  { sprintf (temp, "(>= %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion '+' expresion  { sprintf (temp, "(+ %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion '-' expresion  { sprintf (temp, "(- %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion '*' expresion  { sprintf (temp, "(* %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion '/' expresion  { sprintf (temp, "(/ %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion '%' expresion  { sprintf (temp, "(mod %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            ;

termino:        
                operando                           { $$ = $1 ; }                          
            |   '+' operando %prec UNARY_SIGN      { $$ = $2 ; } 
            |   '-' operando %prec UNARY_SIGN      { sprintf (temp, "(- %s)", $2.code) ;
                                                    $$.code = gen_code (temp) ; }  
            |   '!' operando %prec UNARY_SIGN      { sprintf (temp, "(not %s)", $2.code) ;
                                                    $$.code = gen_code (temp) ; }  
            ;

operando:       
                IDENTIF resto_operando_identif 
                { 
                    if ($2.value == 1) { 
                        // Era solo una variable
                        sprintf(temp, "%s", resolve_var($1.code));
                    } else { 
                        // Era una llamada a función
                        sprintf(temp, "(%s%s)", $1.code, $2.code);
                    }
                    $$.code = gen_code(temp);
                }
            |   NUMBER                   { sprintf (temp, "%d", $1.value) ;
                                           $$.code = gen_code (temp) ; }
            |   '(' expresion ')'        { $$ = $2 ; }
            ;

resto_operando_identif:
                /* vacio */              { $$.value = 1; $$.code = gen_code(""); }
            |   '(' lista_argumentos ')' { 
                    $$.value = 2; 
                    if (strlen($2.code) > 0) {
                        sprintf(temp, " %s", $2.code);
                        $$.code = gen_code(temp);
                    } else {
                        $$.code = gen_code("");
                    }
                }
            ;

%%                            // SECCION 4    Codigo en C

int n_line = 1 ;

int yyerror (mensaje)
char *mensaje ;
{
    fprintf (stderr, "%s en la linea %d\n", mensaje, n_line) ;
    printf ( "\n") ;	// bye
}

char *int_to_string (int n)
{
    char ltemp [2048] ;

    sprintf (ltemp, "%d", n) ;

    return gen_code (ltemp) ;
}

char *char_to_string (char c)
{
    char ltemp [2048] ;

    sprintf (ltemp, "%c", c) ;

    return gen_code (ltemp) ;
}

char *my_malloc (int nbytes)       // reserva n bytes de memoria dinamica
{
    char *p ;
    static long int nb = 0;        // sirven para contabilizar la memoria
    static int nv = 0 ;            // solicitada en total

    p = malloc (nbytes) ;
    if (p == NULL) {
        fprintf (stderr, "No queda memoria para %d bytes mas\n", nbytes) ;
        fprintf (stderr, "Reservados %ld bytes en %d llamadas\n", nb, nv) ;
        exit (0) ;
    }
    nb += (long) nbytes ;
    nv++ ;

    return p ;
}

/***************************************************************************/
/********************** Tabla de Símbolos Locales **************************/
/***************************************************************************/

int is_local_scope = 0;
char local_sym_table[100][256];
int local_sym_count = 0;
char current_func_name[256] = "main"; // Guarda la función actual

void add_local_var(char *name) {
    strcpy(local_sym_table[local_sym_count++], name);
}

int is_local_var(char *name) {
    for(int i = 0; i < local_sym_count; i++) {
        if(strcmp(local_sym_table[i], name) == 0) return 1;
    }
    return 0;
}

char* resolve_var(char *name) {
    static char resolved[512]; //con 256 nos ha dado warning de buffer overflow
    if (is_local_var(name)) {
        // Ahora prefija con el nombre de la función actual, no siempre "main"
        sprintf(resolved, "%s_%s", current_func_name, name);
        return resolved;
    }
    return name; // Si no está en la tabla local, es global
}

void clear_local_vars() {
    local_sym_count = 0; // Resetea la tabla al entrar a una nueva función
}

/***************************************************************************/
/********************** Seccion de Palabras Reservadas *********************/
/***************************************************************************/

typedef struct s_keyword { // para las palabras reservadas de C
    char *name ;
    int token ;
} t_keyword ;

t_keyword keywords [] = { // define las palabras reservadas y los
    "main",        MAIN,           // y los token asociados
    "int",         INTEGER,
    "puts",        PUTS,
    "printf",      PRINTF,
    "while",       WHILE,
    "&&",          AND,
    "||",          OR,
    "==",          EQ,
    "!=",          NEQ,
    "<=",          LEQ,
    ">=",          GEQ,
    "if",          IF,
    "else",        ELSE,
    "for",         FOR,
    "inc",         INC,
    "dec",         DEC,
    "switch",      SWITCH,
    "case",        CASE,
    "default",     DEFAULT,
    "break",       BREAK,
    NULL,          0               // para marcar el fin de la tabla
} ;

t_keyword *search_keyword (char *symbol_name)
{                                  // Busca n_s en la tabla de pal.res.
                                   // y devuelve puntero a registro (simbolo)
    int i ;
    t_keyword *sim ;

    i = 0 ;
    sim = keywords ;
    while (sim [i].name != NULL) {
	    if (strcmp (sim [i].name, symbol_name) == 0) {
		                             // strcmp(a, b) devuelve == 0 si a==b
            return &(sim [i]) ;
        }
        i++ ;
    }

    return NULL ;
}

 
/***************************************************************************/
/******************* Seccion del Analizador Lexicografico ******************/
/***************************************************************************/

char *gen_code (char *name)     // copia el argumento a un
{                                      // string en memoria dinamica
    char *p ;
    int l ;
	
    l = strlen (name)+1 ;
    p = (char *) my_malloc (l) ;
    strcpy (p, name) ;
	
    return p ;
}


int yylex ()
{
// NO MODIFICAR ESTA FUNCION SIN PERMISO
    int i ;
    unsigned char c ;
    unsigned char cc ;
    char ops_expandibles [] = "!<=|>%&/+-*" ;
    char temp_str [256] ;
    t_keyword *symbol ;

    do {
        c = getchar () ;

        if (c == '#') {	// Ignora las lineas que empiezan por #  (#define, #include)
            do {		//	OJO que puede funcionar mal si una linea contiene #
                c = getchar () ;
            } while (c != '\n') ;
        }

        if (c == '/') {	// Si la linea contiene un / puede ser inicio de comentario
            cc = getchar () ;
            if (cc != '/') {   // Si el siguiente char es /  es un comentario, pero...
                ungetc (cc, stdin) ;
            } else {
                c = getchar () ;	// ...
                if (c == '@') {	// Si es la secuencia //@  ==> transcribimos la linea
                    do {		// Se trata de codigo inline (Codigo embebido en C)
                        c = getchar () ;
                        putchar (c) ;
                    } while (c != '\n') ;
                } else {		// ==> comentario, ignorar la linea
                    while (c != '\n') {
                        c = getchar () ;
                    }
                }
            }
        } else if (c == '\\') c = getchar () ;
		
        if (c == '\n')
            n_line++ ;

    } while (c == ' ' || c == '\n' || c == 10 || c == 13 || c == '\t') ;

    if (c == '\"') {
        i = 0 ;
        do {
            c = getchar () ;
            temp_str [i++] = c ;
        } while (c != '\"' && i < 255) ;
        if (i == 256) {
            printf ("AVISO: string con mas de 255 caracteres en linea %d\n", n_line) ;
        }		 	// habria que leer hasta el siguiente " , pero, y si falta?
        temp_str [--i] = '\0' ;
        yylval.code = gen_code (temp_str) ;
        return (STRING) ;
    }

    if (c == '.' || (c >= '0' && c <= '9')) {
        ungetc (c, stdin) ;
        scanf ("%d", &yylval.value) ;
//         printf ("\nDEV: NUMBER %d\n", yylval.value) ;        // PARA DEPURAR
        return NUMBER ;
    }

    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
        i = 0 ;
        while (((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
            (c >= '0' && c <= '9') || c == '_') && i < 255) {
            temp_str [i++] = tolower (c) ;
            c = getchar () ;
        }
        temp_str [i] = '\0' ;
        ungetc (c, stdin) ;

        yylval.code = gen_code (temp_str) ;
        symbol = search_keyword (yylval.code) ;
        if (symbol == NULL) {    // no es palabra reservada -> identificador antes vrariabre
//               printf ("\nDEV: IDENTIF %s\n", yylval.code) ;    // PARA DEPURAR
            return (IDENTIF) ;
        } else {
//               printf ("\nDEV: OTRO %s\n", yylval.code) ;       // PARA DEPURAR
            return (symbol->token) ;
        }
    }

    if (strchr (ops_expandibles, c) != NULL) { // busca c en ops_expandibles
        cc = getchar () ;
        sprintf (temp_str, "%c%c", (char) c, (char) cc) ;
        symbol = search_keyword (temp_str) ;
        if (symbol == NULL) {
            ungetc (cc, stdin) ;
            yylval.code = NULL ;
            return (c) ;
        } else {
            yylval.code = gen_code (temp_str) ; // aunque no se use
            return (symbol->token) ;
        }
    }

//    printf ("\nDEV: LITERAL %d #%c#\n", (int) c, c) ;      // PARA DEPURAR
    if (c == EOF || c == 255 || c == 26) {
//         printf ("tEOF ") ;                                // PARA DEPURAR
        return (0) ;
    }

    return c ;
}


int main ()
{
    yyparse () ;
}
