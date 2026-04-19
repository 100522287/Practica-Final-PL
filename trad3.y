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
char *my_malloc (int) ; //modificado para que concuerde con el my_malloc del código de abajo
char *gen_code (char *) ;
char *int_to_string (int) ;
char *char_to_string (char) ;
extern int is_local_scope;
void add_local_var(char *name);
void add_param_var(char *name);
int is_local_var(char *name);
int is_param_var(char *name);
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
%token IDENTIF                                      // Identificador=variable
%token INTEGER                                      // identifica el tipo entero
%token STRING
%token MAIN                                         // identifica el comienzo del proc main
%token WHILE                                        // identifica el bucle while
%token PUTS                                         // identifica la función de imprimir strings
%token PRINTF                                       // identifica printf
%token AND OR EQ NEQ LEQ GEQ                        // operadores lógicos y relacionales
%token IF                                           // identifica la estructura if
%token ELSE                                         // identifica la parte else de la estructura if
%token FOR                                          // identifica el bucle for
%token INC                                          // identifica la función de incremento    
%token DEC                                          // identifica la función de decremento
%token SWITCH                                       // identifica la estructura switch
%token CASE                                         // identifica cada caso dentro de switch
%token DEFAULT                                      // identifica el caso default dentro de switch
%token BREAK                                        // identifica la sentencia break dentro de switch
%token RETURN                                       // identifica la sentencia return


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
                declaraciones_globales lista_funciones funcion_main     { $$.code = gen_code ("") ; } /* Ya no imprimimos a nivel de axioma */
            ;

/* jerarquía de declaraciones globales*/
declaraciones_globales: 
                            declaracion_global declaraciones_globales   { sprintf (temp, "%s\n%s", $1.code, $2.code) ;
                                                                        $$.code = gen_code (temp) ; }
                                                    
                            |   /* vacio */                             { $$.code = gen_code ("") ; }
                        ;

declaracion_global:    
                            INTEGER lista_variables ';'     { $$ = $2 ; printf ("%s\n", $2.code) ; } /* Imprimimos la variable global sobre la marcha */
                        ;

lista_variables:         // LV -> V | V ',' LV   
                    variable                         { $$ = $1 ; }
                
                |    variable ',' lista_variables    { sprintf (temp, "%s\n%s", $1.code, $3.code) ;
                                                        $$.code = gen_code (temp) ; }
                ;

variable:       
                /* 1. Declaración simple (ej. int a;) */
                IDENTIF 
                    {char *resolved_name;
                        if (is_local_scope) {
                            add_local_var($1.code);
                            resolved_name = resolve_var($1.code);
                        } else {
                            resolved_name = $1.code;
                        }
                        /* Lisp espera que inicialicemos, le ponemos 0 por defecto */
                        sprintf (temp, "(setq %s 0)", resolved_name) ;
                        $$.code = gen_code (temp) ; }
                
                /* 2. Declaración con asignación*/
            |   IDENTIF '=' NUMBER 
                    {char *resolved_name;
                        if (is_local_scope) {
                            add_local_var($1.code);
                            resolved_name = resolve_var($1.code);
                        } else {
                            resolved_name = $1.code;
                        }
                        sprintf (temp, "(setq %s %d)", resolved_name, $3.value) ;
                        $$.code = gen_code (temp) ; }
                
                /* 3. Declaración de vector */
            |   IDENTIF '[' NUMBER ']' 
                    {char *resolved_name;
                        if (is_local_scope) {
                            add_local_var($1.code);
                            resolved_name = resolve_var($1.code);
                        } else {
                            resolved_name = $1.code;
                        }
                        sprintf (temp, "(setq %s (make-array %d))", resolved_name, $3.value) ;
                        $$.code = gen_code (temp) ;}
            ;

/* jerarquía de la función main*/
funcion_main:   
        MAIN '(' ')' '{' { strcpy(current_func_name, "main");
        clear_local_vars(); } marcador_local declaraciones_locales lista_sentencias_f '}'   
        {
            sprintf (temp, "(defun main ()\n%s%s)", $7.code, $8.code) ;
            $$.code = gen_code (temp) ; 
            is_local_scope = 0; 
            /* Imprimimos el main sobre la marcha */
            printf ("%s\n\n", temp) ;
        }         
    ;

funcion:        
        nombre_funcion '(' marcador_local lista_parametros ')' '{' declaraciones_locales lista_sentencias_f '}'
        {
            sprintf (temp, "(defun %s (%s)\n%s%s)", $1.code, $4.code, $7.code, $8.code) ;
            $$.code = gen_code (temp) ;
            is_local_scope = 0;
            /* Imprimimos la función sobre la marcha */
            printf ("%s\n\n", temp) ;
        }
    ;

lista_funciones: // LF -> F LF | vacio
                funcion lista_funciones  { sprintf (temp, "%s\n\n%s", $1.code, $2.code) ;
                                           $$.code = gen_code (temp) ; }
            |   /* vacio */              { $$.code = gen_code ("") ; }
            ;

nombre_funcion:     // NF -> IDENTIF
                IDENTIF {strcpy(current_func_name, $1.code);
                        clear_local_vars();
                        $$ = $1;}
            ;

marcador_local:         // ML -> vacio
                /* vacio */ { is_local_scope = 1; }
            ;

declaraciones_locales: // DL -> D DL | vacio
                            INTEGER lista_variables ';' declaraciones_locales       { sprintf (temp, "\t%s\n%s", $2.code, $4.code) ;
                                                                                        $$.code = gen_code (temp) ; }
                        |   /* vacio */                                             { $$.code = gen_code ("") ; }
                        ;

lista_cases:  // LC -> CI LC | DC | vacio
                case_item lista_cases    { sprintf (temp, "%s\n%s", $1.code, $2.code) ;
                                           $$.code = gen_code (temp) ; }
            |   default_case             { $$ = $1 ; }
            |   /* vacio */              { $$.code = gen_code ("") ; }
            ;

case_item:  // CI -> CASE NUMBER ':' LS BREAK ';'
                CASE NUMBER ':' lista_sentencias BREAK ';'  { sprintf (temp, "\t\t(%d\n%s\t\t)", $2.value, $4.code) ;
                                                            $$.code = gen_code (temp) ; }
            ;

default_case:   // DC -> DEFAULT ':' LS BREAK ';' | vacio
                DEFAULT ':' lista_sentencias BREAK ';'  { sprintf (temp, "\t\t(otherwise\n%s\t\t)", $3.code) ;
                                                        $$.code = gen_code (temp) ; }
            ;

lista_parametros:
        /* vacio */                
        { $$.code = gen_code(""); }
    |   lista_parametros_no_vacia
        { $$ = $1; }
    ;

lista_parametros_no_vacia:
        /* 1. Un solo parámetro */
        parametro 
        { $$ = $1; }
        
        /* 2. Varios parámetros separados por coma */
    |   parametro ',' lista_parametros_no_vacia 
        { 
            sprintf(temp, "%s %s", $1.code, $3.code);
            $$.code = gen_code(temp); 
        }
    ;

parametro:
        INTEGER IDENTIF 
        {
            add_param_var($2.code);
            $$.code = gen_code($2.code);
        }
    ;

lista_argumentos:
        /* vacio */                
        { $$.code = gen_code(""); }
    |   lista_argumentos_no_vacia
        { $$ = $1; }
    ;

lista_argumentos_no_vacia:
        /* 1. Un solo argumento */
        expresion 
        { $$ = $1; }
        
        /* 2. Varios argumentos separados por coma */
    |   expresion ',' lista_argumentos_no_vacia 
        { 
            sprintf(temp, "%s %s", $1.code, $3.code);
            $$.code = gen_code(temp); 
        }
    ;


instruccion:
        sentencia ';'
        {
            sprintf (temp, "\t%s", $1.code) ;
            $$.code = gen_code (temp) ;
        }
    |   RETURN expresion ';'
        {
            sprintf (temp, "\t(return-from %s %s)", current_func_name, $2.code) ;
            $$.code = gen_code (temp) ;
        }
    |   WHILE '(' expresion ')' '{' lista_sentencias '}'
        {
            sprintf (temp, "\t(loop while %s do\n%s\t)", $3.code, $6.code) ;
            $$.code = gen_code (temp) ;
        }
    |   FOR '(' sentencia ';' expresion ';' inc_dec ')' '{' lista_sentencias '}'
        {
            sprintf (temp, "\t%s\n\t(loop while %s do\n%s\t\t%s\n\t)", $3.code, $5.code, $10.code, $7.code) ;
            $$.code = gen_code (temp) ;
        }
    |   IF '(' expresion ')' '{' contenido_if '}'
        {
            sprintf (temp, "\t(if %s\n%s\n\t)", $3.code, $6.code) ;
            $$.code = gen_code (temp) ;
        }
    |   IF '(' expresion ')' '{' contenido_if '}' ELSE '{' contenido_if '}'
        {
            sprintf (temp, "\t(if %s\n%s\n%s\n\t)", $3.code, $6.code, $10.code) ;
            $$.code = gen_code (temp) ;
        }
    |   SWITCH '(' IDENTIF ')' '{' lista_cases '}'
        {
            sprintf (temp, "\t(case %s\n%s\t)", resolve_var($3.code), $6.code) ;
            $$.code = gen_code (temp) ;
        }
    ;

contenido_if:
        /* vacio */
        { $$.code = gen_code ("") ; }
    |   instruccion
        {
            sprintf (temp, "\t\t%s\n", $1.code) ;
            $$.code = gen_code (temp) ;
        }
    |   instruccion instruccion lista_sentencias
        {
            sprintf (temp, "\t\t(progn\n%s\n%s\n%s\t\t)\n", $1.code, $2.code, $3.code) ;
            $$.code = gen_code (temp) ;
        }
    ;

lista_sentencias:
        /* vacio */
        { $$.code = gen_code ("") ; }
    |   instruccion lista_sentencias
        {
            sprintf (temp, "%s\n%s", $1.code, $2.code) ;
            $$.code = gen_code (temp) ;
        }
    ;

lista_sentencias_f:      
        /* vacio */
        { $$.code = gen_code ("") ; }
    |   sentencia ';' lista_sentencias_f         
        { 
            sprintf (temp, "\t%s\n%s", $1.code, $3.code) ;
            $$.code = gen_code (temp) ; 
        }
    |   RETURN expresion ';' lista_sentencias_f
        { 
            if (strlen($4.code) == 0) {
                sprintf (temp, "\t%s\n", $2.code) ;
            } else {
                sprintf (temp, "\t(return-from %s %s)\n%s", current_func_name, $2.code, $4.code) ;
            }
            $$.code = gen_code (temp) ; 
        }
    |   WHILE '(' expresion ')' '{' lista_sentencias '}' lista_sentencias_f
        { 
            sprintf (temp, "\t(loop while %s do\n%s\t)\n%s", $3.code, $6.code, $8.code) ;
            $$.code = gen_code (temp) ; 
        }
    |   FOR '(' sentencia ';' expresion ';' inc_dec ')' '{' lista_sentencias '}' lista_sentencias_f
        { 
            sprintf (temp, "\t%s\n\t(loop while %s do\n%s\t\t%s\n\t)\n%s", $3.code, $5.code, $10.code, $7.code, $12.code) ;
            $$.code = gen_code (temp) ; 
        }
    |   IF '(' expresion ')' '{' contenido_if '}' lista_sentencias_f
        { 
            sprintf (temp, "\t(if %s\n%s\t)\n%s", $3.code, $6.code, $8.code) ;
            $$.code = gen_code (temp) ; 
        }
    |   IF '(' expresion ')' '{' contenido_if '}' ELSE '{' contenido_if '}' lista_sentencias_f
        { 
            sprintf (temp, "\t(if %s\n%s\t\t%s\t)\n%s", $3.code, $6.code, $10.code, $12.code) ;
            $$.code = gen_code (temp) ; 
        }
    |   SWITCH '(' IDENTIF ')' '{' lista_cases '}' lista_sentencias_f
        { 
            sprintf (temp, "\t(case %s\n%s\t)\n%s", resolve_var($3.code), $6.code, $8.code) ;
            $$.code = gen_code (temp) ; 
        }
    ;


// sentencias (solo válidas dentro de funciones)
// sentencias (solo válidas dentro de funciones)
sentencia:      
        IDENTIF '=' expresion
        { 
            sprintf (temp, "(setf %s %s)", resolve_var($1.code), $3.code) ;
            $$.code = gen_code (temp) ;
        }
    |   IDENTIF '[' expresion ']' '=' expresion
        {
            sprintf (temp, "(setf (aref %s %s) %s)", resolve_var($1.code), $3.code, $6.code) ;
            $$.code = gen_code (temp) ;
        }
    |   IDENTIF '(' lista_argumentos ')'
        {
            if (strlen($3.code) > 0) {
                sprintf(temp, "(%s %s)", $1.code, $3.code);
            } else {
                sprintf(temp, "(%s)", $1.code);
            }
            $$.code = gen_code(temp);
        }
    |   PUTS '(' STRING ')'                             
        { 
            sprintf (temp, "(print \"%s\")", $3.code) ;
            $$.code = gen_code (temp) ; 
        }
    |   PRINTF '(' STRING ',' lista_impresion ')'       
            { 
                /* Envolvemos los múltiples princ del printf en un único progn 
                para que Lisp lo trate como una sola expresión siempre */
                sprintf (temp, "(progn\n\t\t%s\n\t)", $5.code) ;
                $$.code = gen_code (temp) ;
            }
    ;

inc_dec:        // ID -> INC '(' IDENTIF ')' | DEC '(' IDENTIF ')'
                INC '(' IDENTIF ')' {sprintf (temp, "(setf %s (+ %s 1))", resolve_var($3.code), resolve_var($3.code)) ;
                                    $$.code = gen_code (temp) ;
                }
            |   DEC '(' IDENTIF ')' {sprintf (temp, "(setf %s (- %s 1))", resolve_var($3.code), resolve_var($3.code)) ;
                                    $$.code = gen_code (temp) ;
                }
            ;

lista_impresion:        // LI -> EI | EI ',' LI
                elemento_impresion                          { $$ = $1 ; }
            
                |elemento_impresion ',' lista_impresion      { sprintf (temp, "%s\n\t%s", $1.code, $3.code) ;
                                                              $$.code = gen_code (temp) ; }
            ;

elemento_impresion:     // EI -> E | STRING
                expresion                { sprintf (temp, "(princ %s)", $1.code) ;
                                           $$.code = gen_code (temp) ; }
            |   STRING                   { sprintf (temp, "(princ \"%s\")", $1.code) ;
                                           $$.code = gen_code (temp) ; }
            ;

expresion:      // E -> E OR E | E AND E | E EQ E | E NEQ E | E '<' E | E '>' E | E LEQ E | E GEQ E | E '+' E | E '-' E | E '*' E | E '/' E | E '%' E | T 
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

termino:        // T -> O | + O | - O | ! O
                operando                           { $$ = $1 ; }                          
            |   '+' operando %prec UNARY_SIGN      { $$ = $2 ; } 
            |   '-' operando %prec UNARY_SIGN      { sprintf (temp, "(- %s)", $2.code) ;
                                                    $$.code = gen_code (temp) ; }  
            |   '!' operando %prec UNARY_SIGN      { sprintf (temp, "(not %s)", $2.code) ;
                                                    $$.code = gen_code (temp) ; }  
            ;

operando:       
        /* 1. Uso de variable simple (ej. a + 1) */
        IDENTIF
        { 
            sprintf(temp, "%s", resolve_var($1.code));
            $$.code = gen_code(temp);
        }
        
        /* 2. Uso de una función en una expresión (ej. a + suma(1, 2)) */
    |   IDENTIF '(' lista_argumentos ')'
        {
            if (strlen($3.code) > 0) {
                sprintf(temp, "(%s %s)", $1.code, $3.code);
            } else {
                sprintf(temp, "(%s)", $1.code);
            }
            $$.code = gen_code(temp);
        }
        
        /* 3. Uso de un array en una expresión (ej. a + vector[2]) */
    |   IDENTIF '[' expresion ']'
        {
            sprintf(temp, "(aref %s %s)", resolve_var($1.code), $3.code);
            $$.code = gen_code(temp);
        }
        
        /* -- El resto de operandos que ya tenías -- */
    |   NUMBER                   
        { 
            sprintf (temp, "%d", $1.value) ;
            $$.code = gen_code (temp) ; 
        }
    |   '(' expresion ')'        
        { 
            $$ = $2 ;
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

int is_local_scope = 0;             // variable global para indicar si estamos en ámbito local (dentro de función) o global
char local_sym_table[100][256];     // tabla de símbolos para variables locales (maximo 100 variables, cada nombre hasta 255 caracteres)
int local_sym_count = 0;            // contador de variables locales
char param_sym_table[100][256];     // tabla de símbolos para parámetros de función (maximo 100 parámetros, cada nombre hasta 255 caracteres)
int param_sym_count = 0;            // contador de parámetros
char current_func_name[256] = "main"; 

void add_local_var(char *name) { // añade una variable local a la tabla de símbolos locales
    strcpy(local_sym_table[local_sym_count++], name);
}

void add_param_var(char *name) { // añade un parámetro a la tabla de símbolos de parámetros
    strcpy(param_sym_table[param_sym_count++], name);
}

int is_local_var(char *name) { // comprueba si una variable es local buscando su nombre en la tabla de símbolos locales
    for(int i = 0; i < local_sym_count; i++) {
        if(strcmp(local_sym_table[i], name) == 0) return 1;
    }
    return 0;
}

int is_param_var(char *name) { // comprueba si una variable es un parámetro buscando su nombre en la tabla de símbolos de parámetros
    for(int i = 0; i < param_sym_count; i++) {
        if(strcmp(param_sym_table[i], name) == 0) return 1;
    }
    return 0;
}

char* resolve_var(char *name) { // resuelve el nombre de una variable añadiendo el prefijo de la función si es local, o dejándolo igual si es global o parámetro
    static char resolved[512];
    if (is_local_var(name)) {
        sprintf(resolved, "%s_%s", current_func_name, name);
        return resolved;
    }
    /* Los parámetros se devuelven tal cual, sin prefijo */
    if (is_param_var(name)) {
        return name;
    }
    return name; // Si no está en ninguna tabla local, es global
}

void clear_local_vars() { // limpia las tablas de símbolos locales y de parámetros al entrar en una nueva función
    local_sym_count = 0;
    param_sym_count = 0;
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
    "return",      RETURN,
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
