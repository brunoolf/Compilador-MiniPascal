/* 
  Compiladores – MiniPascal

  Integrantes:
  - Bruno Lauand Ferrão - 10401081
  - Fernando Pegoraro Bilia - 10402097

  Compilar:
    gcc -g -Og -Wall compilador.c -o compilador

  Rodar:
    ./compilador fonte.txt
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#define MAX_LEXEMA 128
#define MAX_TOKENS_OUT_LINE 256
#define MAX_SYMS 4096
#define MAX_SCOPE_DEPTH 128

typedef enum {
    T_ERRO = 0,
    T_IDENT, T_NUMERO,
    T_TRUE, T_FALSE,
    T_PROGRAM, T_BEGIN, T_END, T_PROCEDURE, T_FUNCTION,
    T_IF, T_THEN, T_ELSE, T_WHILE, T_DO, T_READ, T_WRITE,
    T_VAR, T_INTEGER, T_BOOLEAN, T_AND, T_OR, T_NOT, T_DIV,
    T_ATRIB,      // :=
    T_IGUAL,      // =
    T_DIF,        // <>
    T_MENOR,      // <
    T_MAIOR,      // >
    T_MENORIG,    // <=
    T_MAIORIG,    // >=
    T_MAIS,       // +
    T_MENOS,      // -
    T_VEZES,      // *
    T_ABREPAR,    // (
    T_FECHAPAR,   // )
    T_VIRG,       // ,
    T_DOISPTS,    // :
    T_PONTO,      // .
    T_PVIRG,      // ;
    T_EOF
} TAtomo;

typedef struct {
    TAtomo tipo;
    char lexema[MAX_LEXEMA];
    int linha;
    long atributo_i;
} Token;

/* simbolos (tabela da Fase 1) */
typedef enum { SYM_VAR=1, SYM_PROC, SYM_FUNC, SYM_CONSTBOOL, SYM_CONSTINT } SymKind;
typedef enum { TY_NONE=0, TY_INTEGER, TY_BOOLEAN } TypeKind;

typedef struct {
    char nome[MAX_LEXEMA];
    SymKind classe;
    TypeKind tipo;
    int scope_level;
} Simbolo;

/* === Minitabela de símbolos da Fase 2 (lista encadeada) === */
typedef struct _TNo {
    char ID[16];
    int endereco;
    char tipo[7]; /* "integer" ou "boolean" */
    struct _TNo *prox;
} TNo;

static TNo *lista_vars = NULL;
static int prox_endereco = 0;

/* lexer */
static FILE *fonte = NULL;
static FILE *ftokens = NULL;
static int linha_atual = 1;

/* arquivo de código intermediário */
static FILE *fcode = NULL;

/* parser */
static Token atual;

/* tabela de simbolos e escopos (fase 1) */
static Simbolo tabela[MAX_SYMS];
static int nsym = 0;
static int scope_stack[MAX_SCOPE_DEPTH];
static int scope_top = -1;
static int escopo_atual = 0;

/* rótulos e temporários */
static int rotulo_atual = 0;
static int temp_atual = 0;

/* protótipos principais */
static void avancar(void);
static void consome(TAtomo t);
static void erro_semantico(const char* msg, const char* lexema, int linha);

/* funções auxiliares de código */
static void gera(const char *fmt, ...) {
    if (!fcode) return;
    va_list args;
    va_start(args, fmt);
    vfprintf(fcode, fmt, args);
    va_end(args);
    fprintf(fcode, "\n");
}

static int proximo_rotulo(void) {
    return ++rotulo_atual;
}

static void novo_temp(char *buf, size_t n) {
    snprintf(buf, n, "T%d", ++temp_atual);
}

/* ----------------- UTILS ----------------- */
static const char* nome_token(TAtomo t){
    switch(t){
        case T_ERRO: return "ERRO";
        case T_IDENT: return "IDENTIFICADOR";
        case T_NUMERO: return "NUMERO";
        case T_TRUE: return "TRUE";
        case T_FALSE: return "FALSE";
        case T_PROGRAM: return "PROGRAM";
        case T_BEGIN: return "BEGIN";
        case T_END: return "END";
        case T_PROCEDURE: return "PROCEDURE";
        case T_FUNCTION: return "FUNCTION";
        case T_IF: return "IF";
        case T_THEN: return "THEN";
        case T_ELSE: return "ELSE";
        case T_WHILE: return "WHILE";
        case T_DO: return "DO";
        case T_READ: return "READ";
        case T_WRITE: return "WRITE";
        case T_VAR: return "VAR";
        case T_INTEGER: return "INTEGER";
        case T_BOOLEAN: return "BOOLEAN";
        case T_AND: return "AND";
        case T_OR: return "OR";
        case T_NOT: return "NOT";
        case T_DIV: return "DIV";
        case T_ATRIB: return "ATRIB";
        case T_IGUAL: return "IGUAL";
        case T_DIF: return "DIF";
        case T_MENOR: return "MENOR";
        case T_MAIOR: return "MAIOR";
        case T_MENORIG: return "MENORIG";
        case T_MAIORIG: return "MAIORIG";
        case T_MAIS: return "MAIS";
        case T_MENOS: return "MENOS";
        case T_VEZES: return "VEZES";
        case T_ABREPAR: return "ABREPAR";
        case T_FECHAPAR: return "FECHAPAR";
        case T_VIRG: return "VIRGULA";
        case T_DOISPTS: return "DOISPONTOS";
        case T_PONTO: return "PONTO";
        case T_PVIRG: return "PONTOVIRGULA";
        case T_EOF: return "EOS";
        default: return "DESCONHECIDO";
    }
}

static void fecha_tudo_e_morre(int status) {
    if (ftokens) fclose(ftokens);
    if (fonte) fclose(fonte);
    if (fcode) fclose(fcode);
    exit(status);
}

static void erro_lexico(const char* msg, const char* seq){
    fprintf(stderr, "ERRO LEXICO: %s proximo de '%s' (linha %d)\n", msg, seq, linha_atual);
    fecha_tudo_e_morre(1);
}

static void erro_sintatico(const char* msg, Token t){
    fprintf(stderr, "ERRO SINTATICO: %s em '%s' (linha %d)\n", msg, t.lexema, t.linha);
    fecha_tudo_e_morre(1);
}

/* Erro semântico (Fase 2) */
static void erro_semantico(const char* msg, const char* lexema, int linha){
    if (!lexema) lexema = "";
    fprintf(stderr, "ERRO SEMANTICO: %s '%s' (linha %d)\n", msg, lexema, linha);
    fecha_tudo_e_morre(1);
}

/* ----------------- TABELA DE SÍMBOLOS (Fase 1) ----------------- */
static void push_scope(void){
    scope_top++;
    if(scope_top >= MAX_SCOPE_DEPTH){ fprintf(stderr,"Escopo profundo demais\n"); fecha_tudo_e_morre(1); }
    escopo_atual++;
    scope_stack[scope_top] = escopo_atual;
}

static void pop_scope(void){
    if(scope_top < 0) return;
    int lvl = scope_stack[scope_top--];
    for(int i=nsym-1;i>=0;i--){
        if(tabela[i].scope_level == lvl){
            nsym = i;
            break;
        }
    }
}

static int find_symbol(const char* nome){
    for(int i=nsym-1;i>=0;i--){
        if(strcmp(tabela[i].nome, nome)==0) return i;
    }
    return -1;
}

/* Redeclaração "sintática" só pra PROC/FUNC/PROGRAM.
   Para VAR, a duplicidade é tratada como ERRO SEMÂNTICO na minitabela. */
static void declare_symbol(const char* nome, SymKind classe, TypeKind tipo){
    if(nsym >= MAX_SYMS){ fprintf(stderr,"Tabela de simbolos cheia\n"); fecha_tudo_e_morre(1); }

    if (classe != SYM_VAR) {
        for(int i=nsym-1;i>=0;i--){
            if(tabela[i].scope_level == escopo_atual && strcmp(tabela[i].nome, nome)==0){
                fprintf(stderr, "ERRO SINTATICO: identificador '%s' ja declarado neste escopo (linha %d)\n", nome, linha_atual);
                fecha_tudo_e_morre(1);
            }
            if(tabela[i].scope_level < escopo_atual) break;
        }
    }

    strncpy(tabela[nsym].nome, nome, MAX_LEXEMA-1);
    tabela[nsym].nome[MAX_LEXEMA-1]='\0';
    tabela[nsym].classe = classe;
    tabela[nsym].tipo = tipo;
    tabela[nsym].scope_level = escopo_atual;
    nsym++;
}

/* Impressão de tokens */
static void print_token(const Token* tk){
    char linha_out[MAX_TOKENS_OUT_LINE];
    if(tk->tipo==T_IDENT || tk->tipo==T_NUMERO || tk->tipo==T_TRUE || tk->tipo==T_FALSE){
        snprintf(linha_out, sizeof(linha_out), "%d# %s | %s\n", tk->linha, nome_token(tk->tipo), tk->lexema);
    }else{
        snprintf(linha_out, sizeof(linha_out), "%d# %s\n", tk->linha, nome_token(tk->tipo));
    }
    fputs(linha_out, stdout);
    if (ftokens) fputs(linha_out, ftokens);
}

/* === Minitabela de símbolos (lista encadeada, Fase 2) === */

static void insere_minitabela(const char *id, TypeKind tipo, int linha){
    /* Verifica redeclaração de variável (semântico) */
    TNo *p = lista_vars;
    while (p) {
        if (strcmp(p->ID, id) == 0) {
            erro_semantico("variavel redeclarada", id, linha);
        }
        p = p->prox;
    }

    TNo *novo = (TNo*) malloc(sizeof(TNo));
    if (!novo) {
        fprintf(stderr, "Falta memoria na minitabela de simbolos\n");
        fecha_tudo_e_morre(1);
    }

    strncpy(novo->ID, id, sizeof(novo->ID)-1);
    novo->ID[sizeof(novo->ID)-1] = '\0';
    novo->endereco = prox_endereco++;

    if (tipo == TY_INTEGER) {
        strcpy(novo->tipo, "integer");
    } else if (tipo == TY_BOOLEAN) {
        strcpy(novo->tipo, "boolean");
    } else {
        strcpy(novo->tipo, "");
    }

    novo->prox = lista_vars;
    lista_vars = novo;
}

static TNo* busca_minitabela(const char *id){
    TNo *p = lista_vars;
    while (p) {
        if (strcmp(p->ID, id) == 0) return p;
        p = p->prox;
    }
    return NULL;
}

/* Função do enunciado: retorna endereço ou erro semântico */
static int busca_tabela_simbolos(const char *id) {
    TNo *p = busca_minitabela(id);
    if (!p) {
        erro_semantico("variavel nao declarada", id, linha_atual);
    }
    return p->endereco;
}

static TypeKind tipo_minitabela(const char *id, int linha){
    TNo *p = busca_minitabela(id);
    if (!p) {
        erro_semantico("variavel nao declarada", id, linha);
    }
    if (strcmp(p->tipo, "integer") == 0) return TY_INTEGER;
    if (strcmp(p->tipo, "boolean") == 0) return TY_BOOLEAN;
    return TY_NONE;
}

/* ----------------- LEXER ----------------- */

static int proximo_char(void){
    int c = fgetc(fonte);
    if(c == '\n') linha_atual++;
    return c;
}

static void unread_char(int c){
    if(c == EOF) return;
    ungetc(c, fonte);
}

static TAtomo keyword_or_ident(const char *lex){
    if(strcmp(lex,"program")==0) return T_PROGRAM;
    if(strcmp(lex,"begin")==0) return T_BEGIN;
    if(strcmp(lex,"end")==0) return T_END;
    if(strcmp(lex,"procedure")==0) return T_PROCEDURE;
    if(strcmp(lex,"function")==0) return T_FUNCTION;
    if(strcmp(lex,"if")==0) return T_IF;
    if(strcmp(lex,"then")==0) return T_THEN;
    if(strcmp(lex,"else")==0) return T_ELSE;
    if(strcmp(lex,"while")==0) return T_WHILE;
    if(strcmp(lex,"do")==0) return T_DO;
    if(strcmp(lex,"read")==0) return T_READ;
    if(strcmp(lex,"write")==0) return T_WRITE;
    if(strcmp(lex,"var")==0) return T_VAR;
    if(strcmp(lex,"integer")==0) return T_INTEGER;
    if(strcmp(lex,"boolean")==0) return T_BOOLEAN;
    if(strcmp(lex,"and")==0) return T_AND;
    if(strcmp(lex,"or")==0) return T_OR;
    if(strcmp(lex,"not")==0) return T_NOT;
    if(strcmp(lex,"div")==0) return T_DIV;
    if(strcmp(lex,"true")==0) return T_TRUE;
    if(strcmp(lex,"false")==0) return T_FALSE;
    return T_IDENT;
}

static Token obter_atomo(void){
    Token tk; tk.tipo = T_ERRO; tk.lexema[0]='\0'; tk.linha = linha_atual; tk.atributo_i=0;

    int c;
    while(1){
        c = proximo_char();
        if(c == EOF){ tk.tipo = T_EOF; tk.linha = linha_atual; strcpy(tk.lexema,"<eof>"); return tk; }
        if(isspace(c)) continue;
        if(c=='/'){
            int d = proximo_char();
            if(d=='*'){
                int prev=' ';
                while(1){
                    int e = proximo_char();
                    if(e==EOF) erro_lexico("comentario nao fechado", "/* ...");
                    if(prev=='*' && e=='/'){ break; }
                    prev = e;
                }
                continue;
            }else{
                unread_char(d);
                break;
            }
        }
        break;
    }

    tk.linha = linha_atual;

    if(isalpha(c) || c=='_'){
        int i=0;
        tk.lexema[i++] = (char)c;
        while(1){
            int d = proximo_char();
            if(isalnum(d) || d=='_'){
                if(i<MAX_LEXEMA-1) tk.lexema[i++]=(char)d;
            }else{
                unread_char(d);
                break;
            }
        }
        tk.lexema[i]='\0';
        tk.tipo = keyword_or_ident(tk.lexema);
        return tk;
    }

    if (isdigit(c)) {
        int i = 0;
        tk.lexema[i++] = (char)c;

        while (1) {
            int d = proximo_char();

            if (isdigit(d)) {
                if (i < MAX_LEXEMA - 1)
                    tk.lexema[i++] = (char)d;
            }
            else if (d == '.' || d == ',') {
                /* numero real (1.5 ou 1,5) → erro léxico */
                char seq[MAX_LEXEMA + 2];
                int k;

                for (k = 0; k < i && k < MAX_LEXEMA; k++) {
                    seq[k] = tk.lexema[k];
                }
                if (k < MAX_LEXEMA + 1) {
                    seq[k++] = (char)d;
                }
                seq[k] = '\0';

                erro_lexico("Numeros reais nao sao permitidos", seq);
            }
            else {
                unread_char(d);
                break;
            }
        }

        tk.lexema[i] = '\0';
        tk.tipo = T_NUMERO;
        tk.atributo_i = strtol(tk.lexema, NULL, 10);
        return tk;
    }

    switch(c){
        case ':': {
            int d = proximo_char();
            if(d=='='){ tk.tipo=T_ATRIB; strcpy(tk.lexema,":="); }
            else{ unread_char(d); tk.tipo=T_DOISPTS; strcpy(tk.lexema,":"); }
            return tk;
        }
        case '=': tk.tipo=T_IGUAL; strcpy(tk.lexema,"="); return tk;
        case '<': {
            int d = proximo_char();
            if(d=='>'){ tk.tipo=T_DIF; strcpy(tk.lexema,"<>"); }
            else if(d=='='){ tk.tipo=T_MENORIG; strcpy(tk.lexema,"<="); }
            else { unread_char(d); tk.tipo=T_MENOR; strcpy(tk.lexema,"<"); }
            return tk;
        }
        case '>': {
            int d = proximo_char();
            if(d=='='){ tk.tipo=T_MAIORIG; strcpy(tk.lexema,">="); }
            else { unread_char(d); tk.tipo=T_MAIOR; strcpy(tk.lexema,">"); }
            return tk;
        }
        case '+': tk.tipo=T_MAIS; strcpy(tk.lexema,"+"); return tk;
        case '-': tk.tipo=T_MENOS; strcpy(tk.lexema,"-"); return tk;
        case '*': tk.tipo=T_VEZES; strcpy(tk.lexema,"*"); return tk;
        case '(': tk.tipo=T_ABREPAR; strcpy(tk.lexema,"("); return tk;
        case ')': tk.tipo=T_FECHAPAR; strcpy(tk.lexema,")"); return tk;
        case ',': tk.tipo=T_VIRG; strcpy(tk.lexema,","); return tk;
        case '.': tk.tipo=T_PONTO; strcpy(tk.lexema,"."); return tk;
        case ';': tk.tipo=T_PVIRG; strcpy(tk.lexema,";"); return tk;
        default: {
            char s[4]; s[0]=(char)c; s[1]='\0';
            erro_lexico("simbolo invalido", s);
        }
    }
    return tk;
}

/* ------------ PROTÓTIPOS DO PARSER ------------ */
static void parse_program(void);
static void parse_block(void);
static void parse_var_decl_part(void);
static void parse_var_decl(void);
static TypeKind parse_type(void);
static void parse_subroutine_decl_part(void);
static void parse_procedure_decl(void);
static void parse_function_decl(void);
static void parse_formal_parameters(void);
static void parse_formal_var_part(void);
static void parse_statement_part(void);
static void parse_statement(void);
static void parse_assignment_or_call(void);
static void parse_read(void);
static void parse_write(void);
static void parse_compound(void);
static void parse_if(void);
static void parse_while(void);
static TypeKind parse_expression(char *res);
static TypeKind parse_simple_expression(char *res);
static TypeKind parse_term(char *res);
static TypeKind parse_factor(char *res);
static void parse_parameters_list(void);

/* ------------ PARSER ------------ */
static void avancar(void){
    atual = obter_atomo();
    print_token(&atual);
}

static void consome(TAtomo t){
    if(atual.tipo == t){
        avancar();
    }else{
        char msg[128];
        snprintf(msg,sizeof(msg),"esperado %s", nome_token(t));
        erro_sintatico(msg, atual);
    }
}

/* <program> ::= program <id> ; <block> . */
static void parse_program(void){
    char progname[MAX_LEXEMA] = "";
    consome(T_PROGRAM);
    if(atual.tipo != T_IDENT) erro_sintatico("identificador esperado apos 'program'", atual);
    declare_symbol(atual.lexema, SYM_FUNC, TY_NONE);
    strncpy(progname, atual.lexema, MAX_LEXEMA-1);
    progname[MAX_LEXEMA-1] = '\0';
    consome(T_IDENT);
    consome(T_PVIRG);

    /* Cabeçalho de código */
    gera("PROGRAM %s", progname);

    push_scope();
    parse_block();
    consome(T_PONTO);
    pop_scope();

    gera("ENDPROGRAM %s", progname);
}

/* <block> ::= <var part> <subroutine part> <statement part> */
static void parse_block(void){
    parse_var_decl_part();
    parse_subroutine_decl_part();
    parse_statement_part();
}

/* aceitar varios blocos que iniciam por 'var' */
static void parse_var_decl_part(void){
    while (atual.tipo == T_VAR) {
        consome(T_VAR);
        parse_var_decl();
        consome(T_PVIRG);
    }
}

/* <variable declaration> ::= <id>{, <id>} : <type> */
static void parse_var_decl(void){
    char ids[128][MAX_LEXEMA];
    int nids=0;
    if(atual.tipo != T_IDENT) erro_sintatico("identificador esperado na declaracao", atual);
    strncpy(ids[nids++], atual.lexema, MAX_LEXEMA-1);
    ids[nids-1][MAX_LEXEMA-1] = '\0';
    consome(T_IDENT);
    while(atual.tipo==T_VIRG){
        consome(T_VIRG);
        if(atual.tipo!=T_IDENT) erro_sintatico("identificador apos ','", atual);
        strncpy(ids[nids++], atual.lexema, MAX_LEXEMA-1);
        ids[nids-1][MAX_LEXEMA-1] = '\0';
        consome(T_IDENT);
    }
    consome(T_DOISPTS);
    TypeKind ty = parse_type();
    for(int i=0;i<nids;i++) {
        declare_symbol(ids[i], SYM_VAR, ty);
        insere_minitabela(ids[i], ty, linha_atual);
    }
}

/* <type> ::= integer | boolean */
static TypeKind parse_type(void){
    if(atual.tipo==T_INTEGER){ consome(T_INTEGER); return TY_INTEGER; }
    if(atual.tipo==T_BOOLEAN){ consome(T_BOOLEAN); return TY_BOOLEAN; }
    erro_sintatico("tipo esperado (integer/boolean)", atual);
    return TY_NONE;
}

/* subrotinas: zero ou mais */
static void parse_subroutine_decl_part(void){
    while(atual.tipo==T_PROCEDURE || atual.tipo==T_FUNCTION){
        if(atual.tipo==T_PROCEDURE) parse_procedure_decl();
        else parse_function_decl();
    }
}

/* procedure <id> <formal parameters> ; <block> ; */
static void parse_procedure_decl(void){
    consome(T_PROCEDURE);
    if(atual.tipo!=T_IDENT) erro_sintatico("identificador de procedure esperado", atual);
    char pname[MAX_LEXEMA];
    strncpy(pname, atual.lexema, MAX_LEXEMA-1);
    pname[MAX_LEXEMA-1] = '\0';
    declare_symbol(pname, SYM_PROC, TY_NONE);
    consome(T_IDENT);
    parse_formal_parameters();
    consome(T_PVIRG);
    gera("PROC %s", pname);
    push_scope();
    parse_block();
    pop_scope();
    gera("ENDPROC %s", pname);
    consome(T_PVIRG);
}

/* function <id> <formal parameters> : <type> ; <block> ; */
static void parse_function_decl(void){
    consome(T_FUNCTION);
    if(atual.tipo!=T_IDENT) erro_sintatico("identificador de funcao esperado", atual);
    char fname[MAX_LEXEMA]; strncpy(fname, atual.lexema, MAX_LEXEMA-1);
    fname[MAX_LEXEMA-1] = '\0';
    consome(T_IDENT);
    parse_formal_parameters();
    consome(T_DOISPTS);
    TypeKind rty = parse_type();
    declare_symbol(fname, SYM_FUNC, rty);
    consome(T_PVIRG);
    gera("FUNC %s", fname);
    push_scope();
    parse_block();
    pop_scope();
    gera("ENDFUNC %s", fname);
    consome(T_PVIRG);
}

/* parametros formais: ( var <decl> [; var <decl>]* ) | vazio */
static void parse_formal_parameters(void){
    if (atual.tipo == T_ABREPAR) {
        consome(T_ABREPAR);
        if (atual.tipo == T_VAR) {
            parse_formal_var_part();
        }
        consome(T_FECHAPAR);
    } else {
        /* vazio */
    }
}

/* parametros formais com 'var' */
static void parse_formal_var_part(void){
    consome(T_VAR);
    parse_var_decl();
    while (atual.tipo == T_PVIRG){
        consome(T_PVIRG);
        if (atual.tipo == T_VAR){
            consome(T_VAR);
            parse_var_decl();
        } else if (atual.tipo == T_FECHAPAR){
            erro_sintatico("esperado VAR apos ';' em parametros formais (nao use ';' antes de ')')", atual);
        } else {
            erro_sintatico("esperado VAR apos ';' em parametros formais", atual);
        }
    }
}

/* <statement part> ::= begin <statement> { ; <statement> } end */
static void parse_statement_part(void){
    consome(T_BEGIN);
    parse_statement();
    while(atual.tipo==T_PVIRG){
        consome(T_PVIRG);
        parse_statement();
    }
    consome(T_END);
}

/* statement */
static void parse_statement(void){
    if(atual.tipo==T_BEGIN){ parse_compound(); return; }
    if(atual.tipo==T_IF){ parse_if(); return; }
    if(atual.tipo==T_WHILE){ parse_while(); return; }
    if(atual.tipo==T_READ){ parse_read(); return; }
    if(atual.tipo==T_WRITE){ parse_write(); return; }
    if(atual.tipo==T_IDENT){ parse_assignment_or_call(); return; }

    if(atual.tipo==T_PVIRG){
        erro_sintatico("comando esperado", atual);
    }
}

/* bloco begin ... end */
static void parse_compound(void){
    consome(T_BEGIN);
    parse_statement();
    while(atual.tipo==T_PVIRG){
        consome(T_PVIRG);
        parse_statement();
    }
    consome(T_END);
}

/* atribuicao ou chamada de procedure */
static void parse_assignment_or_call(void){
    char idnome[MAX_LEXEMA];
    int linha_id = atual.linha;
    strncpy(idnome, atual.lexema, MAX_LEXEMA-1);
    idnome[MAX_LEXEMA-1] = '\0';

    consome(T_IDENT);

    if (atual.tipo == T_ATRIB) {
        consome(T_ATRIB);

        /* Semântica: checa tipo e existência na minitabela (ou erro semântico) */
        TypeKind tvar = tipo_minitabela(idnome, linha_id);

        /* uso explícito da função do enunciado (busca_tabela_simbolos) */
        int addr = busca_tabela_simbolos(idnome);
        (void)addr; /* evita warning; endereço pode ser usado na máquina-objeto real */

        char exprres[64];
        TypeKind texp = parse_expression(exprres);

        if (tvar != TY_NONE && texp != TY_NONE && tvar != texp) {
            erro_semantico("tipos incompativeis na atribuicao da variavel", idnome, linha_id);
        }

        /* Código intermediário: MOV id, exprres */
        gera("MOV %s, %s", idnome, exprres);
        return;
    }

    if (atual.tipo == T_ABREPAR) {
        /* chamada de procedure ou function */
        int idx = find_symbol(idnome);
        if (idx < 0) {
            erro_semantico("procedure/funcao nao declarada", idnome, linha_id);
        }
        SymKind classe = tabela[idx].classe;

        consome(T_ABREPAR);
        parse_parameters_list();
        consome(T_FECHAPAR);

        gera("CALL %s", idnome);

        if (classe != SYM_PROC && classe != SYM_FUNC) {
            erro_semantico("identificador nao eh procedure/funcao", idnome, linha_id);
        }
        return;
    }

    erro_sintatico("esperado ':=' ou '(' apos identificador", atual);
}

/* read( id { , id } ) */
static void parse_read(void){
    consome(T_READ);
    consome(T_ABREPAR);
    if(atual.tipo!=T_IDENT) erro_sintatico("identificador em read()", atual);
    tipo_minitabela(atual.lexema, atual.linha);
    (void)busca_tabela_simbolos(atual.lexema);
    gera("READ %s", atual.lexema);
    consome(T_IDENT);
    while(atual.tipo==T_VIRG){
        consome(T_VIRG);
        if(atual.tipo!=T_IDENT) erro_sintatico("identificador apos ',' em read()", atual);
        tipo_minitabela(atual.lexema, atual.linha);
        (void)busca_tabela_simbolos(atual.lexema);
        gera("READ %s", atual.lexema);
        consome(T_IDENT);
    }
    consome(T_FECHAPAR);
}

/* write( id { , id } ) */
static void parse_write(void){
    consome(T_WRITE);
    consome(T_ABREPAR);
    if(atual.tipo!=T_IDENT) erro_sintatico("identificador em write()", atual);
    tipo_minitabela(atual.lexema, atual.linha);
    (void)busca_tabela_simbolos(atual.lexema);
    gera("WRITE %s", atual.lexema);
    consome(T_IDENT);
    while(atual.tipo==T_VIRG){
        consome(T_VIRG);
        if(atual.tipo!=T_IDENT) erro_sintatico("identificador apos ',' em write()", atual);
        tipo_minitabela(atual.lexema, atual.linha);
        (void)busca_tabela_simbolos(atual.lexema);
        gera("WRITE %s", atual.lexema);
        consome(T_IDENT);
    }
    consome(T_FECHAPAR);
}

/* if expr then stmt [ else stmt ] */
static void parse_if(void){
    int linha_if = atual.linha;
    char lex_if[MAX_LEXEMA];
    strncpy(lex_if, atual.lexema, MAX_LEXEMA-1);
    lex_if[MAX_LEXEMA-1] = '\0';

    consome(T_IF);
    char condres[64];
    TypeKind tcond = parse_expression(condres);
    if (tcond != TY_BOOLEAN) {
        erro_semantico("condicao do IF deve ser booleana", lex_if, linha_if);
    }

    int L_else = proximo_rotulo();
    int L_end  = proximo_rotulo();

    gera("JZ %s, L%d", condres, L_else);

    consome(T_THEN);
    parse_statement();

    gera("JUMP L%d", L_end);
    gera("LABEL L%d", L_else);

    if(atual.tipo==T_ELSE){
        consome(T_ELSE);
        parse_statement();
    }

    gera("LABEL L%d", L_end);
}

/* while expr do stmt */
static void parse_while(void){
    int linha_while = atual.linha;
    char lex_while[MAX_LEXEMA];
    strncpy(lex_while, atual.lexema, MAX_LEXEMA-1);
    lex_while[MAX_LEXEMA-1] = '\0';

    consome(T_WHILE);

    int L_begin = proximo_rotulo();
    int L_end   = proximo_rotulo();

    gera("LABEL L%d", L_begin);

    char condres[64];
    TypeKind tcond = parse_expression(condres);
    if (tcond != TY_BOOLEAN) {
        erro_semantico("condicao do WHILE deve ser booleana", lex_while, linha_while);
    }

    gera("JZ %s, L%d", condres, L_end);

    consome(T_DO);
    parse_statement();

    gera("JUMP L%d", L_begin);
    gera("LABEL L%d", L_end);
}

/* expressao com relacional/booleana opcional */
static TypeKind parse_expression(char *res){
    TypeKind t1;
    char left[64], right[64];
    t1 = parse_simple_expression(left);

    if(atual.tipo==T_IGUAL || atual.tipo==T_DIF ||
       atual.tipo==T_MENOR || atual.tipo==T_MENORIG ||
       atual.tipo==T_MAIOR || atual.tipo==T_MAIORIG){
        TAtomo op = atual.tipo;
        consome(op);

        if (t1 != TY_INTEGER) {
            erro_semantico("operando deve ser integer em expressao relacional", left, atual.linha);
        }

        TypeKind t2 = parse_simple_expression(right);
        if (t2 != TY_INTEGER) {
            erro_semantico("operando deve ser integer em expressao relacional", right, atual.linha);
        }

        char tmp[64];
        novo_temp(tmp, sizeof(tmp));

        switch(op){
            case T_MENOR:   gera("CMP_LT %s, %s, %s", tmp, left, right); break;
            case T_MAIOR:   gera("CMP_GT %s, %s, %s", tmp, left, right); break;
            case T_MENORIG: gera("CMP_LE %s, %s, %s", tmp, left, right); break;
            case T_MAIORIG: gera("CMP_GE %s, %s, %s", tmp, left, right); break;
            case T_IGUAL:   gera("CMP_EQ %s, %s, %s", tmp, left, right); break;
            case T_DIF:     gera("CMP_NE %s, %s, %s", tmp, left, right); break;
            default: break;
        }

        strcpy(res, tmp);
        return TY_BOOLEAN;
    }

    if(atual.tipo==T_AND || atual.tipo==T_OR){
        TAtomo op = atual.tipo;
        const char *opstr = (op == T_AND ? "AND" : "OR");
        consome(op);

        if (t1 != TY_BOOLEAN) {
            erro_semantico("operando deve ser boolean em expressao logica", left, atual.linha);
        }
        TypeKind t2 = parse_simple_expression(right);
        if (t2 != TY_BOOLEAN) {
            erro_semantico("operando deve ser boolean em expressao logica", right, atual.linha);
        }

        char tmp[64];
        novo_temp(tmp, sizeof(tmp));
        gera("%s %s, %s, %s", opstr, tmp, left, right);
        strcpy(res, tmp);
        return TY_BOOLEAN;
    }

    strcpy(res, left);
    return t1;
}

/* [ + | - ] termo { (+|-) termo } */
static TypeKind parse_simple_expression(char *res){
    TypeKind t;
    char factor_res[64];

    if(atual.tipo==T_MAIS || atual.tipo==T_MENOS){
        TAtomo unop = atual.tipo;
        consome(unop);
        t = parse_term(factor_res);
        if (t != TY_INTEGER) {
            erro_semantico("operando de sinal deve ser integer", factor_res, atual.linha);
        }
        if (unop == T_MENOS) {
            char tmp[64];
            novo_temp(tmp, sizeof(tmp));
            gera("NEG %s, %s", tmp, factor_res);
            strcpy(res, tmp);
        } else {
            strcpy(res, factor_res);
        }
    } else {
        t = parse_term(res);
    }

    while(atual.tipo==T_MAIS || atual.tipo==T_MENOS){
        TAtomo op = atual.tipo;
        consome(op);
        TypeKind t2 = parse_term(factor_res);
        if (t != TY_INTEGER || t2 != TY_INTEGER) {
            erro_semantico("operadores '+' e '-' apenas para integer", factor_res, atual.linha);
        }
        char tmp[64];
        novo_temp(tmp, sizeof(tmp));
        if (op == T_MAIS)
            gera("ADD %s, %s, %s", tmp, res, factor_res);
        else
            gera("SUB %s, %s, %s", tmp, res, factor_res);
        strcpy(res, tmp);
        t = TY_INTEGER;
    }

    return t;
}

/* termo { (*|div) fator } */
static TypeKind parse_term(char *res){
    TypeKind t = parse_factor(res);
    char factor_res[64];

    while(atual.tipo==T_VEZES || atual.tipo==T_DIV){
        TAtomo op = atual.tipo;
        consome(op);
        TypeKind t2 = parse_factor(factor_res);
        if (t != TY_INTEGER || t2 != TY_INTEGER) {
            erro_semantico("operadores '*' e 'div' apenas para integer", factor_res, atual.linha);
        }
        char tmp[64];
        novo_temp(tmp, sizeof(tmp));
        if (op == T_VEZES)
            gera("MUL %s, %s, %s", tmp, res, factor_res);
        else
            gera("DIV %s, %s, %s", tmp, res, factor_res);
        strcpy(res, tmp);
        t = TY_INTEGER;
    }
    return t;
}

/* fator: id [ '(' params ')' ] | numero | true/false | (expr) | not fator */
static TypeKind parse_factor(char *res){
    if(atual.tipo==T_IDENT){
        char nome[MAX_LEXEMA];
        int linha_id = atual.linha;
        strncpy(nome, atual.lexema, MAX_LEXEMA-1);
        nome[MAX_LEXEMA-1] = '\0';

        consome(T_IDENT);

        if (atual.tipo == T_ABREPAR){
            int idx = find_symbol(nome);
            if (idx < 0 || tabela[idx].classe != SYM_FUNC) {
                erro_semantico("chamada de funcao invalida ou nao declarada", nome, linha_id);
            }
            consome(T_ABREPAR);
            parse_parameters_list();
            consome(T_FECHAPAR);
            gera("CALL %s", nome);
            strcpy(res, nome);
            return tabela[idx].tipo;
        } else {
            TypeKind tvar = tipo_minitabela(nome, linha_id);
            (void)busca_tabela_simbolos(nome);
            strcpy(res, nome);
            return tvar;
        }
    }
    if(atual.tipo==T_NUMERO){
        long val = atual.atributo_i;
        consome(T_NUMERO);
        char tmp[64];
        novo_temp(tmp, sizeof(tmp));
        gera("MOVI %s, %ld", tmp, val);
        strcpy(res, tmp);
        return TY_INTEGER;
    }
    if(atual.tipo==T_TRUE || atual.tipo==T_FALSE){
        int is_true = (atual.tipo == T_TRUE);
        consome(atual.tipo);
        char tmp[64];
        novo_temp(tmp, sizeof(tmp));
        gera("MOVI %s, %d", tmp, is_true ? 1 : 0);
        strcpy(res, tmp);
        return TY_BOOLEAN;
    }
    if(atual.tipo==T_ABREPAR){
        consome(T_ABREPAR);
        TypeKind t = parse_expression(res);
        consome(T_FECHAPAR);
        return t;
    }
    if(atual.tipo==T_NOT){
        consome(T_NOT);
        TypeKind t = parse_factor(res);
        if (t != TY_BOOLEAN) {
            erro_semantico("operando de 'not' deve ser boolean", res, atual.linha);
        }
        char tmp[64];
        novo_temp(tmp, sizeof(tmp));
        gera("NOT %s, %s", tmp, res);
        strcpy(res, tmp);
        return TY_BOOLEAN;
    }
    erro_sintatico("fator invalido", atual);
    return TY_NONE;
}

/* lista de parametros em chamada: id/numero/bool {, ...} */
static void parse_parameters_list(void){
    if(atual.tipo==T_IDENT){
        int linha_id = atual.linha;
        if (find_symbol(atual.lexema) < 0 && !busca_minitabela(atual.lexema)) {
            erro_semantico("identificador de parametro nao declarado", atual.lexema, linha_id);
        }
        consome(T_IDENT);
    }else if(atual.tipo==T_NUMERO){
        consome(T_NUMERO);
    }else if(atual.tipo==T_TRUE || atual.tipo==T_FALSE){
        consome(atual.tipo);
    }else{
        erro_sintatico("parametro esperado em chamada", atual);
    }
    while(atual.tipo==T_VIRG){
        consome(T_VIRG);
        if(atual.tipo==T_IDENT){
            int linha_id = atual.linha;
            if (find_symbol(atual.lexema) < 0 && !busca_minitabela(atual.lexema)) {
                erro_semantico("identificador de parametro nao declarado", atual.lexema, linha_id);
            }
            consome(T_IDENT);
        }else if(atual.tipo==T_NUMERO){
            consome(T_NUMERO);
        }else if(atual.tipo==T_TRUE || atual.tipo==T_FALSE){
            consome(atual.tipo);
        }else{
            erro_sintatico("parametro apos ','", atual);
        }
    }
}

/* main */
int main(int argc, char** argv){
    if(argc < 2){
        fprintf(stderr, "Uso: %s <arquivo-fonte>\n", argv[0]);
        return 1;
    }

    fonte = fopen(argv[1], "r");
    if(!fonte){
        perror("Erro abrindo arquivo fonte");
        return 1;
    }
    ftokens = fopen("tokens.out","w");
    if(!ftokens){
        perror("Nao foi possivel criar tokens.out");
        fclose(fonte);
        return 1;
    }
    fcode = fopen("codigo.txt", "w");
    if(!fcode){
        perror("Nao foi possivel criar codigo.txt");
        fclose(fonte);
        fclose(ftokens);
        return 1;
    }

    escopo_atual = 0;
    push_scope();

    avancar();
    parse_program();

    if(atual.tipo != T_EOF){
        erro_sintatico("fim de arquivo esperado apos '.'", atual);
    }

    printf("Analise concluida sem erros.\n");
    fecha_tudo_e_morre(0);
    return 0;
}
