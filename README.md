## MiniPascal — Projeto de Compiladores

### Integrantes:

- Bruno Lauand Ferrão - 10401081
- Fernando Pegoraro Bilia - 10402097

### Descrição:

Este projeto consiste no desenvolvimento de um compilador para uma linguagem simplificada denominada MP, seguindo exatamente as especificações fornecidas na disciplina de Compiladores.

### O trabalho foi dividido em duas fases:

- Parte 1: Implementação da análise léxica e sintática.
- Parte 2: Implementação da análise semântica e da geração de código intermediário.

O compilador foi implementado totalmente em C, utilizando apenas bibliotecas padrão.

### Como compilar:

``` gcc -g -Og -Wall compilador.c -o compilador ```

### Como executar:

``` ./compilador fonte.txt ```

## Parte 1 - Análise Léxica e Sintática:

### Analisador Léxico:

- Reconhecimento de todos os tokens definidos para a linguagem MP.
- Tratamento de erros léxicos com mensagens claras.
- Rejeição explícita de números reais (exigido pelo enunciado).

### Analisador Sintático:

- Implementação completa da gramática através de analisador descendente recursivo.
- Cada chamada de consome() imprime os tokens lidos (para depuração / correção).
- Tratamento de erros sintáticos com mensagens explicativas contendo:
  - token encontrado,
  - token esperado,
  - linha do erro.

### Testes realizados:

Incluímos testes variados para verificar:

- Tokens inválidos
- Estruturas ausentes
- Erros de ordem de símbolos
- Comandos faltando end, parênteses, ponto, etc

## Parte 2 - Análise semântica

Nesta fase o compilador passou a validar a coerência semântica do programa, seguindo exatamente os três requisitos do enunciado.

### 1. Verificação de Declarações

Foi implementada uma minitabela de símbolos estruturada como lista encadeada, conforme exigido:

typedef struct _TNo {
    char ID[16];
    int endereco;
    char tipo[7];
    struct _TNo *prox;
} TNo;

A tabela armazena:
- nome do identificador,
- endereço (ordem de declaração, iniciando em 0),
- tipo (integer ou boolean).

O compilador detecta:
- variáveis declaradas corretamente
- erro semântico quando uma variável é redeclarada, finalizando a compilação.

### 2. Verificação de Uso no Corpo do Programa

Sempre que um identificador é utilizado:

- em atribuições,
- em expressões aritméticas, lógicas ou relacionais,
- em condições de if e while,
- em comandos de read e write,

O compilador verifica se a variável foi declarada previamente.

Caso contrário:
- Erro semântico: “variavel nao declarada ‘x’ (linha N)”

### 3. Checagem de Tipos
A análise de tipos foi implementada cobrindo todos os casos do enunciado:

Expressões Aritméticas (+, -, *, div)
- Apenas integer.
- Caso contrário → erro semântico explicativo.

Expressões Relacionais (<, >, <=, etc.)

- Operadores exigem inteiros.
- Se um operando não for integer → erro semântico.

Expressões Lógicas (and, or, not)

- Operadores exigem booleanos.
- Se um operando não for boolean → erro semântico.

Atribuição
O compilador compara:
- tipo da variável à esquerda
- tipo da expressão à direita
Se não coincidirem → erro semântico.

Condicionais (if, while)
A expressão dentro da condição deve ser boolean.

## Geração de Código Intermediário

``` proximo_rotulo(): ```

Gera rótulos numéricos crescentes:
``` L1, L2, L3, ... ```

Código Gerado:

O compilador cria um arquivo:
``` codigo.txt ```

Com instruções da máquina-objeto, incluindo:

- PUSH
- ADD, SUB, MUL, DIV
- STORE, LOAD
- JMP, JMPF
- READ, WRITE
- Rótulos (L1:)

Variáveis são usadas pelo nome, conforme exigido.

Estruturas com rotulagem completa:

- atribuições
- expressões
- if / if-else
- while
- comandos read / write

### Testes Realizados na Fase 2

Foram testados:

Redeclarações
- erro semântico corretamente lançado

Uso de variável não declarada
- erro preciso, apontando a linha

Tipos incompatíveis em expressão (ex: boolean + integer)
- erro semântico com mensagem clara

Condições inválidas
- erro apontando que if/while precisa de boolean

Operadores aplicados ao tipo errado (ex: x and y onde x é integer)
- erro semântico apontando o operando causador do problema

### Considerações Finais

- O compilador compila sem warnings, como exigido.
- Todos os requisitos da parte 1 e parte 2 foram cumpridos integralmente.
- A estrutura do código é clara, indentada e comentada.
- A tabela de símbolos e o gerador de rótulos seguem exatamente o enunciado.
- As mensagens de erro são completas e explicativas.
