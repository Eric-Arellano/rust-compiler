# Compiler

A recreation of the final project for a compilers class I took in college, rewritten from C++ to Rust.

## Grammar

```
program -> var_section body
var_section -> id_list SEMICOLON
id_list -> ID COMMA id_list | ID
body -> LBRACE stmt_list RBRACE
stmt_list -> stmt stmt_list | stmt
stmt -> assign_stmt | print_stmt | while_stmt | if_stmt | switch_stmt
assign_stmt -> ID EQUAL primary SEMICOLON
assign_stmt -> ID EQUAL expr SEMICOLON
expr -> primary op primary
primary -> ID | NUM
op -> PLUS | MINUS | MULT | DIV
print_stmt -> 'print' ID SEMICOLON
while_stmt -> WHILE condition body
if_stmt -> IF condition body
condition -> primary relop primary
relop -> GREATER | LESS | NOTEQUAL
switch_stmt -> SWITCH ID LBRACE case_list RBRACE
switch_stmt -> SWITCH ID LBRACE case_list default_case RBRACE
for_stmt -> FOR LPAREN assign_stmt condition SEMICOLON RPAREN body
case_list -> case case_list | case
case -> CASE NUM COLON body
default_case -> DEFAULT COLON body
```

Some notes about this grammar:

1. Expressions are simplified and not recursive.
2. Division is integer division.
3. While there are `if` statements, there is no `else` or `else if`.
4. There is no variable declaration list. There is only only `id_list` in the global scope, which contains all the variables.
5. There is no type specified for variables. All variables are `int`.
6. `NOTEQUAL` is represented with `<>`.
7. `ID`s must start with an ASCII letter. They may contain ASCII letters and ASCII numbers.
