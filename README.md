## Dúvidas e Comentários
Nessa seção serão adicionadas dúvidas e comentários que eventualmente apareçam durante o processo de desenvolvimento do parser.
- O operador MOD está com a precedência de um operador de adição no parser em ANTLR, mas deveria ter a precedência de um operador de multiplicação.
- Parser em ANTLR suporta apenas literais de caracteres que correspondem a letras (por exemplo 'a' é suportado mas '7' não é).
- Oberon especifica apenas comentários feitos com '(\*' e  '\*)', mas o parser em ANTLR suporta somente '//' e '/\*' '\*/'.
- Parser em ANTLR não suporta DIV.
- Parser em ANTLR não suporta inteiros hexadecimais nem strings de um único caractere hexadecimal.
- Parser em ANTLR não suporta comentários aninhados conforme especificado pela linguagem Oberon.
- Parser em ANTLR não suporta 'ScaleFactor's em números reais (por exemplo 4.567E8 não é suportado).
- O caractere '\_' (underline) não é permitido em identificadores pela especificação de Oberon, mas é suportado pelo parser em ANTLR.
- Oberon especifica que '.' deve ser utilizado em identificadores qualificados mas o parser em ANTLR utiliza '::'.
- Parser em ANTLR não suporta '*' após definição de identificadores.
- Parser em ANTLR não suporta as relações IN e IS.
- Parser em ANTLR utiliza '&&' e '||', mas a especificação dita que deveriam ser usados '&'e 'OR'.
- O objeto NullValue herda de expressão, não seria melhor herdar de Value?