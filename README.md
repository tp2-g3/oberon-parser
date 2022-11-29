## Dúvidas e Comentários
Nessa seção serão adicionadas dúvidas e comentários que eventualmente apareçam durante o processo de desenvolvimento do parser.
- O caractere '\_' (underline) não é permitido em identificadores pela especificação de Oberon, mas é suportado pelo parser em ANTLR.
- Parser em ANTLR não suporta inteiros hexadecimais nem strings de um único caractere hexadecimal.
- Parser em ANTLR não suporta 'ScaleFactor's em números reais.
- Oberon especifica apenas comentários feitos com '(\*' e  '\*)', mas o parser em ANTLR suporta '//' e '/\*' '\*/'.
- Parser em ANTLR não suporta comentários aninhados conforme especificado pela linguagem Oberon.
- Oberon especifica que '.' deve ser utilizado em identificadores qualificados mas o parser em ANTLR utiliza '::'.
- Parser em ANTLR não suporta '*' após definição de identificadores.
- Parser em ANTLR suporta apenas literais de caracteres que correspondem a letras (por exemplo 'a' é suportado mas '7' não é).
- O objeto NullValue herda de expressão, não seria melhor herdar de Value?