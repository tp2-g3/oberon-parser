## Divergências em relação a especificação formal da linguagem
- Parser em ANTLR suporta apenas literais de caracteres que correspondem a letras (por exemplo 'a' é suportado mas '7' não é).
- Parser em ANTLR usa Id em diversos lugares em que deveria utilizar qualifiedName, por exemplo em PointerAccess.
- Parser em ANTLR suporta(?) acesso de campo para expression, mas é especificado apenas acesso de campo para qualified identifier. O mesmo vale para acesso de lista.
- Oberon especifica apenas comentários feitos com '(\*' e  '\*)', mas o parser em ANTLR suporta somente '//' e '/\*' '\*/'.
- Parser em ANTLR não suporta DIV.
- Parser em ANTLR não suporta o operador unário +.
- Parser em ANTLR não suporta inteiros hexadecimais nem strings de um único caractere hexadecimal.
- Parser em ANTLR não suporta comentários aninhados conforme especificado pela linguagem Oberon.
- Parser em ANTLR não suporta 'ScaleFactor's em números reais (por exemplo 4.567E8 não é suportado).
- O caractere '\_' (underline) não é permitido em identificadores pela especificação de Oberon, mas é suportado pelo parser em ANTLR.
- Oberon especifica que '.' deve ser utilizado em identificadores qualificados mas o parser em ANTLR utiliza '::'.
- Parser em ANTLR não suporta '*' após definição de identificadores.
- Parser em ANTLR não suporta as relações IN e IS.
- Parser em ANTLR utiliza '&&' e '||', mas a especificação dita que deveriam ser usados '&'e 'OR'.
- Parser em ANTLR usa 'True' e 'False' ao invés de 'TRUE' e 'FALSE'.
- Parser em ANTLR não suporta sintaxe para conjuntos.
- Parser em ANTLR não contêm uma boa parte da especificação de designator. Além disso, a parte de designators que está implementada não é utilizada em expression, apenas em statement.
- Parser em ANTLR não implementa typeguards.

## Anotações
- O objeto NullValue herda de Expression, não seria melhor herdar de Value?
- Decidiu-se que serão feitas algumas extensões a AST para facilitar o desenvolvimento. Idealmente, nenhuma alteração feita quebrará a compatibilidade com o restante da implementação da linguagem.
- Não tenho certeza se a sintaxe implementada em ANTLR para acesso de campo está correta. Se não me engano seriam necessários parenteses em torno da expressão.
