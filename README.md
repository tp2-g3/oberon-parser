Trabalho de Técnicas de programação. UnB 2022.2

## Dúvidas e Comentários
Nessa seção serão adicionadas dúvidas e comentários que eventualmente apareçam durante o processo de desenvolvimento do parser.
- O caractere '\_' (underline) não é permitido em identificadores pela especificação de Oberon, mas é suportado pelo parser em ANTLR.
- O parser em ANTLR não suporta inteiros hexadecimais nem strings de um único caractere hexadecimal.
- O parser em ANTLR não suporta 'ScaleFactor's em números reais.
- Oberon especifica apenas comentários feitos com '(\*' e  '\*)', mas o parser em ANTLR suporta '//' e '/\*' '\*/'
- Oberon especifica que '.' deve ser utilizado em identificadores qualificados mas o parser em ANTLR utiliza '::'.