# Crossy Road

Clone do famoso jogo como um projeto de Laboratórios de Informática I

### Autoria do Grupo 12
 - Humberto Gil Azevedo Sampaio Gomes (a104348[at]alunos.uminho.pt)
 - José António Fernandes Alves Lopes (a104541[at]alunos.uminho.pt)

## Como começar

### Clonar o repositório

Com SSH:

```bash
$ git clone git@gitlab.com:uminho-di/li1/2223/projetos/2022li1g012.git
```

Ou por HTTPS:

```bash
$ git clone https://gitlab.com/uminho-di/li1/2223/projetos/2022li1g012.git
```

### Correr e instalar o jogo

Uma dependência opcional para jogar com áudio é o [mpv](https://mpv.io/).

Para correr o programa (compilação automática do projeto e das dependências):

```bash
$ cabal run exe:2022li1g012
```

Para instalar o jogo:

```bash
$ cabal install
```

## Desenvolvimento

### Interpretador

Para testar funções durante o processo de desenvolvimento, o `ghci` é iniciado com:

```bash
$ cabal repl exe:2022li1g012
```

### Testes

O projeto utiliza a biblioteca
[HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.
Para os executar:

```bash
$ cabal test
```

### Documentação

Pode gerar a documentação do projeto usando
[Haddock](https://haskell-haddock.readthedocs.io/):

```bash
$ cabal haddock --haddock-all
```

### Contribuição

Seguem-se algumas normas na escrita de código para garantir a sua uniformidade.
Estas são vagamente inspiradas nas [orientações recomendadas para Haskell](https://wiki.haskell.org/Programming_guidelines).

#### Formatação de ficheiros

 - As linhas devem ter, no máximo, um comprimento de 80 caracteres;

 - Não deixar espaço em branco numa linha vazia ou no final de uma linha
 (*trailing whitespace*);

 - Não usar tabulações. Dois espaços devem ser usados no seu lugar;

 - Um ficheiro deve terminar com o caracter *newline* ('\n', 0x0A);

 - Em guardas e casos envolvendo `where`, `let`, `do` e `case`, deve começar-se
 uma nova linha para as novas expressões, que se devem manter alinhadas;

```haskell
fact n
  | n == 0 = 1
  | n > 0  = n * fact (n - 1)
```

 - Notação de expressões lambda: preferir `\ x -> ...` a `\x -> ...`;

 - Usar um espaço para separar as parcelas de operadores (`foo == bar`).

### Nomes de objetos

 - Utilizar `camelCase` para funções e `PascalCase` para tipos, módulos, ...

 - Funções com acumulador devem ter o nome da função principal com o sufixo
   `Acc`. Exemplo:

```haskell
sum lst = sumAcc lst 0
  where sumAcc []     acc = acc
        sumAcc (x:xs) acc = sumAcc xs (x+acc)
```

#### Outras boas práticas

 - O nome das funções, dos tipos de dados e de variáveis devem estar em língua
 portuguesa;

 - Explicitar o tipo de funções;

 - Evitar funções parciais (por exemplo, usando o tipo `Data.Maybe`);

 - Procurar escrever funções sucintas e reutilizáveis para outros problemas;

 - Documentar o código com o [Haddock](https://haskell-haddock.readthedocs.io/)
 e procurar escrever testes unitários com
 [HUnit](https://hackage.haskell.org/package/HUnit).

