# Mitozo
## Grupo XX
* Eduardo Ramos
* Pedro Nunes

# Descrição do jogo

## Resumo
\
Mitozo é um jogo <em>turn-based</em> em que cada jogador tenta impossiblitar o oponente de jogar.


## Regras
\
Existe apenas uma regra - se uma peça for colocada em contacto com uma do oponente deve também estar em contacto com uma peça do jogador.

## Referencias
* [Mitozo BoardGameGeek](https://boardgamegeek.com/boardgame/338168/mitozo)
# Lógica do jogo
## Estrutura interna do jogo

### Representação do estado do jogo
O tabuleiro é defenido por uma lista de listas. Cada lista possui celulas sendo estas divididas em peças diferentes.
* 'x' representa uma peça ocupada pelo jogador
* 'o' representa uma peça ocupada pelo oponente
* 'E' representa uma casa vazia
```prolog
% Estado inicial
[
['E', 'E', 'E'], 
['E', 'E', 'E'], 
['E', 'E', 'E']
]

% Estado intermédio

% Estado final

```
## Visualização do jogo

A representação visual do jogo é encarregada à função <em>display_game</em>. A representação é feita linha a linha, sendo que para tal foi criada uma função auxiliar <em>print_line</line>.
