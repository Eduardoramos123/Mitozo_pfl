replicate(String, 0, []).
replicate(String, N, List) :- N1 is N-1,
                              replicate(String, N1, L2),
                              List = [String|L2].

initial_state_helper(N1, 0, String, []).
initial_state_helper(N1, N2, String, List) :- N3 is N2-1,
                                              initial_state_helper(N1, N3, String, L2),
                                              replicate(String, N1, L3),
                                              List = [L3|L2].

initial_state(Size, GameState) :-  initial_state_helper(Size, Size, 'E', GameState).

print_line([]) :- write('').
print_line([X|L]) :- write(X),
                     write(' '),
                     print_line(L).

display_game([]) :- write('').
display_game([X|GameState]) :- print_line(X),
                               nl,
                               display_game(GameState).

move2(['E'|L], 0, String, Res) :- R = String,
                                  Res = [R|L].
move2([X|L], Y, String, Res) :- Y1 is Y-1,
                                move2(L, Y1, String, Res2),
                                Res = [X|Res2].

move([Z|GameState], 0/Y, NewGameState, String) :- move2(Z, Y, String, Res),
                                                  NewGameState = [Res|GameState].
move([Z|GameState], X/Y, NewGameState, String) :- X1 is X-1,
                                                  move(GameState, X1/Y, NewGameState2, String),
                                                  NewGameState = [Z|NewGameState2]. 

get_value2([X|L], 0, X).
get_value2([X|L], Y, Value) :- Y1 is Y-1,
                               get_value2(L, Y1, Value).

get_value([Z|GameState], 0/Y, Value) :- get_value2(Z, Y, Value).
get_value([Z|GameState], X/Y, Value) :- X1 is X-1,
                                        get_value(GameState, X1/Y, Value).
 
get_true_value(GameState, X/Y, 'E') :- \+ get_value(GameState, X/Y, V).
get_true_value(GameState, X/Y, V) :- get_value(GameState, X/Y, V).

possivel_move(GameState, X/Y) :- get_true_value(GameState, X/Y, 'E').

valid_helper(GameState, X/Y, Res) :- possivel_move(GameState, X/Y),
                                     X1 is X-1,
                                     Y1 is Y-1,
                                     X2 is X+1,
                                     Y2 is Y+1,
                                     get_true_value(GameState, X1/Y1, R1),
                                     get_true_value(GameState, X1/Y, R2),
                                     get_true_value(GameState, X1/Y2, R3),
                                     get_true_value(GameState, X/Y1, R4),
                                     get_true_value(GameState, X/Y2, R5),
                                     get_true_value(GameState, X2/Y1, R6),
                                     get_true_value(GameState, X2/Y, R7),
                                     get_true_value(GameState, X2/Y2, R8),
                                     Res = [R1, R2, R3, R4, R5, R6, R7, R8].

is_there_x(['X'|L]).
is_there_x([X|L]) :- is_there_x(L).

is_there_o(['O'|L]).
is_there_o([X|L]) :- is_there_o(L).

not_valid_move(GameState, X/Y) :- X < 0.
not_valid_move(GameState, X/Y) :- length(GameState, S),
                                  X >= S.
not_valid_move(GameState, X/Y) :- Y < 0.
not_valid_move(GameState, X/Y) :- length(GameState, S),
                                  Y >= S.

valid_player_o(GameState, X/Y) :- valid_helper(GameState, X/Y, L),
                                  \+ not_valid_move(GameState, X/Y),
                                  is_there_x(L),
                                  is_there_o(L).
valid_player_o(GameState, X/Y) :- valid_helper(GameState, X/Y, L),
                                  \+ not_valid_move(GameState, X/Y),
                                  \+ is_there_x(L),
                                  \+ is_there_o(L).
valid_player_o(GameState, X/Y) :- valid_helper(GameState, X/Y, L),
                                  \+ not_valid_move(GameState, X/Y),
                                  \+ is_there_x(L),
                                  is_there_o(L).

valid_player_x(GameState, X/Y) :- valid_helper(GameState, X/Y, L),
                                  \+ not_valid_move(GameState, X/Y),
                                  is_there_x(L),
                                  is_there_o(L).
valid_player_x(GameState, X/Y) :- valid_helper(GameState, X/Y, L),
                                  \+ not_valid_move(GameState, X/Y),
                                  \+ is_there_x(L),
                                  \+ is_there_o(L).
valid_player_x(GameState, X/Y) :- valid_helper(GameState, X/Y, L),
                                  \+ not_valid_move(GameState, X/Y),
                                  is_there_x(L),
                                  \+ is_there_o(L).

all_possivel_x_helper(GameState, X/Y, S) :- Y < S,
                                            get_true_value(GameState, X/Y, 'E'),
                                            valid_player_x(GameState, X/Y).
all_possivel_x_helper(GameState, X/Y, S) :- Y < S,
                                            Y1 is Y+1,
                                            all_possivel_x_helper(GameState, X/Y1, S).

all_possivel_x(GameState, X/Y, S) :- X < S,
                                     all_possivel_x_helper(GameState, X/0, S).
all_possivel_x(GameState, X/Y, S) :- X < S,
                                     X1 is X+1,
                                     \+ all_possivel_x_helper(GameState, X/0, S),
                                     all_possivel_x(GameState, X1/0, S).  

all_possivel_o_helper(GameState, X/Y, S) :- Y < S,
                                            get_true_value(GameState, X/Y, 'E'),
                                            valid_player_o(GameState, X/Y).
all_possivel_o_helper(GameState, X/Y, S) :- Y < S,
                                            Y1 is Y+1,
                                            all_possivel_o_helper(GameState, X/Y1, S).

all_possivel_o(GameState, X/Y, S) :- X < S,
                                     all_possivel_o_helper(GameState, X/0, S).
all_possivel_o(GameState, X/Y, S) :- X < S,
                                     X1 is X+1,
                                     \+ all_possivel_o_helper(GameState, X/0, S),
                                     all_possivel_o(GameState, X1/0, S).

get_player_x_coords(GameState, X/Y) :- write('Selecione as coordenadas X/Y: '),
                                       read(X/Y),
                                       write(X/Y),
                                       nl,
                                       valid_player_x(GameState, X/Y).
get_player_x_coords(GameState, X/Y) :- get_player_x_coords(GameState, X/Y).

get_player_o_coords(GameState, X/Y) :- write('Selecione as coordenadas aqui X/Y: '),
                                       read(X/Y),
                                       write(X/Y),
                                       nl,
                                       valid_player_o(GameState, X/Y).
get_player_o_coords(GameState, X/Y) :- get_player_o_coords(GameState, X/Y).












 
 




