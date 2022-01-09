:- use_module(library(random)).

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

get_player_x_coords(GameState, X/Y) :- write('Player X: Select X/Y: '),
                                       read(X/Y),
                                       write(X/Y),
                                       nl,
                                       valid_player_x(GameState, X/Y).
get_player_x_coords(GameState, X/Y) :- get_player_x_coords(GameState, X/Y).

get_player_o_coords(GameState, X/Y) :- write('Player O: Select X/Y: '),
                                       read(X/Y),
                                       write(X/Y),
                                       nl,
                                       valid_player_o(GameState, X/Y).
get_player_o_coords(GameState, X/Y) :- get_player_o_coords(GameState, X/Y).

get_computer_x_coords(GameState, X/Y, S) :- random(0, S, X),
                                            random(0, S, Y),
                                            valid_player_x(GameState, X/Y).
get_computer_x_coords(GameState, X/Y, S) :- get_computer_x_coords(GameState, X/Y, S). 

get_computer_o_coords(GameState, X/Y, S) :- random(0, S, X),
                                            random(0, S, Y),
                                            valid_player_o(GameState, X/Y).
get_computer_o_coords(GameState, X/Y, S) :- get_computer_o_coords(GameState, X/Y, S).

player_x_turn(GameState, NewGameState) :- get_player_x_coords(GameState, X/Y),
                                          move(GameState, X/Y, NewGameState, 'X').

player_o_turn(GameState, NewGameState) :- get_player_o_coords(GameState, X/Y),
                                          move(GameState, X/Y, NewGameState, 'O').

computer_x_turn(GameState, S, NewGameState) :- get_computer_x_coords(GameState, X/Y, S),
                                               move(GameState, X/Y, NewGameState, 'X').

computer_o_turn(GameState, S, NewGameState) :- get_computer_o_coords(GameState, X/Y, S),
                                               move(GameState, X/Y, NewGameState, 'O').

x_turn(GameState, S, New) :- all_possivel_x(GameState, 0/0, S),
                             player_x_turn(GameState, New).

o_turn(GameState, S, New) :- all_possivel_o(GameState, 0/0, S),
                             player_o_turn(GameState, New).

x_turn_computer(GameState, S, New) :- all_possivel_x(GameState, 0/0, S),
                                      computer_x_turn(GameState, S, New).

o_turn_computer(GameState, S, New) :- all_possivel_o(GameState, 0/0, S),
                                      computer_o_turn(GameState, S, New).

gameloop_pp1(GameState, S) :- display_game(GameState),
                              x_turn(GameState, S, New),
                              gameloop_pp2(New, S).
gameloop_pp1(GameState, S) :- write('Player 2 wins!!!').

gameloop_pp2(GameState, S) :- display_game(GameState),
                              o_turn(GameState, S, New),
                              gameloop_pp1(New, S).
gameloop_pp2(GameState, S) :- write('Player 1 wins!!!').

gameloop_cc1(GameState, S) :- display_game(GameState),
                              nl,
                              x_turn_computer(GameState, S, New),
                              gameloop_cc2(New, S).
gameloop_cc1(GameState, S) :- write('Computer 2 wins!!!').

gameloop_cc2(GameState, S) :- display_game(GameState),
                              nl,
                              o_turn_computer(GameState, S, New),
                              gameloop_cc1(New, S).
gameloop_cc2(GameState, S) :- write('Computer 1 wins!!!').

gameloop_cp1(GameState, S) :- display_game(GameState),
                              nl,
                              x_turn_computer(GameState, S, New),
                              gameloop_cp2(New, S).
gameloop_cp1(GameState, S) :- write('Player 2 wins!!!').

gameloop_cp2(GameState, S) :- display_game(GameState),
                              o_turn(GameState, S, New),
                              gameloop_cp1(New, S).
gameloop_cp2(GameState, S) :- write('Computer 1 wins!!!').

gameloop_pc1(GameState, S) :- display_game(GameState),
                              x_turn(GameState, S, New),
                              gameloop_pc2(New, S).
gameloop_pc1(GameState, S) :- write('Computer 2 wins!!!').

gameloop_pc2(GameState, S) :- display_game(GameState),
                              nl,
                              o_turn_computer(GameState, S, New),
                              gameloop_pc1(New, S).
gameloop_pc2(GameState, S) :- write('Player 1 wins!!!').

logo :- write('MMMMMMMM               MMMMMMMM  iiii          tttt'),
        nl,
        write('M:::::::M             M:::::::M i::::i      ttt:::t'),
        nl,
        write('M::::::::M           M::::::::M  iiii       t:::::t'),
        nl,
        write('M:::::::::M         M:::::::::M             t:::::t'),
        nl,
        write('M::::::::::M       M::::::::::Miiiiiiittttttt:::::ttttttt       ooooooooooo   zzzzzzzzzzzzzzzzz   ooooooooooo'),
        nl,
        write('M:::::::::::M     M:::::::::::Mi:::::it:::::::::::::::::t     oo:::::::::::oo z:::::::::::::::z oo:::::::::::oo'),
        nl,
        write('M:::::::M::::M   M::::M:::::::M i::::it:::::::::::::::::t    o:::::::::::::::oz::::::::::::::z o:::::::::::::::o'),
        nl,
        write('M::::::M M::::M M::::M M::::::M i::::itttttt:::::::tttttt    o:::::ooooo:::::ozzzzzzzz::::::z  o:::::ooooo:::::o'),
        nl,
        write('M::::::M  M::::M::::M  M::::::M i::::i      t:::::t          o::::o     o::::o      z::::::z   o::::o     o::::o'),
        nl,
        write('M::::::M   M:::::::M   M::::::M i::::i      t:::::t          o::::o     o::::o     z::::::z    o::::o     o::::o'),
        nl,
        write('M::::::M    M:::::M    M::::::M i::::i      t:::::t          o::::o     o::::o    z::::::z     o::::o     o::::o'),
        nl,
        write('M::::::M     MMMMM     M::::::M i::::i      t:::::t    tttttto::::o     o::::o   z::::::z      o::::o     o::::o'),
        nl,
        write('M::::::M               M::::::Mi::::::i     t::::::tttt:::::to:::::ooooo:::::o  z::::::zzzzzzzzo:::::ooooo:::::o'),
        nl,
        write('M::::::M               M::::::Mi::::::i     tt::::::::::::::to:::::::::::::::o z::::::::::::::zo:::::::::::::::o'),
        nl,
        write('M::::::M               M::::::Mi::::::i       tt:::::::::::tt oo:::::::::::oo z:::::::::::::::z oo:::::::::::oo'),
        nl,
        write('MMMMMMMM               MMMMMMMMiiiiiiii         ttttttttttt     ooooooooooo   zzzzzzzzzzzzzzzzz   ooooooooooo'),
        nl,
        nl,
        nl.

regras :- write('Rules:'),
          nl,
          write('Mitozo is an turn-based abstract strategy game with an oriental flavour, designed by Pablo Soto Cid.'),
          nl,
          write('It was designed as a pen and paper game for two players, but can be played with a regular square board and X/O pieces.'),
          nl,
          write('Players take turns to fill every position on the game grid, with only one rule: if the piece to be placed is going to contact the opponent, it also must contact a previous piece from the player.'),
          nl,
          write('To place a piece you have to write the position with X/Y.'),
          nl,
          write('The goal of the game is to make impossible for the rival to place any more pieces before the board is completely filled.'),
          nl,
          nl.

valid_size(S) :- S > 0.

get_size(S) :- write('Select size of board: '),
               read(S),
               valid_size(S).
get_size(S) :- get_size(S).

valid_mode('PP').
valid_mode('PC').
valid_mode('CP').
valid_mode('CC').

get_mode(M) :- write('Select Mode (Player vs Player (PP), Computer vs Player (CP), Player vs Computer (PC), Computer vs Computer (CC)): '),
               read(M),
               valid_mode(M).
get_mode(M) :- get_mode(M).

run_game(GameState, S, 'PP') :- gameloop_pp1(GameState, S).
run_game(GameState, S, 'PC') :- gameloop_pc1(GameState, S).
run_game(GameState, S, 'CP') :- gameloop_cp1(GameState, S).
run_game(GameState, S, 'CC') :- gameloop_cc1(GameState, S).

play :- logo,
        regras,
        get_size(S),
        get_mode(X),
        initial_state(S, GameState),
        run_game(GameState, S, X).





valid_moves_column_x(GameState,[], S/Y,S).
valid_moves_column_x(GameState,ListOfMoves, X/Y,S) :- \+ valid_player_x(GameState,X/Y),
                                                  X1 is X + 1,
                                                  valid_moves_column_x(GameState,ListOfMoves,X1/Y,S).
valid_moves_column_x(GameState,ListOfMoves, X/Y,S) :- valid_player_x(GameState,X/Y),
                                                  X1 is X + 1,
                                                  valid_moves_column_x(GameState,ListOfMoves2,X1/Y,S),
                                                  ListOfMoves = [X/Y|ListOfMoves2].
valid_moves_helper_x(GameState,[],S,S).
valid_moves_helper_x(GameState,ListOfMoves,Y,S) :-    Y < S,
                                                    Y1 is Y+1,
                                                    valid_moves_column_x(GameState,ListOfMoves3,0/Y,S),
                                                    valid_moves_helper_x(GameState,ListOfMoves2,Y1,S),
                                                    append(ListOfMoves3,ListOfMoves2,ListOfMoves).



valid_moves_x(GameState,ListOfMoves) :- valid_moves_helper_x(GameState,ListOfMoves,0,5).

 
 


valid_moves_column_o(GameState,[], S/Y,S).
valid_moves_column_o(GameState,ListOfMoves, X/Y,S) :- \+ valid_player_o(GameState,X/Y),
                                                    X1 is X + 1,
                                                    valid_moves_column_o(GameState,ListOfMoves,X1/Y,S).
valid_moves_column_o(GameState,ListOfMoves, X/Y,S) :- valid_player_o(GameState,X/Y),
                                                  X1 is X + 1,
                                                  valid_moves_column_o(GameState,ListOfMoves2,X1/Y,S),
                                                  ListOfMoves = [X/Y|ListOfMoves2].
valid_moves_helper_o(GameState,[],S,S).
valid_moves_helper_o(GameState,ListOfMoves,Y,S) :-    Y < S,
                                                    Y1 is Y+1,
                                                    valid_moves_column_o(GameState,ListOfMoves3,0/Y,S),
                                                    valid_moves_helper_o(GameState,ListOfMoves2,Y1,S),
                                                    append(ListOfMoves3,ListOfMoves2,ListOfMoves).



valid_moves_o(GameState,ListOfMoves) :- valid_moves_helper_o(GameState,ListOfMoves,0,5).



