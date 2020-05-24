/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <TO BE FILLED IN BEFORE THE GRADING> 
%    Student user id  : <TO BE FILLED IN BEFORE THE GRADING> 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').
% :- ensure_loaded('stupid.pl').
:- ensure_loaded('rndBoard.pl').
:- ensure_loaded("tests.pl").


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 
initialize(Board, 1) :- initBoard(Board).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 
winner(State, 1) :- 
	terminal(State), 
	score(State, 1, P1Score),
	score(State, 2, P2Score),
	P1Score > P2Score.
winner(State, 2) :- 
	terminal(State), 
	score(State, 1, P1Score),
	score(State, 2, P2Score),
	P2Score > P1Score.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 
tie(State) :- 
	terminal(State), 
	score(State, 1, P1Score), 
	score(State, 2, P2Score), 
	P1Score =:= P2Score.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   
terminal(State) :-
	moves(1, State, Moves1),
	moves(2, State, Moves2),
	Moves1 == [n],
	Moves2 == [n].





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%
moves(Plyr, State, [n]) :- 
	findall([X, Y], moveAllowed(Plyr, State, [X, Y]), MvList), 
	length(MvList, L), L =:= 0.
moves(Plyr, State, MvList) :- 
	setof([X, Y], moveAllowed(Plyr, State, [X, Y]), M), i_sort(M, MvList).

% Insertion procedure for insertion sort. 
i([X,Y], [], [[X,Y]]) :- !.
i(
	[X1,Y1],
	[[X2, Y2]|T],
	[[X1, Y1],[X2, Y2]|T]
	) :-  X1 < X2, !.
i(
	[X, Y1],
	[[X, Y2] | T],
	[[X, Y1] , [X, Y2] | T]) :- Y1 < Y2, !.
i([X1, Y1], [[X2, Y2] | T], [[X2, Y2]|T1]) :- i([X1, Y1], T, T1).

% Insertion sort
i_sort([], []).
i_sort([M|T], T2) :- i_sort(T, T1), i(M, T1, T2).

% squareEmpty(State, [X,y]): Check if a square is empty (contains '.')
squareEmpty(State, [X,Y]) :- get(State, [X, Y], S), S == '.'.

% moveAllowed(Plyr, State, [X,Y]): Check if a move is legal
moveAllowed(Plyr, State, [X,Y]) :- 
	squareEmpty(State, [X, Y]), checkLeft(Plyr, State, [X, Y]).
moveAllowed(Plyr, State, [X,Y]) :- 
	squareEmpty(State, [X, Y]), checkRight(Plyr, State, [X, Y]).
moveAllowed(Plyr, State, [X,Y]) :- 
	squareEmpty(State, [X, Y]), checkUp(Plyr, State, [X, Y]).
moveAllowed(Plyr, State, [X,Y]) :- 
	squareEmpty(State, [X, Y]), checkDown(Plyr, State, [X, Y]).
moveAllowed(Plyr, State, [X,Y]) :- 
	squareEmpty(State, [X, Y]), checkNE(Plyr, State, [X, Y]).
moveAllowed(Plyr, State, [X,Y]) :- 
	squareEmpty(State, [X, Y]), checkNW(Plyr, State, [X, Y]).
moveAllowed(Plyr, State, [X,Y]) :- 
	squareEmpty(State, [X, Y]), checkSE(Plyr, State, [X, Y]).
moveAllowed(Plyr, State, [X,Y]) :- 
	squareEmpty(State, [X, Y]), checkSW(Plyr, State, [X, Y]).

% playerInSquare(Plyr, State, [X, Y]): 
%	Check if the player is in a square
playerInSquare(Plyr,State,[X,Y]) :- 
	get(State, [X, Y], S),
	S == Plyr.

% otherPlayerInSquare(Plyr, State, [X, Y]): 
%	Check if the other player is in a square
otherPlayerInSquare(Plyr, State, [X, Y]) :-
	get(State, [X, Y], S),
	S \= Plyr, S \= '.'.

% checkLeft(Plyr, State, [X,Y])
% Check if placing a stone in [X,Y] turns any stone to the left
checkLeft(Plyr, State, [X,Y]) :- 
	X1 is X-1,  X1 >= 0,
	X2 is X-2,  X2 >= 0,
	otherPlayerInSquare(Plyr, State, [X1, Y]),
	playerInSquare(Plyr, State, [X2, Y]).
checkLeft(Plyr, State, [X,Y]) :- 
	X1 is X-1, X1 >= 0,
	otherPlayerInSquare(Plyr, State, [X1, Y]),
	checkLeft(Plyr, State, [X1, Y]).

% checkDown(Plyr, State, [X,Y])
% Check if placing a stone in [X,Y] turns any stone below
checkDown(Plyr, State, [X,Y]) :- 
	Y1 is Y+1,  Y1 < 6,
	Y2 is Y+2,  Y2 < 6,
	otherPlayerInSquare(Plyr, State, [X, Y1]),
	playerInSquare(Plyr, State, [X, Y2]).
checkDown(Plyr, State, [X,Y]) :- 
	Y1 is Y+1, Y1 < 6,
	otherPlayerInSquare(Plyr, State, [X, Y1]),
	checkDown(Plyr, State, [X, Y1]).

% checkUp(Plyr, State, [X,Y])
% Check if placing a stone in [X,Y] turns any stone above
checkUp(Plyr, State, [X,Y]) :- 
	Y1 is Y-1,  Y1 >= 0,
	Y2 is Y-2,  Y2 >= 0,
	otherPlayerInSquare(Plyr, State, [X, Y1]),
	playerInSquare(Plyr, State, [X, Y2]).
checkUp(Plyr, State, [X,Y]) :- 
	Y1 is Y-1, Y1 >= 0,
	otherPlayerInSquare(Plyr, State, [X, Y1]),
	checkUp(Plyr, State, [X, Y1]).

% checkRight(Plyr, State, [X,Y])
% Check if placing a stone in [X,Y] turns any stone to the right
checkRight(Plyr, State, [X,Y]) :- 
	X1 is X+1,  X1 < 6,
	X2 is X+2,  X2 < 6,
	otherPlayerInSquare(Plyr, State, [X1, Y]),
	playerInSquare(Plyr, State, [X2, Y]).
checkRight(Plyr, State, [X,Y]) :- 
	X1 is X+1, X1 < 6,
	otherPlayerInSquare(Plyr, State, [X1, Y]),
	checkRight(Plyr, State, [X1, Y]).

% checkNE(Plyr, State, [X,Y])
% Check if placing a stone in [X,Y] turns any stone in the north eastern 
% direction
checkNE(Plyr, State, [X,Y]) :- 
	X1 is X+1,  X1 < 6,
	X2 is X+2,  X2 < 6,
	Y1 is Y-1,  Y1 >= 0,
	Y2 is Y-2,  Y2 >= 0,
	otherPlayerInSquare(Plyr, State, [X1, Y1]),
	playerInSquare(Plyr, State, [X2, Y2]).
checkNE(Plyr, State, [X,Y]) :- 
	X1 is X+1, X1 < 6,
	Y1 is Y-1, Y1 >= 0,
	otherPlayerInSquare(Plyr, State, [X1, Y1]),
	checkNE(Plyr, State, [X1, Y1]).

% checkSE(Plyr, State, [X,Y])
% Check if placing a stone in [X,Y] turns any stone in the south eastern 
% direction
checkSE(Plyr, State, [X,Y]) :- 
	X1 is X+1,  X1 < 6,
	X2 is X+2,  X2 < 6,
	Y1 is Y+1,  Y1 < 6,
	Y2 is Y+2,  Y2 < 6,
	otherPlayerInSquare(Plyr, State, [X1, Y1]),
	playerInSquare(Plyr, State, [X2, Y2]).
checkSE(Plyr, State, [X,Y]) :- 
	X1 is X+1, X1 < 6,
	Y1 is Y+1, Y1 < 6,
	otherPlayerInSquare(Plyr, State, [X1, Y1]),
	checkSE(Plyr, State, [X1, Y1]).

% checkSW(Plyr, State, [X,Y])
% Check if placing a stone in [X,Y] turns any stone in the south western 
% direction
checkSW(Plyr, State, [X,Y]) :- 
	X1 is X-1,  X1 >= 0,
	X2 is X-2,  X2 >= 0,
	Y1 is Y+1,  Y1 < 6,
	Y2 is Y+2,  Y2 < 6,
	otherPlayerInSquare(Plyr, State, [X1, Y1]),
	playerInSquare(Plyr, State, [X2, Y2]).
checkSW(Plyr, State, [X,Y]) :- 
	X1 is X-1, X1 >= 0,
	Y1 is Y+1, Y1 < 6,
	otherPlayerInSquare(Plyr, State, [X1, Y1]),
	checkSW(Plyr, State, [X1, Y1]).

% checkNW(Plyr, State, [X,Y])
% Check if placing a stone in [X,Y] turns any stone in the north western 
% direction
checkNW(Plyr, State, [X,Y]) :- 
	X1 is X-1,  X1 >= 0,
	X2 is X-2,  X2 >= 0,
	Y1 is Y-1,  Y1 >= 0,
	Y2 is Y-2,  Y2 >= 0,
	otherPlayerInSquare(Plyr, State, [X1, Y1]),
	playerInSquare(Plyr, State, [X2, Y2]).
checkNW(Plyr, State, [X,Y]) :- 
	X1 is X-1, X1 >= 0,
	Y1 is Y-1, Y1 >= 0,
	otherPlayerInSquare(Plyr, State, [X1, Y1]),
	checkNW(Plyr, State, [X1, Y1]).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
nextState(1, Move, State, NewState, 2) :- placeStone(1, State, Move, NewState).
nextState(2, Move, State, NewState, 1) :- placeStone(2, State, Move, NewState).
nextState(1, n, State, State, 2).
nextState(2, n, State, State, 1).

turnStonesLeft(Plyr, State, [X,Y], NewState) :-
	checkLeft(Plyr, State, [X,Y]), doTurnLeft(Plyr, State, [X,Y], NewState).
turnStonesLeft(_, State, _, State).

doTurnLeft(Plyr, State, [X,Y], State) :-
	playerInSquare(Plyr, State, [X,Y]).
doTurnLeft(Plyr, State, [X,Y], NewState) :-
	X1 is X - 1,
	set(State, S1, [X,Y], Plyr), doTurnLeft(Plyr, S1, [X1, Y], NewState).

turnStonesRight(Plyr, State, [X,Y], NewState) :-
	checkRight(Plyr, State, [X,Y]), doTurnRight(Plyr, State, [X,Y], NewState).
turnStonesRight(_, State, _, State).

doTurnRight(Plyr, State, [X,Y], State) :-
	playerInSquare(Plyr, State, [X,Y]).
doTurnRight(Plyr, State, [X,Y], NewState) :-
	X1 is X + 1,
	set(State, S1, [X,Y], Plyr), doTurnRight(Plyr, S1, [X1, Y], NewState).

turnStonesUp(Plyr, State, [X,Y], NewState) :-
	checkUp(Plyr, State, [X,Y]), doTurnUp(Plyr, State, [X,Y], NewState).
turnStonesUp(_, State, _, State).

doTurnUp(Plyr, State, [X,Y], State) :-
	playerInSquare(Plyr, State, [X,Y]).
doTurnUp(Plyr, State, [X,Y], NewState) :-
	Y1 is Y - 1,
	set(State, S1, [X,Y], Plyr), doTurnUp(Plyr, S1, [X, Y1], NewState).

turnStonesDown(Plyr, State, [X,Y], NewState) :-
	checkDown(Plyr, State, [X,Y]), doTurnDown(Plyr, State, [X,Y], NewState).
turnStonesDown(_, State, _, State).

doTurnDown(Plyr, State, [X,Y], State) :-
	playerInSquare(Plyr, State, [X,Y]).
doTurnDown(Plyr, State, [X,Y], NewState) :-
	Y1 is Y + 1,
	set(State, S1, [X,Y], Plyr), doTurnDown(Plyr, S1, [X, Y1], NewState).

turnStonesNE(Plyr, State, [X,Y], NewState) :-
	checkNW(Plyr, State, [X,Y]), doTurnNE(Plyr, State, [X,Y], NewState).
turnStonesNE(_, State, _, State).

doTurnNE(Plyr, State, [X,Y], State) :-
	playerInSquare(Plyr, State, [X,Y]).
doTurnNE(Plyr, State, [X,Y], NewState) :-
	X1 is X + 1, Y1 is Y - 1,
	set(State, S1, [X,Y], Plyr), doTurnNE(Plyr, S1, [X1, Y1], NewState).

turnStonesSE(Plyr, State, [X,Y], NewState) :-
	checkSE(Plyr, State, [X,Y]), doTurnSE(Plyr, State, [X,Y], NewState).
turnStonesSE(_, State, _, State).

doTurnSE(Plyr, State, [X,Y], State) :-
	playerInSquare(Plyr, State, [X,Y]).
doTurnSE(Plyr, State, [X,Y], NewState) :-
	X1 is X + 1, Y1 is Y + 1,
	set(State, S1, [X,Y], Plyr), doTurnSE(Plyr, S1, [X1, Y1], NewState).

turnStonesSW(Plyr, State, [X,Y], NewState) :-
	checkSW(Plyr, State, [X,Y]), doTurnSW(Plyr, State, [X,Y], NewState).
turnStonesSW(_, State, _, State).

doTurnSW(Plyr, State, [X,Y], State) :-
	playerInSquare(Plyr, State, [X,Y]).
doTurnSW(Plyr, State, [X,Y], NewState) :-
	X1 is X - 1, Y1 is Y + 1,
	set(State, S1, [X,Y], Plyr), doTurnSW(Plyr, S1, [X1, Y1], NewState).

turnStonesNW(Plyr, State, [X,Y], NewState) :-
	checkNW(Plyr, State, [X,Y]), doTurnNW(Plyr, State, [X,Y], NewState).
turnStonesNW(_, State, _, State).

doTurnNW(Plyr, State, [X,Y], State) :-
	playerInSquare(Plyr, State, [X,Y]).
doTurnNW(Plyr, State, [X,Y], NewState) :-
	X1 is X - 1, Y1 is Y - 1,
	set(State, S1, [X,Y], Plyr), doTurnNW(Plyr, S1, [X1, Y1], NewState).

turnStonesDiagonally(Plyr, State, [X,Y], NewState) :-
	turnStonesNE(Plyr, State, [X,Y], S1),
	turnStonesSE(Plyr, S1, [X,Y], S2),
	turnStonesSW(Plyr, S2, [X,Y], S3),
	turnStonesNW(Plyr, S3, [X,Y], NewState).

turnStonesHorizontal(Plyr, State, [X,Y], NewState) :- 
	turnStonesRight(Plyr, State, [X,Y], S1),
	turnStonesLeft(Plyr, S1, [X,Y], NewState).

turnStonesVertical(Plyr, State, [X,Y], NewState) :- 
	turnStonesDown(Plyr, State, [X,Y], S1),
	turnStonesUp(Plyr, S1, [X,Y], NewState).

placeStone(Plyr, State, [X,Y], NewState) :- 
	validmove(Plyr, State, [X,Y]),
	turnStonesHorizontal(Plyr, State, [X,Y], S1),
	turnStonesVertical(Plyr, S1, [X,Y], S2),
	turnStonesDiagonally(Plyr, S2, [X,Y], NewState).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.
validmove(Plyr, State, Proposed) :-
	moves(Plyr, State, MvList), 
	member(Proposed, MvList).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.
h(State, Val) :- score(State, 1, S1), score(State, 2, S2), Val is S1 - S2.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.
lowerBound(-1).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.
upperBound(37).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
%
% score(State, Plyr, Score): Calculates a player's score given the current 
%		state.
%
score([], _, 0).
score([Row|Rows], Plyr, Score) :-
	scoreRow(Row, Plyr, S1),
	score(Rows, Plyr, S2),
	Score is S1+S2.

%
% scoreRow(State, Plyr, Score): Calculates a player's score of a row given the
%		current state.
scoreRow([], _, 0).
scoreRow([.|Rest], 1, Score) :- scoreRow(Rest, 1, Score).
scoreRow([2|Rest], 1, Score) :- scoreRow(Rest, 1, Score).
scoreRow([1|Rest], 1, Score) :- scoreRow(Rest, 1, S), Score is 1+S.
scoreRow([.|Rest], 2, Score) :- scoreRow(Rest, 2, Score).
scoreRow([1|Rest], 2, Score) :- scoreRow(Rest, 2, Score).
scoreRow([2|Rest], 2, Score) :- scoreRow(Rest, 2, S), Score is 1+S.
