:- ensure_loaded("testboards.pl").
:- ensure_loaded("othello.pl").

tests :- 
	testFlipRLtop,
	testFlipLRbottom,
	testFlipTBleft,
	testFlipBTright,
	testFlipDiagULtoLR,
	testFlipDiagURtoLL,
	testNoMovesNoFlipsA,
	testNoMovesNoFlipsB,
	testFlipLRonly1,
	testFlipAll8Dirs1,
	testFlipAll8Dirs2,
	testTieInTwoMovesFullBoard,
	testTieFourEmptyInCorners,
	testTieFourEmptyOnly1canMove,
	testTie30emptyOnly1canMove,
	testTie30emptyOnly2canMove,
	testWinInTwoMovesFullBoard,
	testOnlyTwos,
	testOnlyOnes,
	testForcing2toDoNullMove,
	testForcing1toDoNullMove.

testFlipRLtop :- 
	flipRLtop(B),
	moves(1, B, [[5,0]]),
	moves(2, B, [[0,0]]),
	nextState(1, [5,0], B, NewState, 2), nth0(0, NewState, [.,1,1,1,1,1]).

testFlipLRbottom :- 
	flipLRbottom(B),
	moves(1, B, [n]), 
	moves(2, B, [[0,5]]), 
	nextState(2, [0,5], B, NewState, 1), nth0(5, NewState, [2,2,2,2,2,2]).

testFlipTBleft :-
	flipTBleft(B),
	moves(1, B, [n]), 
	moves(2, B, [[0,0]]), 
	nextState(2, [0,0], B, NewState, 1),
	NewState == [
		[2,.,.,.,.,.],
		[2,.,.,.,.,.],
		[2,.,.,.,.,.],
		[2,.,.,.,.,.],
		[2,.,.,.,.,.],
		[2,.,.,.,.,.]
	].
	
testFlipBTright :-
	flipBTright(B),
	moves(1, B, [[5,5]]), 
	moves(2, B, [n]), 
	nextState(1, [5,5], B, NewState, 2),
	NewState == [
		[.,.,.,.,.,1],
		[.,.,.,.,.,1],
		[.,.,.,.,.,1],
		[.,.,.,.,.,1],
		[.,.,.,.,.,1],
		[.,.,.,.,.,1]
	].

testFlipDiagULtoLR :-
	flipDiagULtoLR(B),
	moves(1, B, [[0,0]]), 
	moves(2, B, [[5,5]]),
	nextState(1, [0,0], B, NewState, 2),
	NewState == [
		[1,.,.,.,.,.],
		[.,1,.,.,.,.],
		[.,.,1,.,.,.],
		[.,.,.,1,.,.],
		[.,.,.,.,1,.],
		[.,.,.,.,.,.]
	].

testFlipDiagURtoLL :-
	flipDiagURtoLL(B),
	moves(1, B, [n]), 
	moves(2, B, [[5,0]]), 
	nextState(2, [5,0], B, NewState, 1),
	NewState == [
		[.,.,.,.,.,2],
		[.,.,.,.,2,.],
		[.,.,.,2,.,.],
		[.,.,2,.,.,.],
		[.,2,.,.,.,.],
		[2,.,.,.,.,.]
	].

testNoMovesNoFlipsA :- 
	noMovesNoFlipsA(B),
	moves(1, B, [n]),
	moves(2, B, [n]),
	not(placeStone(1, B, [2,2], B)),
	not(placeStone(2, B, [2,2], B)).

testNoMovesNoFlipsB :- 
	noMovesNoFlipsB(B),
	moves(1, B, [n]),
	moves(2, B, [n]),
	not(placeStone(1, B, [2,2], B)),
	not(placeStone(2, B, [2,2], B)).

testFlipLRonly1 :-
	flipLRonly1(B),
	moves(1, B, MvList), member([3,3], MvList),
	placeStone(1, B, [3,3], NewState),
	NewState == [
		[.,.,.,2,.,.],
		[.,.,.,1,.,.],
		[.,.,.,1,.,.],
		[1,1,1,1,1,1],
		[.,.,.,1,.,.],
		[.,.,.,2,.,.]
	].

testFlipAll8Dirs1 :-
	flipAll8Dirs1(B),
	moves(1, B, [[2,2]]), 
	moves(2, B, [n]), 
	placeStone(1, B, [2,2], NewState),
	NewState == [
		[1,2,1,2,1,2],
		[2,1,1,1,2,2],
		[1,1,1,1,1,1],
		[2,1,1,1,2,2],
		[1,2,1,2,1,2],
		[2,2,1,2,2,1]
	].

testFlipAll8Dirs2 :-
	flipAll8Dirs2(B),
	moves(1, B, [n]), 
	moves(2, B, [[3,3]]), 
	placeStone(2, B, [3,3], NewState),
	NewState == [
		[2,2,2,2,2,2],
		[2,2,2,2,2,2],
		[2,2,2,2,2,2],
		[2,2,2,2,2,2],
		[2,2,2,2,2,2],
		[2,2,2,2,2,2]
	].

testTieInTwoMovesFullBoard :-
	tieInTwoMovesFullBoard(B),
	moves(1, B, [[0,0]]),
	moves(2, B, [[5,5]]),
	placeStone(1, B, [0,0], S1),
	placeStone(2, S1,[5,5], S2),
	tie(S2).

testTieFourEmptyInCorners :-
	tieFourEmptyInCorners(B),
	moves(1, B, [n]), 
	moves(2, B, [n]), 
	tie(B).

testTieFourEmptyOnBorders :-
	tieFourEmptyOnBorders(B),
	moves(1, B, [n]), 
	moves(2, B, [n]), 
	tie(B).

testTieFourEmptyOnly1canMove :-
	tieFourEmptyOnly1canMove(B),
	moves(1, B, [[0, 4]]),
	placeStone(1, B, [0,4], NewState),
	NewState == [
				[.,2,2,2,2,.],
			  [1,2,2,2,1,1],
			  [1,2,2,1,1,1],
			  [1,2,1,2,1,1],
			  [1,1,1,1,2,1],
			  [.,2,2,2,2,.]
	],
	tie(NewState).

testTie30emptyOnly1canMove :-
	tie30emptyOnly1canMove(B),
	moves(1, B, [[1,2]]),
	moves(2, B, [n]),
	placeStone(1, B, [1,2], NewState),
	score(NewState, 1, 3),
	score(NewState, 2, 3),
	tie(NewState).

testTie30emptyOnly2canMove :-
	tie30emptyOnly2canMove(B),
	moves(1, B, [n]),
	moves(2, B, [[1,2]]),
	placeStone(2, B, [1,2], NewState),
	score(NewState, 1, 3),
	score(NewState, 2, 3),
	tie(NewState).

testWinInTwoMovesFullBoard :-
	winInTwoMovesFullBoard(B),
	moves(1, B, [[0,0]]),
	placeStone(1, B, [0,0], S1),
	placeStone(2, S1, [5,5], S2),
	winner(S2, 1).

testOnlyTwos :-
	onlyTwos(B),
	winner(B, 1).

testOnlyOnes :-
	onlyOnes(B),
	winner(B, 2).

testForcing2toDoNullMove :-
	forcing2toDoNullMove(B),
	moves(1, B, [[0, 5], [5,0]]),
	nextState(1, [5,0], B, S1, 2),
	moves(2, S1, [n]),
	nextState(2, n, S1, S1, 1),
	nextState(1, [0,5], S1, S2, 2),
	winner(S2, 2).

testForcing1toDoNullMove :-
	forcing1toDoNullMoves(B),
	moves(2, B, [[0, 5], [5,0]]),
	nextState(2, [5,0], B, S1, 1),
	moves(1, S1, [n]),
	nextState(1, n, S1, S1, 2),
	nextState(2, [0,5], S1, S2, 1),
	winner(S2, 1).
