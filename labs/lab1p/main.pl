% State is state(
%		robot(pos, [Items]), 
%		room1([Items]), 
%		room2([Items]), 
%		room3([Items])	
%	)

% types of move:
% 1. move between rooms
% 2. pick up items
% 3. drop items

% Initial state of the problem
% state(robot(1, []), [s], [b], [p])



move(
	state(robot(1, Items), R1, R2, R3),
	move(2),
	state(robot(2, Items), R1, R2, R3)
) :- member(s, Items).

move(
	state(robot(2, Items), R1, R2, R3),
	move(1),
	state(robot(1, Items), R1, R2, R3)
) :- member(s, Items).

move(
	state(robot(1, Items), R1, R2, R3),
	move(3),
	state(robot(3, Items), R1, R2, R3)
) :- member(b, Items).

move(
	state(robot(3, Items), R1, R2, R3),
	move(1),
	state(robot(1, Items), R1, R2, R3)
) :- member(b, Items).

move(
	state(robot(1, Items), R1_old, R2, R3),
	pickup(Item),
	state(robot(1, [Item|Items]), R1_new, R2, R3)
) :- length(Items, L), L < 2, member(Item, R1_old), delete(R1_old, Item, R1_new).

move(
	state(robot(2, Items), R1, R2_old, R3),
	pickup(Item),
	state(robot(2, [Item|Items]), R1, R2_new, R3)
) :- length(Items, L), L < 2, member(Item, R2_old), delete(R2_old, Item, R2_new).

move(
	state(robot(3, Items), R1, R2, R3_old),
	pickup(Item),
	state(robot(3, [Item|Items]), R1, R2, R3_new)
) :- length(Items, L), L < 2, member(Item, R3_old), delete(R3_old, Item, R3_new).

move(
	state(robot(1, I1), R1, R2, R3),
	drop(Item),
	state(robot(1, I2), [Item|R1], R2, R3)
) :- member(Item, I1), delete(I1, Item, I2).
move(
	state(robot(2, I1), R1, R2, R3),
	drop(Item),
	state(robot(2, I2), R1, [Item|R2], R3)
) :- member(Item, I1), delete(I1, Item, I2).
move(
	state(robot(3, I1), R1, R2, R3),
	drop(Item),
	state(robot(3, I2), R1, R2, [Item|R3])
) :- member(Item, I1), delete(I1, Item, I2).

solveR(state(_, _, R2, _), N, [done | []]) :- member(p, R2).
solveR(State1, N, [Move|T]) :- N>0, move(State1, Move, State2), solveR(State2, N-1, T).
