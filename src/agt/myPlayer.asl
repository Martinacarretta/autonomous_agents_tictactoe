/*

Implementation of a Tic-Tac-Toe player that just plays random moves.

When the agent is started it must first perform a 'sayHello' action.
Once all agents have done this, the game or tournament starts.

Each turn the agent will observe the following percepts:

- symbol(x) or symbol(o) 
	This indicates which symbol this agent should use to mark the cells. It will be the same in every turn.

- a number of marks:  e.g. mark(0,0,x) , mark(0,1,o) ,  mark(2,2,x)
  this indicates which cells have been marked with a 'x' or an 'o'. 
  Of course, in the first turn no cell will be marked yet, so there will be no such percept.

- round(Z)
	Indicates in which round of the game we are. 
	Since this will be changing each round, it can be used by the agent as a trigger to start the plan to determine
	its next move.

Furthermore, the agent may also observe the following:

- next 
	This means that it is this agent's turn.
  
- winner(x) or winner(o)
	If the game is over and did not end in a draw. Indicates which player won.
	
- end 
	If the game is finished.
	
- After observing 'end' the agent must perform the action 'confirmEnd'.

To mark a cell, use the 'play' action. For example if you perform the action play(1,1). 
Then the cell with coordinates (1,1) will be marked with your symbol. 
This action will fail if that cell is already marked.

*/

/* Initial beliefs and rules */


// First, define a 'cell' to be a pair of numbers, between 0 and 2. i.e. (0,0) , (0,1), (0,2) ... (2,2).

isCoordinate(0).
isCoordinate(1).
isCoordinate(2).

isCell(X,Y) :- isCoordinate(X) & isCoordinate(Y).

/* A cell is 'available' if it does not contain a mark.*/
available(X,Y) :- isCell(X,Y) & not mark(X,Y,_).

// Define a winning move: A cell (X, Y) is a winning move if it is available 
// and completes a row, column, or diagonal with two marks of the same symbol (S).
winningMove(X, Y, S) :-
    symbol(S) & 
    isCell(X, Y) & available(X, Y) & (
        twoInRow(X, Y, S) |
        twoInColumn(X, Y, S) |
        twoInDiagonal(X, Y, S)
    ).

losingMove(X, Y, S) :-
    isCell(X, Y) & available(X, Y) & (
        twoInRow(X, Y, S) |
        twoInColumn(X, Y, S) |
        twoInDiagonal(X, Y, S)
    ).

// Check if (X, Y) completes a row with two existing marks of S
twoInRow(X, Y, S) :-
    available(X, Y) &
    isCoordinate(Y1) & isCoordinate(Y2) &  // Get two other Y-coordinates
    Y1 \== Y & Y2 \== Y & Y1 \== Y2 &         // Ensure they are different
    mark(X, Y1, S) & mark(X, Y2, S).       // Check if they are marked with S

// Check if (X, Y) completes a column with two existing marks of S
twoInColumn(X, Y, S) :-
    available(X, Y) &
    isCoordinate(X1) & isCoordinate(X2) &  // Get two other X-coordinates
    X1 \== X & X2 \== X & X1 \== X2 &         // Ensure they are different
    mark(X1, Y, S) & mark(X2, Y, S).       // Check if they are marked with S

// Check if (X, Y) completes a diagonal with two existing marks of S
twoInDiagonal(X, Y, S) :-
    available(X, Y) & (
        (diagonal1(X, Y) & twoMarkedInDiagonal1(S)) |  // Main diagonal
        (diagonal2(X, Y) & twoMarkedInDiagonal2(S))    // Anti-diagonal
    ).

twoMarkedInDiagonal1(S) :-
    (mark(0, 0, S) & mark(1, 1, S)) |
    (mark(0, 0, S) & mark(2, 2, S)) |
    (mark(1, 1, S) & mark(2, 2, S)).

twoMarkedInDiagonal2(S) :-
    (mark(0, 2, S) & mark(1, 1, S)) |
    (mark(0, 2, S) & mark(2, 0, S)) |
    (mark(1, 1, S) & mark(2, 0, S)).

diagonal1(X, Y) :- X = Y.
diagonal2(X, Y) :- X + Y = 2.

started.


/* Plans */

/* When the agent is started, perform the 'sayHello' action. */
+started <- sayHello.


/* Check for winning moves */
+round(Z) : next & .findall(winningMove(A, B, S), winningMove(A, B, S), WinningMoves) & .length(WinningMoves, L) & L > 0
<- 
    .print("Winning moves__: ", WinningMoves);
    N = math.floor(math.random(L));
    .nth(N, WinningMoves, winningMove(A, B, S));
    play(A, B).

/* Check for losing moves and block the opponent */
+round(Z) : next & .findall(losingMove(A, B, S), losingMove(A, B, S), LosingMoves) & .length(LosingMoves, L) & L > 0
<- 
    .print("Losing moves xxxxxxxxxxxxxxxxxxxxxxx: ", LosingMoves);
    N = math.floor(math.random(L));
    .nth(N, LosingMoves, losingMove(A, B, S));
    play(A, B).

/* play center if free*/
+round(Z) : next & available(1, 1)
<- 
    .print("Taking center move at (1,1).");
    play(1,1).

/* If no winning or blocking move, play a random available move. */
+round(Z) : next & .findall(available(X,Y), available(X,Y), AvailableCells) & .length(AvailableCells, L) & L > 0
<- 
    .print("Available moves: ", AvailableCells);
    N = math.floor(math.random(L));
    .nth(N, AvailableCells, available(X,Y));
    play(X,Y).

/* If I am the winner, then print "I won!"  */
+winner(S) : symbol(S) <- .print("I won!").

+end <- confirmEnd.