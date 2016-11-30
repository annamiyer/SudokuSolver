(* Name: Annam Iyer and Caroline Memishian *)
(* Sudoku Solver *)
(*I pledge to pursue all academic endeavors with honor and integrity. I understand the principles of the Honor System, and I promise to uphold these standards by adhering to the Honor Code in order to preserve the integrity of Vanderbilt University and its individual members.*)

	     
						     

(*returns string representation of row*)
fun row_to_string (row, output) =
  case row of
      [] => ""
    | h::t => if output = 3 orelse output = 6
	      then Int.toString(h) ^ " | " ^ row_to_string (t, output + 1)
	      else Int.toString(h) ^ " " ^ row_to_string (t, output + 1);
							 

(*returns string representation of board*)
fun board_to_string (board, row) =
  case board of
      [] => ""
    | h::t  => if row = 3 orelse row = 6
	       then row_to_string (h, 1) ^ "\n--------------------\n" ^ board_to_string(t, row + 1)
	       else row_to_string (h, 1) ^ "\n" ^ board_to_string(t, row + 1);
								  
		    
(*prints full board*)
fun printBoard (board) =
  print(board_to_string(board, 1) ^ "\n");


(*
val solved = [[5, 3, 4, 6, 0, 8, 9, 1, 2], [6, 7, 2, 1, 9, 5, 3, 4, 8], [1, 9, 8, 3, 4, 2, 5, 6, 7],[8, 5, 9, 7, 6, 1, 4, 2, 3], [4, 2, 6, 8, 5, 3, 7, 9, 1],[7, 1, 3, 9, 2, 4, 8, 5, 6], [9, 6, 1, 5, 3, 7, 2, 8, 4], [2, 8, 7, 4, 1, 9, 6, 3, 5], [3, 4, 5, 2, 8, 6, 1, 7, 9]];*)

val tester =
    [[5, 3, 0, 0, 7, 0, 0, 0, 0],[6, 0, 0, 1, 9, 5, 0, 0, 0],[0, 9, 8, 0, 0, 0, 0, 6, 0],[8, 0, 0, 0, 6, 0, 0, 0, 3],[4, 0, 0, 8, 0, 3, 0, 0, 1],[7, 0, 0, 0, 2, 0, 0, 0, 6],[0, 6, 0, 0, 0, 0, 2, 8, 0],[0, 0, 0, 4, 1, 9, 0, 0, 5],[0, 0, 0, 0, 8, 0, 0, 7, 9]];

(*get value*)
fun getVal (board,row, column) = List.nth (List.nth (board, row), column);

(*
getVal (tester, 1, 1);
getVal(tester, 0, 2);
getVal(tester, 0, 4);
getVal(tester, 2, 1);*)

(*setting a value of a row*)
fun setRow (row, col, num) =
  case row of
      [] => []
    | h::t => if col = 0
	      then num :: t
	      else h :: setRow (t, col - 1, num);

		       
(*sets value of box*)
fun setVal (board, row, col, num) =
  case board of
      [] => []
    | h::t  => if row = 0
	       then setRow (h, col, num) :: t
	       else h :: setVal (t, (row - 1), col, num);

(*
setVal (tester, 3, 1, 10);*)
 



 
(*helper to check the row to see if the number is legal*)
fun rowCheckHelper (row, num) =
  case row of
      [] => false
    | h::t => if h  = num
	      then true
	      else rowCheckHelper (t, num);

(*check the row to see if the number is legal*)
fun rowCheck (board, index, num) =
  rowCheckHelper (List.nth (board, index), num);
		       

(*
rowCheck(tester, 1, 9);
rowCheck(tester, 1, 8);*)


(*helper to check if the number is legal in the column*)
fun colHelper (board, row, col, num) =
  if row > 8
  then false
  else
      if getVal(board, row, col) = num
      then true
      else colHelper (board, (row + 1), col, num);

(*check the col to see if the number is legal*)
fun columnCheck (board, col, num) =
  colHelper (board, 0, col, num);			    


(*
columnCheck(tester, 3, 1);
columnCheck(tester, 3, 7);*)


(*helper to check if a number is legal in 3x3 box*)
fun squareHelper (board, row, col, finalRow, finalCol, num) =
    if col >= finalCol
    then squareHelper(board, (row + 1), (finalCol - 3), finalRow, finalCol, num)
    else if row >= finalRow
    then false
    else if getVal (board, row, col) = num
    then true
    else squareHelper (board, row, (col + 1), finalRow, finalCol, num);

(*check if num is legal in 3x3 box*)
fun squareCheck (board, row, col, num) =	    
  squareHelper (board, (row - (row mod 3)), (col - (col mod 3)), (3 + (row - (row mod 3))), (3 + (col - (col mod 3))), num);


(*
squareCheck(tester, 1, 1, 5);
squareCheck(tester, 1, 1, 2);*)
	    



(*call all the checking functions and determine if the number is legal*)
fun overallCheck (board, row, column, num) =
 if rowCheck (board, row, num) orelse columnCheck (board, column, num) orelse squareCheck (board, row, column, num)
     then true
 else false;
	       
(*our driver to solve the sudoku*)
fun solveHelper (board, row, col, num) =
  if row > 8
  then SOME (board)
  else if col > 8
  then solveHelper (board, row + 1, 0, 1)
  else if num > 9
  then NONE 	   
  else if getVal (board, row, col) = 0
  then if overallCheck(board, row, col, num)
       then solveHelper (board, row, col, num+1)
       else let val newBoard =  solveHelper ( setVal(board, row, col, num), row, col+1, 1)
	    in if isSome(newBoard)
	       then newBoard
	       else solveHelper (board, row, col, num+1) 
	    end
  else solveHelper (board, row, col +1, 1);
      
		
		

(*prints out the full, final board*)
fun sudokuSolver () =
  solveHelper (solved, 0, 0, 1);

fun solveAndPrint(board) =
  let val solvedBoard = solveHelper(board, 0, 0, 1)
  in
      if isSome(solvedBoard)
      then
	  printBoard(getOpt(solvedBoard, board))
      else
	  print("No solution")
  end;

printBoard(tester);
solveAndPrint(tester);

(*	      
  let val board' = tester
  in if board'
     then (print ""; print "Here is the initial board:"; printBoard (board); print "")
	   let val solution =  solveDriver (board);
	   in if solution = null
	      then (print "No Solution")
	      else (print ""; print "Here is the solution:"; printBoard (solution); print "")
	   end
     else (print "There is no board to process")
  end;

*)
