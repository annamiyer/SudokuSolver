
(* Name: Annam Iyer and Caroline Memishian *)
(* Sudoku Solver *)

(*read in a file, can't figure this out exactly*)

(*
val infile = "C:/sudoku.txt";
fun read infile = let val inStream = TextIO.openIn file;
in
    TextIO.input1 inStream;
		  end;

fun print-board board; 


exception invalid of int;
(* the sudoku board is a list of lists representing the board *)

fun solve board = let fun solve' board row illegalNums =
    (*keep track of illegal numbers, if we are on row 9 and board legal, we are done*)
    if row = 9 andalso completedBoard board andalso checkLegal board then board else
    (* if we are on row 9 but the board is not legal, throw exception*)
    if row = 9 then raise invalid n
    let val row = (List.nth (board, row)); (* get current row to work on*)

fun attempt num row col = 
    (*if number is great than 9, throw exception and backtrack*)
    if num > 9 then raise invalid n
    (*if the spot we are checking is already filled, move to next column*)
    else if not (List.nth (row, col) = 0) then attempt num row (col+1)
    (*if the number is not allowed, skip to next number, ADD CASE HERE*)
    
    (*if the row has the number, go to the next number*)
    

    (*if the col has the number, go to the next number*)
*)

(* Racket Functions from Proj#2 below *)
						       
fun  printBoard (board)						       
						       
fun printRow (row)
						       
fun getBoard ()

val infile = "":string;
fun getBoardFromFile (infile) =
  let val inStream = TextIO.openln(infile);
      val text = TextIO.inputAll(inStream);
  in print(text)
  end;

read(infile);

						       
fun getVal (board col row) = List.nth (List.nth (board, row), column);

fun setVal (board, col, row, num) =
  let val new = num
  in setVal(num)
  end;
 
fun sudokuSolver () =
  let val board = getBoard;
  in if board
     then (print ""; print "Here is the initial board:"; printBoard (board); print "")
	   let val solution =  solveDriver (board);
	   in if solution = null
	      then (print "No Solution")
	      else (print ""; print "Here is the solution:"; printBoard (solution); print "")
     else (print "There is no board to process")
  end;
      
fun solveDriver (board) =
  let val value = findZero (board);
      let fun check (board, row, column, num) =
	    if (num > 9) or (num <= 0)
	    then null
	    else if overallCheck (board, row, column, num)
	    then let val newBoard =
		       solveDriver (setVal (board, row, colum,n num))
		 in if  newBoard = null		       
		    then check (board, row, column, (num+1))
		    else newBoard
		 end;	     
	    else check (board, row, column, (num+1))	       
      in if value = -1
	 then board
	 else check (board, (hd  value), (tl value) 1)
      end
  in value
  end;
 
      
		
fun rowCheck board row num =				 
  let fun checkHelper board row start finish num =
	let val testNum' =  fun getVal (board col row)
				       if finish < start
				       then true
				       else if val testNum = num
				       then false
				       else fun checkHelper (board row start (finish - 1) num)
	in testNum' end
  in fun checkHelper (board row 0 8 num)
  end;
   						
fun columnCheck board col num =				 
  let fun checkHelper board col start finish num =
	let val testNum' =  fun getVal (board col row)
				       if finish < start
				       then true
				       else if val testNum = num
				       then false
				       else fun checkHelper (board col  start (finish - 1) num)
	in testNum' end
  in fun checkHelper (board col 0 8 num)
  end;

fun squareCheck (board, firstrow, firstcolumn, row, column, num) =
  let fun checkHelper (board, firstrow, firstcolumn, row, column, num) =
	let val testNum' = fun getVal (board, (firstcolumn + column), (firstrow + row))
					     if val testNum = num
					     then false
					     else if row = 8 then
						 if column = 8
						 then true
						 else  checkHelper (board, firstrow, firstcolumn, 0, (column+1), num)
					     else checkHelper (board, firstrow, firstcolumn, (row+1), column, num)
	in testNum' end
  in checkHelper (firstrow, firstcolumn, 0, 0, num)
  end;

fun overallCheck (board, row, column, num) =
  let val firstInRow' = row - (row % 9);
      val firstInColumn' = column - (column % 9);
  in if rowCheck (board, row, num) and columnCheck (board, column, num) and squareCheck (board, firstInRow, firstInColumn, row, column, num)
     then true
     else false
  end;

fun findZero (board) =
  let fun check (board, row, column)
		if column = 8
		then if row = 8
		     then -1
		     else check (board, (row+1), 0)
		else check (board, row, (column +1))
  in check (board, 0, 0)
  end;			       
				  						     
end;

							     




						     
							    
							     
