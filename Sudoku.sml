(* Name: Annam Iyer and Caroline Memishian *)
(* Sudoku Solver *)
(*Honor code: We pledge to pursue all academic endeavors with honor and integrity. 
We understand the principles of the Honor System, and we promise to uphold these standards 
by adhering to the Honor Code in order to preserve the integrity of Vanderbilt University 
and its individual members. *)

						       
fun getBoard ()*)
(*
val infile = "C:/sudoku.txt":string;
fun getBoardFromFile (infile) =
  let val inStream = TextIO.openIn(infile)
      val text = TextIO.inputAll(inStream)
  in print(text)
  end;
read(infile);
*)


fun row_to_string (row) =
  case row of
      [] => ""
    | h::t => Int.toString(h) ^ " " ^ row_to_string (t);


fun board_to_string (board) =
  case board of
      [] => ""
    | h::t  => row_to_string (h) ^ "\n" ^ board_to_string(t);
 
val tester =
    [[5, 3, 0, 0, 7, 0, 0, 0, 0],[6, 0, 0, 1, 9, 5, 0, 0, 0],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9]];

fun getVal (board, column, row) = List.nth (List.nth (board, row), column);

(*getVal (tester, 1, 1);*)

fun setVal (row, index, num) =
  case row of
      [] => []
    | h::t  => if index = 0
	       then num :: t
	       else h :: setVal (t, (index - 1), num);

(*setVal ([1,2,3,0,4,5], 3, 5);*)
 
 
 
(*check the row to see if the number is legal*)
fun rowCheck (row, num) =
  case row of
      [] => false
    | h::t => if h  = num
	      then true
	      else rowCheck (t, num);
		       

(*
fun rowCheck (board, row, num) =				 
  let fun checkHelper (board, row, start, finish, num) =
	let val testNum' =   getVal (board, col, row)
				       if finish < start
				       then true
				       else if  testNum' = num
				       then  false
				       else  checkHelper (board, row, start, (finish - 1), num)
	in testNum' end
  in checkHelper (board, row, 0, 8, num)
  end;
  *)


(*check the col to see if the number is legal*)
fun columnCheck (col, num) =
  case col of
      [] => false
    | h::t => if h  = num
	      then true
	      else columnCheck (t, num);


(*
fun columnCheck (board, col, num) =				 
  let fun checkHelper (board, col, start, finish, num) =
	let val testNum' =   getVal (board col row)
				       if finish < start
				       then true
				       else if  testNum' = num
				       then false
				       else checkHelper (board, col, start, (finish - 1), num)
	in testNum' end
  in checkHelper (board, col, 0, 8, num)
  end;
  
  *)


(*attempt at square check
fun squareCheck (row, firstrow, firstcolumn, num) =
  while firstrow <=2 do (
		     while firstcolumn  <= 2 do (
					     case row of
						 [] => false
					       | h::t => if h = num
							 then true
							 else squareCheck(t, firstrow, firstcolumn+1, num)
		     )
					     
  );
*)
	    
(*check the 3x3 box to see if the number is legal*)
fun squareCheck (board, firstrow, firstcolumn, row, column, num) =
  let fun checkHelper (board, firstrow, firstcolumn, row, column, num) =
	let val testNum' =  getVal (board, (firstcolumn + column), (firstrow + row))
					     if  testNum' = num
					     then false
					     else if row = 8 then
						 if column = 8
						 then true
						 else  checkHelper (board, firstrow, firstcolumn, 0, (column+1), num)
					     else checkHelper (board, firstrow, firstcolumn, (row+1), column, num)
	in testNum' end
  in checkHelper (firstrow, firstcolumn, 0, 0, num)
  end;


(*call all the checking functions and determine if the number is legal*)
fun overallCheck (board, row, column, num) =
  let val firstInRow' = row - (row % 9);
      val firstInColumn' = column - (column % 9);
  in if rowCheck (board, row, num) and columnCheck (board, column, num) and squareCheck (board, firstInRow', firstInColumn', row, column, num)
     then true
     else false
  end;

(*finds all the zeros on the board *)
fun findZero (board) =
  let fun check (board, row, column)
		if column = 8
		then if row = 8
		     then -1
		     else check (board, (row+1), 0)
		else check (board, row, (column +1))
  in check (board, 0, 0)
  end;			       

(*calls the overall check and sets the value*)
fun solveDriver (board) =
  let val value' = findZero (board);
      let fun check (board, row, column, num) =
	    if (num > 9) or (num <= 0)
	    then null
	    else if overallCheck (board, row, column, num)
	    then let val newBoard' =
		       solveDriver (setVal (board, row, column, num))
		 in if  newBoard' = null		       
		    then check (board, row, column, (num+1))
		    else newBoard'
		 end;	     
	    else check (board, row, column, (num+1))	       
      in if value' = -1
	 then board
	 else check (board, (hd  value'), (tl value') 1)
      end
  in value
  end;

(*prints out the full, final board*)
fun sudokuSolver () =
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

