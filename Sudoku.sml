(* Name: Annam Iyer and Caroline Memishian *)
(* Sudoku Solver *)

exception invalid of int;

fun checkValid n = if n >= 0 andalso n < 9 then n else raise invalid n;

fun getVal (board, col, row) = List.nth (List.nth (board, row), column);

fun getRow board row  = List.nth (board, checkValid row);

fun getCol board col  = List.nth (board, checkValid column);

fun solve board
			      
