with Ada.Text_IO;

package Day_01
	with SPARK_Mode => On
is
	-- Dial Points
	subtype DP is Integer range 0 .. 99;

	-- Range limit
	Limit : constant := 8192;

	-- Range of the number (N) in "R|L(N)"
	-- It limits the maximum possible solution (password)
	subtype Shift_Amount is Integer range 0 .. Limit;

	-- L(N) or R(N)
	type Direction is (Left, Right);

	type Instruction is record
		Dir		: Direction;
		Value	: Shift_Amount;
	end record;

	-- Maximum amount of lines for the Input file
	Max_Instructions : constant := Limit;

	subtype Instruction_Index is Integer range 1 .. Max_Instructions;
	subtype Loaded_Count      is Integer range 0 .. Max_Instructions;

	type Instruction_Array is array (Instruction_Index) of Instruction;

	function Increment (Value : DP; Amount : Shift_Amount) return DP
		with
			Global => null,
			Post   => Increment'Result = (Value + Amount) mod 100;
	function Decrement (Value : DP; Amount : Shift_Amount) return DP
		with
			Global => null,
			Post   => Decrement'Result = (Value - Amount) mod 100;

	function Solve
		(List  : Instruction_Array;
		 Count : Loaded_Count) return Loaded_Count
		with
			Global => null,
			-- We cannot find more '0's than the number of instructions we ran
			Post   => Solve'Result <= Count;

	procedure Run
		with
			-- We modify the file system
			Global => (In_Out => Ada.Text_IO.File_System);

	-- Run may crash (e.g. File Not Found)
	pragma Annotate (GNATprove, Might_Not_Return, Run);
end Day_01;
