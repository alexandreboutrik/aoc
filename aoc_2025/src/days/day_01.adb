with Ada.Text_IO; use Ada.Text_IO;

package body Day_01
	with SPARK_Mode => On
is
	function Increment (Value : DP; Amount : Shift_Amount) return DP is
	begin
		return (Value + Amount) mod 100;
	end Increment;

	function Decrement (Value : DP; Amount : Shift_Amount) return DP is
	begin
		return (Value - Amount) mod 100;
	end Decrement;

	function Solve
		(List  : Instruction_Array;
		 Count : Loaded_Count) return Loaded_Count
	is
		Dial     : DP := 50;
		Solution : Loaded_Count := 0;
	begin
		for I in 1 .. Count loop
			pragma Loop_Invariant (Solution >= 0);
			pragma Loop_Invariant (Solution <= I - 1);
			pragma Loop_Invariant (Solution <= Count);

			if List(I).Dir = Right then
				Dial := Increment(Dial, List(I).Value);
			else
				Dial := Decrement(Dial, List(I).Value);
			end if;

			if Dial = 0 then
				Solution := Solution + 1;
			end if;
		end loop;

		return Solution;
	end Solve;

	procedure Run is
		-- For now, because parsing is always hard to prove formally.
		pragma SPARK_Mode (Off);

		Input_File : File_Type;
		Line       : String (1 .. 20);
		Last       : Natural;

		Instructions : Instruction_Array :=
			[others => (Dir => Right, Value => 0)];

		Count  : Loaded_Count := 0;
		Result : Loaded_Count;

		-- Temporary variable to gold the parsed number before checking range
		Tmp : Integer;
	begin
		Open(Input_File, In_File, "input/day_01.txt");

		while not End_Of_File(Input_File) loop
			Get_Line(Input_File, Line, Last);

			if Last > 1 and then Count < Max_Instructions then
				Tmp := Integer'Value(Line(2 .. Last));

				if Tmp in Shift_Amount then
					Count := Count + 1;
					Instructions(Count).Value := Tmp;

					if Line(1) = 'L' then
						Instructions(Count).Dir := Left;
					else
						Instructions(Count).Dir := Right;
					end if;

				else
					Put_Line("Warning: Value out of range ->" & Tmp'Image);
				end if; -- if in Shift_Amount
			end if; -- if Last, Count
		end loop; -- while

		Close(Input_File);

		Result := Solve(Instructions, Count);

		Put_Line("Solution:" & Result'Image);
	end Run;

end Day_01;
