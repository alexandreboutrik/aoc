with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Safe_IO; use Safe_IO;

with Day_02;
with Day_01;

procedure Main is
	Day_Number : Integer;

	-- signature of Day_XY.Run
	type Day_Runner is access procedure (F : in out File_Type);

	function Parse_Argument return Integer is
	begin
		if Argument_Count < 1 then
			return 0;
		end if;

		return Integer'Value(Argument(1));
	exception
		when others => return -1;
	end Parse_Argument;

	procedure Execute_Day (Path : String; Runner : Day_Runner) is
		File   : File_Type;
		Status : File_Status;
	begin
		Try_Open(File, Path, Status);

		case Status is
			when Safe_IO.Success =>
				Runner(File);

				if Is_Open(File) then
					Close(File);
				end if;

			when others =>
				New_Line;
				Put_Line("Day " & Argument(1) & " cannot continue due to previous error.");

		end case;
	end Execute_Day;
begin
   Day_Number := Parse_Argument;

   case Day_Number is
		when 2 =>
			Execute_Day("input/day_02.txt", Day_02.Run'Access);

		when 1 =>
			Execute_Day("input/day_01.txt", Day_01.Run'Access);

		when 0 =>
			Put_Line("Usage: ./bin/aoc_2025 <day_number>");
		
		when others =>
			Put_Line("Error: Day" & Day_Number'Image & " not implemented.");

	end case;
end Main;
