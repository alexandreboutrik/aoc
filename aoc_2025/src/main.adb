with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Day_02;
with Day_01;

procedure Main is
	Day_Number : Integer;

	function Parse_Argument return Integer is
	begin
		if Argument_Count < 1 then
			return 0;
		end if;

		return Integer'Value(Argument(1));
	exception
		when others => return -1;
	end Parse_Argument;
begin
   Day_Number := Parse_Argument;

   case Day_Number is
		when 2 =>
			Day_02.Run;

		when 1 =>
			Day_01.Run;

		when 0 =>
			Put_Line("Usage: ./bin/aoc_2025 <day_number>");
		
		when others =>
			Put_Line("Error: Day" & Day_Number'Image & "not implemented.");

	end case;
end Main;
