with Ada.Text_IO; use Ada.Text_IO;

package body Day_02
	with SPARK_Mode => On
is
	function Is_Invalid (Value : ID) return Boolean
	is
		Limit : constant := 20;

		type Digits_Length is mod Limit;

		Length   : Digits_Length := 0;
		Tmp      : ID            := Value;
		Divisor  : ID;
		Half_Len : Integer;

		-- Size is half of Digits_Length
		type Power_Table is array (0 .. Limit/2) of ID;
		Powers : constant Power_Table :=
			[1,
			 10,
			 100,
			 1_000,
			 10_000,
			 100_000,
			 1_000_000,
			 10_000_000,
			 100_000_000,
			 1_000_000_000,
			 10_000_000_000];
	begin
		-- Calculate the number of digits
		loop
			pragma Loop_Invariant (Length >= 0);
			pragma Loop_Variant   (Decreases => Tmp);

			Tmp    := Tmp / 10;
			Length := Length + 1;

			exit when Tmp = 0;
		end loop;

		if Length mod 2 /= 0 then
			return False;
		end if;

		Half_Len := Integer(Length) / 2;
		pragma Assert (Half_Len in 0 .. Limit/2);

		Divisor := Powers(Half_Len);

		return (Value / Divisor) = (Value mod Divisor);
	end Is_Invalid;

	function Solve
		(Input_Data : Data_Array;
		 Count      : Data_Index) return ID
	is
		Sum : ID := 0;
	begin
		for I in 1 .. Count loop
			pragma Loop_Invariant (I <= Count);
			pragma Loop_Invariant (Count <= Max_Data);

			declare
				Rec : Data renames Input_Data(Data_Index(I));
			begin
				for Candidate in Rec.Lower_Bound .. Rec.Upper_Bound loop
					if Is_Invalid(Candidate) then
						Sum := Sum + Candidate;
					end if;
				end loop;
			end; -- delcare
		end loop; -- for Count

		return Sum;
	end Solve;

	procedure Run is
		pragma SPARK_Mode (Off);

		Input_File : File_Type;

		package ID_IO is new Ada.Text_IO.Modular_IO(ID);

		Input_Data : Data_Array := 
			[others => (Lower_Bound => 0, Upper_Bound => 0)];

		Lower : ID;
		Upper : ID;
		Count : Natural := 0;

		Dummy : Character; -- to consumme ',' and '-'

		Result : ID;
	begin
		Open(Input_File, In_File, "input/day_02.txt");

		while not End_Of_File(Input_File) loop
			if Count < Max_Data then
				ID_IO.Get(Input_File, Lower);
				Ada.Text_IO.Get(Input_File, Dummy);		-- '-'
				ID_IO.Get(Input_File, Upper);

				Count := Count + 1;
				Input_Data(Data_Index(Count)).Lower_Bound := Lower;
				Input_Data(Data_Index(Count)).Upper_Bound := Upper;

				if not End_Of_File(Input_File) then
					Ada.Text_IO.Get(Input_File, Dummy);	-- ','
				end if;
			end if; -- if Count
		end loop; -- while

		Close(Input_File);

		Result := Solve(Input_Data, Data_Index(Count));

		Put_Line("Solution:" & Result'Image);
	end Run;

end Day_02;
