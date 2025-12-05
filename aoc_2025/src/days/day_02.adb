with Ada.Text_IO; use Ada.Text_IO;

package body Day_02
	with SPARK_Mode => On
is
	function Get_Number_Length (Value : ID) return Digits_Length
	is
		Length : Digits_Length := 0;
		Tmp    : ID            := Value;
	begin
		loop
			pragma Loop_Invariant (Length >= 0);
			pragma Loop_Variant   (Decreases => Tmp);

			Tmp    := Tmp / 10;
			Length := Length + 1;

			exit when Tmp = 0;
		end loop;

		return Length;
	end Get_Number_Length;

	function PT1_Is_Invalid (Value : ID) return Boolean
	is
		Length   : Digits_Length;
		Tmp      : ID            := Value;
		Divisor  : ID;
		Half_Len : Integer;
	begin
		Length := Get_Number_Length(Value);

		if Length mod 2 /= 0 then
			return False;
		end if;

		Half_Len := Integer(Length) / 2;
		pragma Assert (Half_Len in 0 .. Limit/2);

		Divisor := Powers(Half_Len);

		return (Value / Divisor) = (Value mod Divisor);
	end PT1_Is_Invalid;

	function PT2_Is_Invalid (Value : ID) return Boolean
	is
		Length_DL     : Digits_Length;
		Length        : Natural;
		Chunk_Size    : Natural;
		Num_Chunks    : Natural;
		Pattern       : ID;
		Multiplier    : ID;
	begin
		Length_DL := Get_Number_Length(Value);
		Length := Natural(Length_DL);

		for K in 1 .. Length / 2 loop
			Chunk_Size := K;

			pragma Loop_Invariant (Chunk_Size >= 1);
			pragma Loop_Invariant (Chunk_Size <= Length / 2);

			if Length mod Chunk_Size = 0 then
				Num_Chunks := Length / Chunk_Size;

				if (Length - Chunk_Size) < Limit then
					Pattern := Value / Powers(Length - Chunk_Size);

					Multiplier := 0;
					for I in 0 .. Num_Chunks - 1 loop
						pragma Loop_Invariant (Num_Chunks * Chunk_Size = Length);
						pragma Loop_Invariant (I < Num_Chunks);
						pragma Loop_Invariant (I * Chunk_Size <= Powers'Last);

						Multiplier := Multiplier + Powers(I * Chunk_Size);
					end loop;

					if Value = (Pattern * Multiplier) then
						return True;
					end if;
				end if;
			end if;
		end loop;

		return False;
	end PT2_Is_Invalid;

	procedure Solve
		(Input_Data : Data_Array;
		 Count      : Data_Index)
	is
		PT1_Sum : ID := 0;
		PT2_Sum : ID := 0;
	begin
		for I in 1 .. Count loop
			pragma Loop_Invariant (I <= Count);
			pragma Loop_Invariant (Count <= Max_Data);

			declare
				Rec : Data renames Input_Data(Data_Index(I));
			begin
				for Candidate in Rec.Lower_Bound .. Rec.Upper_Bound loop
					if PT1_Is_Invalid(Candidate) then
						PT1_Sum := PT1_Sum + Candidate;
					end if;

					if PT2_Is_Invalid(Candidate) then
						PT2_Sum := PT2_Sum + Candidate;
					end if;
				end loop;
			end; -- delcare
		end loop; -- for Count

		Put_Line("Part One: " & PT1_Sum'Image);
		Put_Line("Part Two: " & PT2_Sum'Image);
	end Solve;

	procedure Run (File : in out File_Type)
		with SPARK_Mode => Off
	is
		package ID_IO is new Ada.Text_IO.Modular_IO(ID);

		Input_Data : Data_Array := 
			[others => (Lower_Bound => 0, Upper_Bound => 0)];

		Lower : ID;
		Upper : ID;
		Count : Natural := 0;

		Dummy : Character; -- to consumme ',' and '-'
	begin
		while not End_Of_File(File) loop
			if Count < Max_Data then
				ID_IO.Get(File, Lower);
				Ada.Text_IO.Get(File, Dummy);		-- '-'
				ID_IO.Get(File, Upper);

				Count := Count + 1;
				Input_Data(Data_Index(Count)).Lower_Bound := Lower;
				Input_Data(Data_Index(Count)).Upper_Bound := Upper;

				if not End_Of_File(File) then
					Ada.Text_IO.Get(File, Dummy);	-- ','
				end if;
			end if; -- if Count
		end loop; -- while

		Solve(Input_Data, Data_Index(Count));
	end Run;

end Day_02;
