with Ada.Text_IO;

package Day_02
	with SPARK_Mode => On
is
	type ID is mod 2**64;

	type Data is record
		Lower_Bound : ID;
		Upper_Bound : ID;
	end record;

	Max_Data : constant := 8192;
	subtype Data_Index is Integer range 1 .. Max_Data;
	type    Data_Array is array (Data_Index) of Data;

	Limit : constant := 20;

	type Digits_Length is mod Limit;
	type Power_Table is array (0 .. Limit) of ID;

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
		 10_000_000_000,
		 100_000_000_000,
		 1_000_000_000_000,
		 10_000_000_000_000,
		 100_000_000_000_000,
		 1_000_000_000_000_000,
		 10_000_000_000_000_000,
		 100_000_000_000_000_000,
		 1_000_000_000_000_000_000,
		 others => 0];

	function Get_Number_Length (Value : ID) return Digits_Length
		with Global => null;

	function PT1_Is_Invalid (Value : ID) return Boolean
		with Global => null;

	function PT2_Is_Invalid (Value : ID) return Boolean
		with Global => null;

	procedure Solve
		(Input_Data : Data_Array;
		 Count      : Data_Index)
	with
		Global => (In_Out => Ada.Text_IO.File_System),
		Pre => Count <= Max_Data;

	procedure Run (File : in out Ada.Text_IO.File_Type)
	with
		Global => null,
		Pre    => Ada.Text_IO.Is_Open(File);

end Day_02;
