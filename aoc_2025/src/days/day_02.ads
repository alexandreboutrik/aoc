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

	function Is_Invalid (Value : ID) return Boolean
		with Global => null;

	function Solve
		(Input_Data : Data_Array;
		 Count      : Data_Index) return ID
	with
		Global => null,
		Pre => Count <= Max_Data;

	procedure Run;

end Day_02;
