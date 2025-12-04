package body Safe_IO
is
	procedure Try_Open 
		(File : in out File_Type;
		 Name : String;
		 Status : out File_Status)
	is
	begin
		Open(File, In_File, Name);
		Status := Success;
	exception
		when Name_Error =>
			Put_Line("Error: File not found ->" & Name);
			Status := Not_Found;

		when Use_Error =>
			Put_Line("Error: Permission denied ->" & Name);
			Status := Permission_Denied;

		when others =>
			Put_Line("Error: Unknown IO error ->" & Name);
			Status := Unknown_Error;

	end Try_Open;

end Safe_IO;
