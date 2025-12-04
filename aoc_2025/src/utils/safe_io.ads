with Ada.Text_IO; use Ada.Text_IO;

package Safe_IO 
	with SPARK_Mode => On
is
   type File_Status is
	   (Success, Not_Found, Permission_Denied, Unknown_Error);

   procedure Try_Open (File : in out File_Type;
                       Name : String;
                       Status : out File_Status)
     with
       Global => (In_Out => Ada.Text_IO.File_System),
       Pre    => not Is_Open(File),
       Post   => (if Status = Success then Is_Open(File) 
   				  else not Is_Open(File));

end Safe_IO;
