
with Ada.Text_IO; use Ada.Text_IO;

package Audio_Manager is

   Audio_Manager_Exception : Exception;

   function Init_Audio return Boolean;
   procedure Load_Ambient_Sounds (Input_File : File_Type);

end Audio_Manager;
