
with Ada.Text_IO; use Ada.Text_IO;

package Text_Manager is

    Text_Manager_Exception : Exception;

    procedure Preload_Comic_Texts (Input_File : File_Type);

end Text_Manager;
