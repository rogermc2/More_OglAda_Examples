
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types; use GL.Types;

with Maths;

with Event_Controller;
with Mesh_Loader;
with Prop_Renderer_Support;

package Properties_Manager.Process is

    Properties_Process_Exception : Exception;

    function Get_Index_Of_Prop_Script (Script_File : String) return Natural;
    function Load_Property_Script (File_Name : String; Prop_Index : out Positive)
                                   return Boolean;
    procedure Process_Script_Type
      (New_Props : in out Prop_Renderer_Support.Property_Data;
       aScript : Prop_Renderer_Support.Prop_Script;
       Rx_Kind   : in out Event_Controller.RX_Type;
       Rebalance  : in out Boolean);

end Properties_Manager.Process;
