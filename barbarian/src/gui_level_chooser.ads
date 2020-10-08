
with Glfw.Windows;

package GUI_Level_Chooser is

   GUI_Level_Chooser_Exception : Exception;

   function Cheated_On_Map return Boolean;
   procedure Init;
   function Get_Selected_Map_Name (Custom : Boolean) return String;
   procedure Set_Cheated_On_Map (State : Boolean);
   function Start_Level_Chooser_Loop (Window : in out Glfw.Windows.Window;
                                      Custom_Maps : Boolean) return Boolean;

end GUI_Level_Chooser;
