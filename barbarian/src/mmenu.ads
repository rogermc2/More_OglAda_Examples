
with Glfw.Windows;

package MMenu is

   MMenu_Exception : Exception;

   function Are_We_In_Custom_Maps return Boolean;
   function Did_User_Choose_Custom_Maps return Boolean;
   function Did_User_Choose_New_Game return Boolean;
   procedure Draw_Menu (Elapsed : Float);
   procedure Draw_Title_Only;
   function End_Story_Open return Boolean;
   procedure Init;
   function Menu_Open return Boolean;
   function Menu_Was_Closed return Boolean;
   procedure Set_Menu_Open (State : Boolean);
   procedure Start_Menu_Title_Bounce;
   function Update_Menu (Window     : in out Glfw.Windows.Window;
                         Delta_Time : Float) return Boolean;

end MMenu;
