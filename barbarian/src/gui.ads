
with GL.Types;

package GUI is

   procedure Change_Crong_Head (Health_Factor : GL.Types.Single);
   procedure Change_Health_Bar (Index         : GL.Types.Int;
                                Health_Factor : GL.Types.Single;
                                Name  : String);
   procedure Draw_Controller_Button_Overlays (Elapsed : Float);
   procedure Init_GUIs;
   procedure Load_Gui_Shaders;
   procedure Reset_GUIs;
   --      procedure Set_GUI_Gold (Amount : Integer);
   --      procedure Set_GUI_Javalin_Ammo (Amount : Integer);
   --      procedure Set_GUI_Kills (Amount : Integer);
   procedure Show_Controller_Button_Overlay
     (Pos_Index, Tex_Index : in out Integer);
   function Show_Defeated return Boolean;
   procedure Show_Finished_Stats (Won : Boolean;
                                  Kills, Kills_Max, Gold, Gold_Max : Integer;
                                  Time  : Float; Par : String);
   function Show_Victory return Boolean;
   procedure Update_GUIs (Seconds : Float);

end GUI;
