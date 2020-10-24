
package GUI is

   procedure Draw_Controller_Button_Overlays (Elapsed : Float);
   procedure Init_GUIs;
   procedure Load_Gui_Shaders;
   --      procedure Set_GUI_Gold (Amount : Integer);
   --      procedure Set_GUI_Javalin_Ammo (Amount : Integer);
   --      procedure Set_GUI_Kills (Amount : Integer);
   procedure Show_Controller_Button_Overlay
     (Pos_Index, Tex_Index : in out Integer);

end GUI;
