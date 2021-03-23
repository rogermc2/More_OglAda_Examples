
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Keys;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;

with Glfw;

with Maths;
with Utilities;

with Audio;
with Camera;
with Cursor_Shader_Manager;
with Game_Utils;
with GL_Maths;
with GL_Utils;
with Input_Handler;
with Menu_Credits_Shader_Manager;
with Main_Menu_Initialization;
with Menu_Strings;
with Menu_Support;
with Mesh_Loader;
with Settings;
with Shader_Attributes;
with Text;
with Texture_Manager;
with Title_Shader_Manager;

package body Main_Menu is
   use GL.Types;
   use Menu_Strings;
   use Menu_Support;
   use Main_Menu_Initialization;

   Black              : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
   Menu_Text_Y_Offset : constant Single := 270.0; --  orig 300 pixels above horizontal for text to start
   Menu_Big_Text_Size : constant Single := 92.0;  --  orig 80 height of subseq lines to offset below that
   Credit_Scroll_Rate : constant Float := 0.1;    --  orig 0.5
   Credits_Music      : constant String := "Protagonist_through_Pain.ogg";

   Title_VAO              : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Cursor_VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Menu_VAO               : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Main_Text              : Main_Text_Array := (others => -1);
   Graphics_Text          : Graphic_Value_Array := (others => -1);
   Audio_Text             : Audio_Text_Array := (others => -1);
   Audio_Value_Text       : Audio_Text_Array := (others => -1);
   Input_Text             : Input_Text_Array := (others => -1);
   Input_Value_Text       : Input_Text_Array := (others => -1);
   Quit_Text              : Quit_Text_Array := (others => -1);
   Confirm_Quit_Text      : Quit_Text_Array := (others => -1);

   Menu_Is_Open           : Boolean := False;
   Menu_Closed            : Boolean := False;
   Menu_Credits_Open      : Boolean := False;
   Menu_End_Story_Open    : Boolean := False;
   Menu_Graphics_Open     : Boolean := False;
   Menu_Audio_Open        : Boolean := False;
   Menu_Cal_KB_Open       : Boolean := False;
   Menu_Input_Open        : Boolean := False;
   Menu_Confirm_Quit_Open : Boolean := False;
   Menu_Cal_Gp_Butts_Open : Boolean := False;
   Menu_Cal_Gp_Axes_Open  : Boolean := False;
   Menu_Choice            : Menu_Strings.Main_Choice_Type :=
                              Menu_Strings.Main_New_Game;

   Enabled_Strings        : Menu_String_Array (1 .. 2)
     := (To_Unbounded_String ("disabled"), To_Unbounded_String ("enabled "));
   Tex_Filter_Strings     : Menu_String_Array (1 .. 3)
     := (To_Unbounded_String ("nearest"), To_Unbounded_String ("bilinear"),
         To_Unbounded_String ("trilinear"));
   Graphic_Value_Strings  : Graphic_Value_String_Array :=
                              (others => To_Unbounded_String (""));

   Graphic_Value_Text               : Graphic_Value_Array := (others => -1);
   Cal_KB_Text                      : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   Cal_GP_Text                      : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   GP_Axis_Binding_Text             : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   GP_Buttons_Binding_Text          : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   KB_Binding_Text                  : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);

   User_Chose_Custom_Maps    : Boolean := False;
   User_Chose_New_Game       : Boolean := False;
   We_Are_In_Custom_Maps     : Boolean := False;
   Title_Author_Text         : Integer := -1;
   Title_Buildstamp_Text     : Integer := -1;
   Credits_Text_Pos          : Singles.Vector2 := (0.0, 0.0);
   Credits_Scale             : Singles.Vector2 := (1.0, 1.0);
   Credits_Pos               : Singles.Vector2 := (0.0, 0.0);
   Cal_Kb_Cursor_Curr_Item   : constant Integer := -1;
   Cal_GP_Cursor_Curr_Item   : constant Integer := -1;  -- GP: game pad
   Input_Cursor_Current_Item : Input_Choice_Type;
   Text_Background_Pos       : constant GL.Types.Singles.Vector2 := (0.0, 0.0);
   Text_Background_Scale     : GL.Types.Singles.Vector2 := (1.0, 1.0);
   Text_Background_Texture   : GL.Objects.Textures.Texture;
   Menu_Credits_Texture      : GL.Objects.Textures.Texture;
   Title_Version_Text        : Integer := -1;
   End_Story_Text            : End_Story_Array;
   Joy_Name                  : Unbounded_String := To_Unbounded_String ("");
   Joystick_Detected_Text    : Integer := -1;
   Greatest_Text_Axis        : Integer := -1;
   Restart_Graphics_Text     : constant Positive := 1;
   Graphics_Restart_Flag     : Boolean := False;
   Already_Bound_Text        : Integer := -1;
   Title_Shader_Program      : GL.Objects.Programs.Program;
   Title_Skull_Texture       : GL.Objects.Textures.Texture;
   Greatest_Axis_Text        : Integer := -1;
   Modify_Binding_Mode       : Boolean := False;
   Already_Bound             : Boolean := False;

   Position_Buffer                 : GL.Objects.Buffers.Buffer;
   Texture_Buffer                  : GL.Objects.Buffers.Buffer;
   Menu_Cursor_Curr_Item           : Main_Choice_Type := Main_New_Game;
   Graphic_Cursor_Curr_Item        : Graphic_Choice_Type :=
                                       Graphic_Choice_Type'First;
   Graphic_Preset_Cursor_Curr_Item : Graphic_Preset_Choice_Type :=
                                       Graphic_Preset_Dire;
   Audio_Cursor_Curr_Item          : Audio_Choice_Type :=
                                       Audio_Choice_Type'First;
   Input_Cursor_Curr_Item          : Input_Choice_Type :=
                                       Input_Choice_Type'First;
   Quit_Cursor_Curr_Item           : Quit_Choice_Type :=
                                       Quit_Choice_Type'First;
   --     Cursor_Current_Item         : Integer := -1;
   Cursor_Point_Count              : Integer := 0;
   Cursor_Shader_Program           : GL.Objects.Programs.Program;
   Credits_Shader_Program          : GL.Objects.Programs.Program;
   Menu_Cursor_Texture             : GL.Objects.Textures.Texture;
   Audio_Cursor_Current_Item       : Audio_Choice_Type;
   Title_Point_Count               : Integer := 0;
   Cursor_M                        : Singles.Matrix4 := GL.Types.Singles.Identity4;
   Cursor_V                        : Singles.Matrix4 := GL.Types.Singles.Identity4;
   Title_M                         : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
   Title_V                         : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
   Title_Bounce_Timer              : Float := 5.0;
   Text_Timer                      : Float := 0.0;
   Since_Last_Key                  : Float := 0.0;
   Current_Video_Mode              : Integer := -1;

   procedure Draw_Title;

   --  ------------------------------------------------------------------------

   function Are_We_In_Custom_Maps return Boolean is
   begin
      return We_Are_In_Custom_Maps;
   end Are_We_In_Custom_Maps;

   --  ------------------------------------------------------------------------

   function Credits_Program return GL.Objects.Programs.Program is
   begin
      return Credits_Shader_Program;
   end Credits_Program;

   --  ------------------------------------------------------------------------

   function Did_User_Choose_Custom_Maps return Boolean is
   begin
      return User_Chose_Custom_Maps;
   end Did_User_Choose_Custom_Maps;

   --  ------------------------------------------------------------------------

   function Did_User_Choose_New_Game return Boolean is
   begin
      return User_Chose_New_Game;
   end Did_User_Choose_New_Game;

   --  ------------------------------------------------------------------------

   procedure Draw_Menu (Elapsed : Float) is
      use GL.Objects.Programs;
      use GL.Objects.Vertex_Arrays;
      use GL.Toggles;
      use Menu_Credits_Shader_Manager;
      use Menu_Strings;
      use Settings;
      FB_Width       : constant Single := Single (Framebuffer_Width);
      FB_Height      : constant Single := Single (Framebuffer_Height);
      Cursor_Scale   : Single := 60.0;
      Cursor_Pos     : Singles.Vector2 :=
                         ((-220.0 - 512.0) / Single (Framebuffer_Width), 0.0);

      function Cursor_Y (Val : Integer) return Single is
         Y : constant Single := Single (2 * Val + 1);
      begin
         return (380.0 - 23.0 * Y) / Single (Framebuffer_Height);
      end Cursor_Y;

   begin
      Utilities.Clear_Depth;  --  clear depth buffer so that menu is always on top
      GL.Objects.Programs.Use_Program (Credits_Shader_Program);
      if Menu_Credits_Open then
         Utilities.Clear_Background_Colour_And_Depth (Black);
         Disable (Depth_Test);
         Text_Timer := Text_Timer + Elapsed;

         Menu_VAO.Bind;
         Set_Scale (Credits_Scale);
         Set_Position (Credits_Pos);
         Texture_Manager.Bind_Texture (0, Menu_Credits_Texture);
         Draw_Arrays (Triangles, 0, 6);

         if End_Story_Open then
            for index in End_Story_Array'Range loop
               Text.Move_Text (End_Story_Text (index), Credits_Text_Pos (GL.X),
                               Credits_Text_Pos (GL.Y) - 0.2 * Single (index - 1)
                                 + Single (Credit_Scroll_Rate * Text_Timer));
               Text.Draw_Text (End_Story_Text (index));
            end loop;
         else
            for index in Credits_Text_Array'Range loop
               Text.Move_Text (Credits_Text_ID (index), Credits_Text_Pos (GL.X),
                               Credits_Text_Pos (GL.Y) - 0.2 * Single (index - 1)
                                  + Single (Credit_Scroll_Rate * Text_Timer));
               Text.Draw_Text (Credits_Text_ID (index));
            end loop;
         end if;
         Enable (Depth_Test);
      else  --  Menu_Credits not open
         Enable (Blend);
         --  text background box
         GL.Objects.Programs.Use_Program (Credits_Shader_Program);
         Set_Scale (Text_Background_Scale);
         Set_Position (Text_Background_Pos);

         GL_Utils.Bind_VAO (Menu_VAO);
         Texture_Manager.Bind_Texture (0, Text_Background_Texture);
         --           GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
         Draw_Arrays (Triangles, 0, 6);
         --           Draw_Arrays (Points, 0, 1);

         Disable (Blend);
         Utilities.Clear_Depth;

         if Menu_Graphics_Open then
            for index in Graphic_Choice_Type'Range loop
               Text.Draw_Text (Graphics_Text (index));
               Text.Draw_Text (Graphic_Value_Text (index));
            end loop;
            if Graphics_Restart_Flag then
               Text.Draw_Text (Restart_Graphics_Text);
            end if;
            Cursor_Pos (GL.Y) := Cursor_Y (Graphic_Choice_Type'Enum_Rep
                                           (Graphic_Cursor_Curr_Item));
         elsif Menu_Audio_Open then
            Game_Utils.Game_Log ("Main_Menu.Draw_Menu Menu_Audio_Open");
            for index in Audio_Choice_Type'Range loop
               Text.Draw_Text (Audio_Text (index));
               Text.Draw_Text (Audio_Value_Text (index));
            end loop;
            Cursor_Pos (GL.X) := 0.0;
            Cursor_Pos (GL.Y) :=
              2.0 + Single (Audio_Choice_Type'Enum_Rep (Audio_Cursor_Current_Item));
            Cursor_Pos (GL.Y) := (400.0 - 20.0  * (Cursor_Pos (GL.Y) - 1.0)) /
              Single (Framebuffer_Height);
         elsif Menu_Cal_KB_Open then
            Game_Utils.Game_Log ("Main_Menu.Draw_Menu Menu_Cal_KB_Open");
            for index in 1 .. Input_Handler.Num_Actions loop
               Text.Draw_Text (Cal_KB_Text (index));
               Text.Draw_Text (KB_Binding_Text (index));
            end loop;
            if Already_Bound then
               Text.Draw_Text (Already_Bound_Text);
            end if;
            Cursor_Pos (GL.Y) := Cursor_Y (Cal_Kb_Cursor_Curr_Item);
         elsif Menu_Cal_Gp_Butts_Open then
            Game_Utils.Game_Log ("Main_Menu.Draw_Menu Menu_Cal_Gp_Butts_Open");
            for index in 1 .. Input_Handler.Num_Actions loop
               Text.Draw_Text (Cal_GP_Text (index));
               Text.Draw_Text (GP_Buttons_Binding_Text (index));
            end loop;
            Cursor_Pos (GL.Y) := Cursor_Y (Cal_GP_Cursor_Curr_Item);
            Text.Draw_Text (Greatest_Axis_Text);
            if Already_Bound then
               Text.Draw_Text (Already_Bound_Text);
            end if;
         elsif Menu_Cal_Gp_Axes_Open then
            Game_Utils.Game_Log ("Main_Menu.Draw_Menu Menu_Cal_Gp_Axes_Open");
            for index in 1 .. Input_Handler.Num_Actions loop
               Text.Draw_Text (Cal_GP_Text (index));
               Text.Draw_Text (GP_Axis_Binding_Text (index));
            end loop;
            Cursor_Pos (GL.Y) := Cursor_Y (Cal_GP_Cursor_Curr_Item);
            Text.Draw_Text (Greatest_Axis_Text);
            if Already_Bound then
               Text.Draw_Text (Already_Bound_Text);
            end if;
         elsif Menu_Input_Open then
            Game_Utils.Game_Log ("Main_Menu.Draw_Menu Menu_Input_Open");
            for index in Input_Choice_Type'Range loop
               Text.Draw_Text (Input_Text (index));
            end loop;
            Text.Draw_Text (Input_Value_Text (Input_Choice_Type'First));
            Cursor_Pos (GL.Y) :=
              2.0 + Single (Input_Choice_Type'Enum_Rep (Input_Cursor_Current_Item));
            Cursor_Pos (GL.Y) := (400.0 - 20.0  * (Cursor_Pos (GL.Y) - 1.0)) /
              Single (Framebuffer_Height);
            Text.Draw_Text (Joystick_Detected_Text);
         elsif Menu_Confirm_Quit_Open then
            Game_Utils.Game_Log ("Main_Menu.Draw_Menu Menu_Confirm_Quit_Open");
            for index in Quit_Choice_Type'Range loop
               Text.Draw_Text (Confirm_Quit_Text (index));
            end loop;
            Cursor_Pos (GL.Y) := 0.0;
         else  --  Draw main menu
            for index in Main_Choice_Type'Range loop
               Text.Draw_Text (Main_Text (index));
            end loop;

            --  Set scale and position for drawing Skull_Cursor
            Cursor_Scale := 120.0;
            Cursor_Pos (GL.X) := -312.0 / Single (Framebuffer_Width);
            Cursor_Pos (GL.Y) :=
              Single (Main_Choice_Type'Enum_Rep (Menu_Cursor_Curr_Item));
            Cursor_Pos (GL.Y) :=
              (Menu_Text_Y_Offset - Menu_Big_Text_Size * Cursor_Pos (GL.Y) - 40.0) /
                Single (Framebuffer_Height);
         end if;

         Draw_Skull_Cursor (Menu_Cursor_Texture, Cursor_VAO,
                            Cursor_Shader_Program, Cursor_M, Cursor_V,
                            Cursor_Pos, Cursor_Scale, Cursor_Point_Count,
                            Elapsed);
         Do_Bounce (Title_Bounce_Timer, Elapsed, Title_V);
         Draw_Title;
      end if;
   end Draw_Menu;

   --  ------------------------------------------------------------------------

   procedure Draw_Title is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use GL.Objects.Vertex_Arrays;
      use GL.Types;
      use Singles;
      use Maths;
      use Shader_Attributes;
      Y            : constant Single := 200.0 / Single (Settings.Framebuffer_Height);
      T_Matrix     : constant Singles.Matrix4 :=
                       Translation_Matrix ((0.0, Y, 0.0));
      P_Matrix     : constant Singles.Matrix4 :=
                       T_Matrix * Camera.GUI_Proj_Matrix;
      Current_Time : constant Single := Single (Glfw.Time);
   begin
      GL.Objects.Programs.Use_Program (Title_Shader_Program);
      Title_Shader_Manager.Set_View_Matrix (Title_V);
      Title_Shader_Manager.Set_Model_Matrix (Title_M);
      Title_Shader_Manager.Set_Perspective_Matrix (P_Matrix);
      Title_Shader_Manager.Set_Time (Current_Time);

      if not Title_VAO.Initialized then
         raise Main_Menu_Exception with
           "MMen.Draw_Title_Only, Title_VAO has not been initialized";
      end if;
      GL_Utils.Bind_VAO (Title_VAO);
      Draw_Arrays (Triangles, 0, Int (Title_Point_Count));

   end Draw_Title;

   --  ------------------------------------------------------------------------

   procedure Draw_Title_Only is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use GL.Objects.Vertex_Arrays;
      use GL.Types;
      use Singles;
      use Maths;
      USE Shader_Attributes;
      S_Matrix     : constant Singles.Matrix4 := Scaling_Matrix (5.0);  --  orig 10.0
      T_Matrix     : constant Singles.Matrix4 :=
                       Translation_Matrix ((0.0, -10.0, -30.0)); --  orig 0.0, -10.0, -30.0
      M_Matrix     : constant Singles.Matrix4 := T_Matrix * S_Matrix;
      Title_Matrix : constant Singles.Matrix4 :=
                       Translation_Matrix ((-0.4, -2.0, -1.0)); --  orig z -1.0
      Current_Time : constant Single := Single (Glfw.Time);
   begin
      --  Draw cursor skull in background
      Texture_Manager.Bind_Texture (0, Title_Skull_Texture);
      if not Title_Skull_Texture.Initialized then
         raise Main_Menu_Exception with
           "MMen.Draw_Title_Only, Title_Skull_Texture has not been initialized";
      elsif not Cursor_VAO.Initialized then
         raise Main_Menu_Exception with
           "MMen.Draw_Title_Only, Cursor_VAO has not been initialized";
      end if;
      GL_Utils.Bind_VAO (Cursor_VAO);

      GL.Objects.Programs.Use_Program (Cursor_Shader_Program);
      Cursor_Shader_Manager.Set_Model_Matrix (M_Matrix);
      Cursor_Shader_Manager.Set_Perspective_Matrix (Camera.Projection_Matrix);
      Cursor_Shader_Manager.Set_View_Matrix (Cursor_V);
      Draw_Arrays (Triangles, 0, Int (Cursor_Point_Count));

      --  3D title
      GL.Objects.Programs.Use_Program (Title_Shader_Program);
      Title_Shader_Manager.Set_View_Matrix (Title_V);
      Title_Shader_Manager.Set_Model_Matrix (Title_Matrix);
      Title_Shader_Manager.Set_Perspective_Matrix (Camera.Projection_Matrix);
      Title_Shader_Manager.Set_Time (Current_Time);

      if not Title_VAO.Initialized then
         raise Main_Menu_Exception with
           "MMen.Draw_Title_Only, Title_VAO has not been initialized";
      end if;
      GL_Utils.Bind_VAO (Title_VAO);
      Draw_Arrays (Triangles, 0, Int (Title_Point_Count));

      --  Draw library logos and stuff
      Text.Draw_Text (Title_Author_Text);
      Text.Draw_Text (Title_Buildstamp_Text);

   end Draw_Title_Only;

   --  ------------------------------------------------------------------------

   function End_Story_Open return Boolean is
   begin
      return Menu_End_Story_Open;
   end End_Story_Open;

   --  ------------------------------------------------------------------------

   procedure Flag_End_Story_Credits_Start is
   begin
      Menu_End_Story_Open := True;
      Menu_Credits_Open := True;
   end Flag_End_Story_Credits_Start;

   --  ------------------------------------------------------------------------

   procedure Init is
      use Main_Menu_Initialization;
   begin
      Game_Utils.Game_Log ("---MAIN MENU---");
      Init_Position_And_Texture_Buffers (Menu_VAO, Position_Buffer, Texture_Buffer);
      Init_Title (Title_Author_Text, Title_Buildstamp_Text,
                  Title_M, Title_V , Title_Shader_Program, Title_VAO,
                  Title_Point_Count);
      Init_Cursor (Cursor_Shader_Program, Cursor_VAO, Cursor_M, Cursor_V,
                   Cursor_Point_Count);
      Init_Credits (Credits_Shader_Program, Text_Background_Scale,
                    Credits_Text_ID, Credits_Scale, Credits_Pos,
                    Credits_Text_Pos);
      Init1 (End_Story_Text, Credits_Text_Pos, Text_Background_Texture,
             Menu_Credits_Texture, Title_Skull_Texture, Menu_Cursor_Texture);
      Init_Main_Menu_Text (Main_Text);   --  mmenu.cpp aprox line 536
      Init_Graphic_Value_Strings (Enabled_Strings, Graphic_Value_Strings);   --  mmenu.cpp aprox line 546
      Init_Graphic_Text (Graphics_Text, Graphic_Value_Text, Graphic_Value_Strings);
      Init_Audio_Value_Strings (Audio_Text, Audio_Value_Text);
      Init_Input_Text (Input_Text, Input_Value_Text, Enabled_Strings);
      Init_Input_Actions (Cal_KB_Text, Cal_GP_Text, KB_Binding_Text,
                          GP_Axis_Binding_Text, GP_Buttons_Binding_Text);
      Init_Quit_Text (Confirm_Quit_Text);
      Init_Various (Input_Text, To_String (Joy_Name), Joystick_Detected_Text,
                    Greatest_Axis_Text, Already_Bound_Text);

   end Init;

   --  ------------------------------------------------------------------------

   function Menu_Open return Boolean is
   begin
      return Menu_Is_Open;
   end Menu_Open;

   --  ------------------------------------------------------------------------

   function Menu_Was_Closed return Boolean is
   begin
      return Menu_Closed;
   end Menu_Was_Closed;

   --  ------------------------------------------------------------------------

   procedure Play_End_Story_Music is
   begin
	Audio.Pause_Music (True);
	Audio.Play_Credits_Music (Credits_Music);
   end Play_End_Story_Music;

   --  ------------------------------------------------------------------------

   procedure Set_Joystick_Name (Name : String) is
   begin
      Joy_Name := To_Unbounded_String (Name);
   end Set_Joystick_Name;

   --  ------------------------------------------------------------------------

   procedure Set_Menu_Open (State : Boolean) is
   begin
      Menu_Is_Open := State;
   end Set_Menu_Open;

   --  ------------------------------------------------------------------------

   procedure Start_Menu_Title_Bounce is
   begin
      Title_Bounce_Timer := 0.0;
   end Start_Menu_Title_Bounce;

   --  ------------------------------------------------------------------------

   function Update_Main_Menu (Window     : in out Input_Callback.Barbarian_Window;
                              Delta_Time : Float) return Boolean is
      use Glfw.Input.Keys;
      use Input_Handler;
      use Menu_Support;
      use Menu_Strings;
      use Settings;
      Temp    : Unbounded_String := To_Unbounded_String ("");
      Result  : Boolean := False;
   begin
      Since_Last_Key := Since_Last_Key + Delta_Time;
      Menu_Closed := False;    --  Menu_Was_Closed
      User_Chose_Custom_Maps := False;
      User_Chose_New_Game := False;
      --  Joystick processsing
      Result := Since_Last_Key < 0.15;
      if not Result then
         --  Since_Last_Key > 0.15
         if Menu_Graphics_Open then
            Result := Process_Menu_Graphics
              (Window, Graphic_Value_Text, Menu_Graphics_Open,
               Graphics_Restart_Flag, Since_Last_Key, Graphic_Cursor_Curr_Item,
               Current_Video_Mode);
         end if;

         if Menu_Audio_Open then
            Game_Utils.Game_Log ("Main_Menu.Update_Menu Menu_Audio_Open");
            Process_Menu_Audio (Window, Audio_Value_Text, Menu_Audio_Open,
                                Since_Last_Key, Audio_Cursor_Current_Item);
         end if;

         if Menu_Cal_KB_Open then
            Game_Utils.Game_Log ("Main_Menu.Update_Menu Menu_Cal_KB_Open");
            Process_Menu_Cal_KB (Window, KB_Binding_Text, Greatest_Axis_Text,
                                 Already_Bound_Text, Modify_Binding_Mode,
                                 Already_Bound, Menu_Cal_KB_Open, Since_Last_Key);
         end if;  --  Menu_Audio_Open
         if Menu_Cal_Gp_Butts_Open or Menu_Cal_Gp_Axes_Open then
            Game_Utils.Game_Log ("Main_Menu.Update_Menu Menu_Cal_Gp_Butts_Open");
            Process_Menu_Cal_GP;
         end if;

         if Menu_Input_Open then
            Game_Utils.Game_Log ("Main_Menu.Update_Menu Menu_Input_Open");
            Process_Menu_Input (Window, To_String (Joy_Name), Since_Last_Key,
                                Menu_Input_Open, Menu_Cal_KB_Open,
                                Menu_Cal_Gp_Axes_Open, Menu_Cal_Gp_Butts_Open,
                                Joystick_Detected_Text, Input_Cursor_Current_Item);
         end if;

         if Menu_Confirm_Quit_Open then
            Game_Utils.Game_Log ("Main_Menu.Update_Menu Menu_Confirm_Quit_Open");
            Result := Confirm_Quit_Open (Window, Menu_Confirm_Quit_Open);
         end if;

         if Menu_Credits_Open then
            Check_Close_Menu_Credits (Window, Menu_Credits_Open,
                                      Menu_End_Story_Open, Menu_Closed, Text_Timer);
         end if;

         General_Menu_Support (Window, Joystick_Detected_Text,
                               To_String (Joy_Name), Menu_Closed,
                               Menu_Graphics_Open, Menu_Audio_Open,
                               Menu_Input_Open, Menu_Confirm_Quit_Open,
                               Menu_Credits_Open, User_Chose_New_Game,
                               We_Are_In_Custom_Maps, User_Chose_Custom_Maps,
                               Since_Last_Key, Menu_Cursor_Curr_Item);
      end if; --  Since_Last_Key < 0.15

      return not Menu_Confirm_Quit_Open;
   end Update_Main_Menu;

   --  ------------------------------------------------------------------------

end Main_Menu;
