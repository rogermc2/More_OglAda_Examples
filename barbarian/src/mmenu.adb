
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glfw.Input.Keys;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;

with Glfw;

with Maths;
with Utilities;

with Camera;
with Cursor_Shader_Manager;
with Game_Utils;
with GL_Maths;
with GL_Utils;
with Input_Handler;
with Menu_Credits_Shader_Manager;
with MMenu_Initialization;
with Menu_Strings;
with Mesh_Loader;
with Settings;
with Shader_Attributes;
with Text;
with Texture_Manager;
with Title_Shader_Manager;

package body MMenu is
   use GL.Types;
   use Menu_Strings;

   type Menu_Choice is (Menu_New_Game, Menu_Custom_Map, Menu_Graphics,
                        Menu_Audio, Menu_Input, Menu_Credits, Menu_Quit);

   Black                : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
   Cursor_VAO           : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Title_VAO            :GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Menu_VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Menu_Is_Open         : Boolean := False;
   Menu_Closed          : Boolean := False;
   Menu_Credits_Open    : Boolean := False;
   Menu_End_Story_Open  : Boolean := False;
   Menu_Gr_Open         : Boolean := False;

   Enabled_Strings                         :
   MMenu_Initialization.Menu_String_Array (1 .. 2)
     := (To_Unbounded_String ("disabled"), To_Unbounded_String ("enabled "));
   Tex_Filter_Strings                      : MMenu_Initialization.Menu_String_Array (1 .. 3)
     := (To_Unbounded_String ( "nearest"), To_Unbounded_String ("bilinear"),
         To_Unbounded_String ("trilinear"));
   Menu_Text                               : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Menu_Entries) := (others => -1);
   Graphics_Text                           : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Graphic_Entries) := (others => -1);
   Graphic_Value_Strings                   :
   MMenu_Initialization.Menu_String_Array (1 .. Menu_Strings.Num_Graphic_Entries)
     := (others => To_Unbounded_String (""));
   Graphic_Value_Text                      : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Graphic_Entries) := (others => -1);
   Cal_KB_Text                             : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   Cal_GP_Text                             : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   GP_Axis_Binding_Text                    : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   GP_Buttons_Binding_Text                 : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   Audio_Text                              : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Audio_Entries) := (others => -1);
   Audio_Value_Text                        : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Audio_Entries) := (others => -1);
   Input_Text                              : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Input_Entries) := (others => -1);
   Input_Value_Text                        : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Input_Entries) := (others => -1);
   Quit_Text                               : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Quit_Entries) := (others => -1);
   Confirm_Quit_Text                       : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Quit_Entries) := (others => -1);
   KB_Binding_Text                         : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);

   User_Chose_Custom_Maps     : Boolean := False;
   User_Chose_New_Game        : Boolean := False;
   We_Are_In_Custom_Maps      : Boolean := False;
   Title_Author_Text          : Integer := -1;
   Title_Buildstamp_Text      : Integer := -1;
   Credits_Text_X             : Single := 0.0;
   Credits_Text_Y             : Single := -1.0;
   Credits_X                  : Single := 0.0;
   Credits_Y                  : Single := -1.0;
   Credits_Pos_X              : Single := 0.0;
   Credits_Pos_Y              : Single := -1.0;
   Text_Background_Texture    : GL.Objects.Textures.Texture;
   Menu_Credits_Texture       : GL.Objects.Textures.Texture;
   Title_Version_Text         : Integer := -1;
   End_Story_Text             : Integer := -1;
   Joy_Name                   : Unbounded_String := To_Unbounded_String ("");
   Joystick_Detected_Text     : Integer := -1;
   Greatest_Text_Axis         : Integer := -1;
   Restart_Graphics_Text      : Integer := -1;
   Already_Bound_Text         : Integer := -1;
   Title_Shader_Program       : GL.Objects.Programs.Program;
   Title_Skull_Texture        : GL.Objects.Textures.Texture;

   Position_Buffer             : GL.Objects.Buffers.Buffer;
   Texture_Buffer              : GL.Objects.Buffers.Buffer;
   Menu_Cursor_Curr_Item       : Integer := -1;
   Cursor_Current_Item         : Integer := -1;
   Cursor_Point_Count          : Integer := 0;
   Cursor_Shader_Program       : GL.Objects.Programs.Program;
   Menu_Cursor_Texture         : GL.Objects.Textures.Texture;
   Title_Point_Count           : Integer := 0;
   Cursor_V                    : Singles.Matrix4 := GL.Types.Singles.Identity4;
   Title_M                     : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
   Title_V                     : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
   Title_Bounce_Timer          : Float := 5.0;
   Text_Timer                  : Float := 0.0;
   Since_Last_Key              : Float := 0.0;
   Credits_Shader_Program      : GL.Objects.Programs.Program;

   --  ------------------------------------------------------------------------

   function Are_We_In_Custom_Maps return Boolean is
   begin
      return We_Are_In_Custom_Maps;
   end Are_We_In_Custom_Maps;

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
      use GL.Objects.Vertex_Arrays;
      use GL.Toggles;
      use Menu_Credits_Shader_Manager;
      use Menu_Strings;
   begin
      if Menu_Cursor_Curr_Item < 0 then
         Menu_Cursor_Curr_Item := Integer (Num_Menu_Entries - 1);
      elsif Menu_Cursor_Curr_Item >= Integer (Num_Menu_Entries) then
         Menu_Cursor_Curr_Item := 0;
      end if;

      Utilities.Clear_Depth;
      if Menu_Credits_Open then
         Utilities.Clear_Background_Colour_And_Depth (Black);
         Disable (Depth_Test);
         Text_Timer := Text_Timer + Elapsed;
         Menu_VAO.Bind;
         GL.Objects.Programs.Use_Program (Credits_Shader_Program);
         Set_Scale ((Credits_X, Credits_Y));
         Set_Position ((Credits_Pos_X, Credits_Pos_Y));
         Texture_Manager.Bind_Texture (0, Menu_Credits_Texture);

         GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Shader_Attributes.Attrib_VP, 2, Single_Type, False, 0, 0);

         GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Shader_Attributes.Attrib_VT, 2, Single_Type, False, 0, 0);
         --           Game_Utils.Game_Log ("Mmenu.Draw_Menu drawing menu");
         Draw_Arrays (Triangles, 0, 6);
      end if;
      Enable (Depth_Test);
   end Draw_Menu;

   --  ------------------------------------------------------------------------

   procedure Draw_Title_Only is
      use GL.Objects.Vertex_Arrays;
      use GL.Types;
      use Singles;
      use Maths;
      USE Shader_Attributes;
      S_Matrix     : constant Singles.Matrix4 := Scaling_Matrix (5.0);  --  orig 10.0
      T_Matrix     : constant Singles.Matrix4 :=
                       Translation_Matrix ((0.0, 0.0, -30.0)); --  orig 0.0, -10.0, -30.0
      M_Matrix     : constant Singles.Matrix4 := T_Matrix * S_Matrix;
      Title_Matrix : constant Singles.Matrix4 :=
                       Translation_Matrix ((-0.4, -3.0, 1.0)); --  orig z -1.0
      Current_Time : constant Single := Single (Glfw.Time);
   begin

      --  Draw cursor skull in background
      Texture_Manager.Bind_Texture (0, Title_Skull_Texture);
      if not Title_Skull_Texture.Initialized then
         raise MMenu_Exception with
         "MMen.Draw_Title_Only, Title_Skull_Texture has not been initialized";
      end if;
      if not Cursor_VAO.Initialized then
         raise MMenu_Exception with
         "MMen.Draw_Title_Only, Cursor_VAO has not been initialized";
      end if;
      GL_Utils.Bind_VAO (Cursor_VAO);

      --        Game_Utils.Game_Log ("Mmenu.Draw_Title_Only Far: " &
      --                               Single'Image (Settings.Far_Clip));

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
         raise MMenu_Exception with
         "MMen.Draw_Title_Only, Title_VAO has not been initialized";
      end if;
      GL_Utils.Bind_VAO (Title_VAO);
      Draw_Arrays (Triangles, 0, Int (Title_Point_Count));
      Game_Utils.Game_Log ("Mmenu.Draw_Title_Only Title drawn");
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

   procedure Init is
      use MMenu_Initialization;
   begin
      Init (Cursor_VAO, Title_VAO, Menu_VAO, Enabled_Strings,
            Tex_Filter_Strings,
            Menu_Text, Graphics_Text, Graphic_Value_Text, Cal_KB_Text,
            Cal_GP_Text, GP_Axis_Binding_Text, GP_Buttons_Binding_Text,
            Audio_Text, Audio_Value_Text, Input_Text, Input_Value_Text,
            Confirm_Quit_Text, KB_Binding_Text, Graphic_Value_Strings,
            Title_Author_Text, Title_Buildstamp_Text, Title_Shader_Program,
            Cursor_Shader_Program, Credits_Shader_Program, Cursor_Point_Count,
            Title_Point_Count, Position_Buffer, Texture_Buffer,
            Text_Background_Texture,  Menu_Credits_Texture,
            Title_Skull_Texture, Menu_Cursor_Texture, Title_M, Title_V);
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

   function Update_Menu (Window     : in out Glfw.Windows.Window;
                         Delta_Time : Float) return Boolean is
      use Glfw.Input.Keys;
      use Input_Handler;
      Num_Video_Modes : constant Integer := 10;
      type Size_Array is array (1 .. Num_Video_Modes) of Integer;
      Widths          : Size_Array := (640, 800, 1024, 1280, 1600,
                                       1920, 1280, 1366, 1600, 1920);
      Heights         : Size_Array := (480, 600, 768, 960, 1200,
                                       1440, 720, 768, 900, 1080);
      Temp            : Unbounded_String := To_Unbounded_String ("");
      Result          : Boolean := False;
   begin
      Since_Last_Key := Since_Last_Key + Delta_Time;
      Menu_Closed := False;
      User_Chose_Custom_Maps := False;
      User_Chose_New_Game := False;
      --  Joystick processsing
      Result := Since_Last_Key < 0.15;
      if not Result then
         Result := Menu_Gr_Open;
         if Result then
            Result := Was_Key_Pressed (Window, Escape) or
              Was_Action_Pressed (Window, Open_Menu_Action) or
              Was_Action_Pressed (Window, Menu_Back_Action);
            if Result then
               Menu_Gr_Open := False;
            else
               Result := Was_Key_Pressed (Window, Enter) or
                 Was_Action_Pressed (Window, OK_Action) or
                 Was_Action_Pressed (Window, Attack_Action);
               if Result then
                  case Cursor_Current_Item is
                     when 0 => null;
                     when 1 => null;
                     when 2 => null;
                     when 3 => null;
                     when 4 => null;
                     when 5 => null;
                     when 6 => null;
                     when 7 => null;
                     when 8 => null;
                     when 9 => null;
                     when 10 => null;
                     when 11 => null;
                     when 12 => null;
                     when 13 => null;
                     when 14 => null;
                     when 15 => null;
                     when 16 => null;
                     when others => null;
                  end case;
               else
                  Result := Is_Key_Down (Up);
               end if;
            end if;
         end if;
      end if;

      return Result;
   end Update_Menu;

   --  ------------------------------------------------------------------------

end MMenu;
