
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

with Audio;
with Camera;
with Cursor_Shader_Manager;
with Game_Utils;
with GL_Maths;
with GL_Utils;
with Input_Handler;
with Menu_Credits_Shader_Manager;
with MMenu_Initialization;
with Menu_Strings;
with Menu_Support;
with Mesh_Loader;
with Settings;
with Shader_Attributes;
with Text;
with Texture_Manager;
with Title_Shader_Manager;

package body MMenu is
   use GL.Types;
   use MMenu_Initialization;

   type Menu_Choice is (Menu_New_Game, Menu_Custom_Map, Menu_Graphics,
                        Menu_Audio, Menu_Input, Menu_Credits, Menu_Quit);

   Black                : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
   Cursor_VAO           : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Title_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Menu_VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Menu_Is_Open         : Boolean := False;
   Menu_Closed          : Boolean := False;
   Menu_Credits_Open    : Boolean := False;
   Menu_End_Story_Open  : Boolean := False;
   Menu_Gr_Open         : Boolean := False;
   Menu_Audio_Open      : Boolean := False;

   Enabled_Strings                         : Menu_String_Array (1 .. 2)
     := (To_Unbounded_String ("disabled"), To_Unbounded_String ("enabled "));
   Tex_Filter_Strings                      : Menu_String_Array (1 .. 3)
     := (To_Unbounded_String ("nearest"), To_Unbounded_String ("bilinear"),
         To_Unbounded_String ("trilinear"));
   Menu_Text                               : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Menu_Entries) := (others => -1);
   Graphics_Text                           : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Graphic_Entries) := (others => -1);
   Graphic_Value_Strings                   : Menu_String_Array (1 .. Menu_Strings.Num_Graphic_Entries)
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
   Credits_Text_ID            : Integer := -1;
   Credits_Text_X             : Single := 0.0;
   Credits_Text_Y             : Single := -1.0;
   Credits_X                  : Single := 0.0;
   Credits_Y                  : Single := -1.0;
   Credits_Pos_X              : Single := 0.0;
   Credits_Pos_Y              : Single := -1.0;
   Text_Background_Pos        : GL.Types.Singles.Vector2;
   Text_Background_Texture    : GL.Objects.Textures.Texture;
   Menu_Credits_Texture       : GL.Objects.Textures.Texture;
   Title_Version_Text         : Integer := -1;
   End_Story_Text             : Integer := -1;
   Joy_Name                   : Unbounded_String := To_Unbounded_String ("");
   Joystick_Detected_Text     : Integer := -1;
   Greatest_Text_Axis         : Integer := -1;
   Restart_Graphics_Text      : Integer := -1;
   Graphics_Restart_Flag      : Boolean := False;
   Already_Bound_Text         : Integer := -1;
   Title_Shader_Program       : GL.Objects.Programs.Program;
   Title_Skull_Texture        : GL.Objects.Textures.Texture;
   Title_Mesh                 : Integer := 0;
   Title_Mesh_ID              : Integer := -1;

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
   Current_Video_Mode          : Integer := -1;
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

         Game_Utils.Game_Log ("Mmenu.Draw_Menu drawing menu");
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
      elsif not Cursor_VAO.Initialized then
         raise MMenu_Exception with
           "MMen.Draw_Title_Only, Cursor_VAO has not been initialized";
      end if;
      GL_Utils.Bind_VAO (Cursor_VAO);

      GL.Objects.Programs.Use_Program (Cursor_Shader_Program);
      Cursor_Shader_Manager.Set_Model_Matrix (M_Matrix);
      Cursor_Shader_Manager.Set_Perspective_Matrix (Camera.Projection_Matrix);
      Cursor_Shader_Manager.Set_View_Matrix (Cursor_V);
      Draw_Arrays (Triangles, 0, Int (Cursor_Point_Count));

      Utilities.Print_Matrix ("Title_Matrix", Title_Matrix);
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
      Game_Utils.Game_Log ("---MAIN MENU---");
      Init_Position_And_Texture_Buffers (Menu_VAO, Position_Buffer, Texture_Buffer);
      Init_Title (Title_Mesh_ID, Title_Author_Text, Title_Buildstamp_Text,
                  Title_M, Title_V , Title_Shader_Program, Title_VAO,
                  Title_Point_Count);
      Init_Cursor (Title_Mesh, Menu_Cursor_Texture,
                   Cursor_Shader_Program, Cursor_VAO, Cursor_Point_Count);
      Init_Credits (Credits_Shader_Program, Text_Background_Pos, Credits_Text_ID);
      Init1 (Menu_Text, End_Story_Text, Text_Background_Texture,
              Menu_Credits_Texture, Title_Skull_Texture);
      Init_Graphic_Value_Strings (Enabled_Strings, Graphic_Value_Strings);
      Init_Audio_Value_Strings (Audio_Text, Audio_Value_Text);
      Init_Input_Text (Input_Text);
      Init_Input_Actions (Cal_KB_Text, Cal_GP_Text, KB_Binding_Text,
                          GP_Axis_Binding_Text, GP_Buttons_Binding_Text);
      Init_Graphic_Text (Graphics_Text, Graphic_Value_Text, Graphic_Value_Strings);
      Init_Quit_Text (Input_Value_Text, Confirm_Quit_Text, Enabled_Strings);
      Init_Various (Input_Text, To_String (Joy_Name));

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

   function Update_Menu (Window     : in out Glfw.Windows.Window;
                         Delta_Time : Float) return Boolean is
      use Glfw.Input.Keys;
      use Input_Handler;
      use Menu_Support;
      use Menu_Strings;
      use Settings;
      Temp            : Unbounded_String := To_Unbounded_String ("");
      Result          : Boolean := False;
   begin
      Since_Last_Key := Since_Last_Key + Delta_Time;
      Menu_Closed := False;
      User_Chose_Custom_Maps := False;
      User_Chose_New_Game := False;
      --  Joystick processsing
      Result := Since_Last_Key > 0.15;
      if Result then
         Result := Menu_Gr_Open;
         if Result then
            Result := Process_Menu_Gr
              (Window, Graphic_Value_Text, Menu_Gr_Open, Graphics_Restart_Flag,
               Since_Last_Key, Cursor_Current_Item, Current_Video_Mode);
         end if;  --  Menu_Gr_Open

         if Menu_Audio_Open then
            Process_Menu_Audio (Cursor_Current_Item);
         end if;  --  Menu_Audio_Open
      end if; --  Since_Last_Key < 0.15

      return Result;
   end Update_Menu;

   --  ------------------------------------------------------------------------

end MMenu;
