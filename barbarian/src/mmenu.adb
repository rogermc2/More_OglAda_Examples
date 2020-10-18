
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

   Mmenu_Text_Yoffs   : constant Single := 300.0; -- pixels above horizontal
   --  for text to start
   Mmenu_Big_Text_Sz  : constant Single := 80.0;  -- height of subseq lines to
   --  offset below that
   CRLF               : constant String := ASCII.LF & ASCII.CR;
   CRLF2              : constant String := CRLF & CRLF;
   CRLF3              : constant String := CRLF2 & CRLF;
   CRLF4              : constant String := CRLF2 & CRLF2;

   End_Story_String : constant String :=
                        "crongdor glanced back at the temple" & CRLF2 &
                        "through the shadowy palm groves. a" & CRLF2 &
                        "power, its time long gone, would not" & CRLF2 &
                        "rise again. the place of his barbarian" & CRLF2 &
                        "people assured for now." & CRLF4 &
                        "his friend the merchant was waiting\" & CRLF2 &
                        "with two camels. the merchant eyed" & CRLF2 &
                        "the crown in crongdor's hand." & CRLF4 &
                        "you are rich! what will you do now?" & CRLF4 &
                        "HAH! he said, running his fingers" & CRLF2 &
                        "through his blood-matted mane." & CRLF4 &
                        "with this i will hire a company of" & CRLF2 &
                        "men to PLUNDER THE WORLD!" & CRLF2 &
                        "but first, there is a tavern i know" & CRLF2 &
                        "nearby where the meat is good, and" & CRLF2 &
                        "the drink is strong. gods know, i" & CRLF2 &
                        "have earned it today!" & CRLF3 &
                        "COME!" & CRLF3;

   Black                 : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
   Cursor_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Title_VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Mmenu_Open            : Boolean := False;
   Mmenu_Was_Closed      : Boolean := False;
   Mmenu_Credits_Open    : Boolean := False;
   Mmenu_End_Story_Open  : Boolean := False;
   Mmenu_Gr_Open         : Boolean := False;

   Enabled_Strings         : array (1 .. 2) of Unbounded_String
     := (To_Unbounded_String ("disabled"), To_Unbounded_String ("enabled "));
   Tex_Filter_Strings      : array (1 .. 3) of Unbounded_String
     := (To_Unbounded_String ( "nearest"), To_Unbounded_String ("bilinear"),
         To_Unbounded_String ("trilinear"));
   Menu_Text               : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Menu_Entries) := (others => -1);
   Graphics_Text           : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Graphic_Entries) := (others => -1);
   Graphic_Value_Strings   : array (1 .. Menu_Strings.Num_Graphic_Entries)
     of Unbounded_String := (others => To_Unbounded_String (""));
   Graphic_Value_Text      : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Graphic_Entries) := (others => -1);
   Cal_KB_Text             : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   Cal_GP_Text             : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   GP_Axis_Binding_Text    : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   GP_Buttons_Binding_Text : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);
   Audio_Text              : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Audio_Entries) := (others => -1);
   Audio_Value_Text        : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Audio_Entries) := (others => -1);
   Input_Text              : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Input_Entries) := (others => -1);
   Input_Value_Text        : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Input_Entries) := (others => -1);
   Quit_Text               : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Quit_Entries) := (others => -1);
   Confirm_Quit_Text       : GL_Maths.Integer_Array
     (1 .. Menu_Strings.Num_Quit_Entries) := (others => -1);
   KB_Binding_Text         : GL_Maths.Integer_Array
     (1 .. Input_Handler.Max_Actions) := (others => -1);

   Text_Background_Texture   : GL.Objects.Textures.Texture;
   User_Chose_Custom_Maps                  : Boolean := False;
   User_Chose_New_Game                     : Boolean := False;
   We_Are_In_Custom_Maps      : Boolean := False;
   Title_Author_Text          : Integer := -1;
   Title_Buildstamp_Text      : Integer := -1;
   Credits_Text_X: Single := 0.0;
   Credits_Text_Y: constant Single := -1.0;
   Mmenu_Credits_Texture      : GL.Objects.Textures.Texture;
   Title_Version_Text         : Integer := -1;
   End_Story_Text             : Integer := -1;
   Joy_Name                   : Unbounded_String := To_Unbounded_String ("");
   Joystick_Detected_Text     : Integer := -1;
   Greatest_Text_Axis         : Integer := -1;
   Restart_Graphics_Text      : Integer := -1;
   Already_Bound_Text         : Integer := -1;
   Title_Shader_Program       : GL.Objects.Programs.Program;
   Title_Skull_Texture        : GL.Objects.Textures.Texture;

   Mmenu_Cursor_Curr_Item  : Integer := -1;
   Cursor_Current_Item     : Integer := -1;
   Cursor_Point_Count      : Integer := 0;
   Cursor_Shader_Program   : GL.Objects.Programs.Program;
   Mmenu_Cursor_Texture    : GL.Objects.Textures.Texture;
   Title_Point_Count       : Integer := 0;
   Title_M                 : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
   Title_V                 : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
   Cursor_M                : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
   Cursor_V                : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
   Title_Bounce_Timer      : Float := 5.0;
   Text_Timer              : Float := 0.0;
   Since_Last_Key          : Float := 0.0;


   procedure Init_Audio_Value_Strings;
   procedure Init_Cursor (Title_Mesh : Integer);
   procedure Init_Graphic_Value_Strings;
   procedure Init_Graphic_Text;
   procedure Init_Input_Text;
   procedure Init_Input_Actions;
   procedure Init_Position_And_Texture_Buffers;
   procedure Init_Quit_Text;
   procedure Init_Title (Title_Mesh : out Integer);
   procedure Init_Various;

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
      use GL.Toggles;
      use Menu_Strings;
   begin
      if Mmenu_Cursor_Curr_Item < 0 then
         Mmenu_Cursor_Curr_Item := Integer (Num_Menu_Entries - 1);
      end if;
      if Mmenu_Cursor_Curr_Item >= Integer (Num_Menu_Entries) then
         Mmenu_Cursor_Curr_Item := 0;
      end if;
      Utilities.Clear_Depth;
      if Mmenu_Credits_Open then
         Utilities.Clear_Background_Colour_And_Depth (Black);
         Disable (Depth_Test);
      end if;
      Text_Timer := Text_Timer + Elapsed;
      Enable (Depth_Test);
   end Draw_Menu;

   --  ------------------------------------------------------------------------

   procedure Draw_Title_Only is
      use GL.Objects.Vertex_Arrays;
      use GL.Types;
      use Singles;
      use Maths;
      S_Matrix     : constant Singles.Matrix4 := Scaling_Matrix (10.0);
      T_Matrix     : constant Singles.Matrix4 :=
                       Translation_Matrix ((0.0, -10.0, -30.0));
      M_Matrix     : constant Singles.Matrix4 := T_Matrix * S_Matrix;
      Title_Matrix : constant Singles.Matrix4 :=
                       Translation_Matrix ((-0.4, -3.0, -1.0));
      Current_Time : constant Single := Single (Glfw.Time);
   begin
      --  Draw cursor skull in background
--        Game_Utils.Game_Log ("Mmenu.Draw_Title_Only");
     Texture_Manager.Bind_Texture (0, Title_Skull_Texture);
      Cursor_VAO.Initialize_Id;
      GL_Utils.Bind_VAO (Cursor_VAO);
      GL.Objects.Programs.Use_Program (Cursor_Shader_Program);
      Cursor_Shader_Manager.Set_Perspective_Matrix (Identity4);
      Cursor_Shader_Manager.Set_View_Matrix (Identity4);
      --        Cursor_Shader_Manager.Set_Perspective_Matrix (Camera.Projection_Matrix);
      --        Cursor_Shader_Manager.Set_View_Matrix (Cursor_V);
      Cursor_Shader_Manager.Set_Model_Matrix (Identity4);
--        Game_Utils.Game_Log ("Mmenu.Draw_Title_Only Cursor_Point_Count" &
--                                  Integer'Image (Cursor_Point_Count));
      Draw_Arrays (Triangles, 0, Int (Cursor_Point_Count));

--        Game_Utils.Game_Log ("Mmenu.Draw_Title_Only 3D title");
      --  3D title
      GL.Objects.Programs.Use_Program (Title_Shader_Program);
      --        Title_Shader_Manager.Set_View_Matrix (Title_V);
      --        Title_Shader_Manager.Set_Model_Matrix (Title_Matrix);
      --        Title_Shader_Manager.Set_Perspective_Matrix (Camera.Projection_Matrix);
      Title_Shader_Manager.Set_View_Matrix (Identity4);
      Title_Shader_Manager.Set_Model_Matrix (Identity4);
      Title_Shader_Manager.Set_Perspective_Matrix (Identity4);
      Title_Shader_Manager.Set_Time (Current_Time);

      Title_VAO.Initialize_Id;
      Title_VAO.Bind;
      Draw_Arrays (Triangles, 0, Int (Title_Point_Count));

      --  Draw library logos and stuff
      Text.Draw_Text (Title_Author_Text);
      Text.Draw_Text (Title_Buildstamp_Text);

   end Draw_Title_Only;

   --  ------------------------------------------------------------------------

   function End_Story_Open return Boolean is
   begin
      return Mmenu_End_Story_Open;
   end End_Story_Open;

   --  ------------------------------------------------------------------------

   procedure Init is

      use GL.Objects.Buffers;
      use GL.Types;
      use GL.Types.Singles;
      use Menu_Strings;
      X               : constant Single := 319.0 / Single (Settings.Framebuffer_Width);
      Y               : Single := 19.0 / Single (Settings.Framebuffer_Height);

      Title_Mesh      : Integer := 0;
      Menu_Colour     : constant Singles.Vector4 := (1.0, 1.0, 1.0, 1.0);
   begin
      Game_Utils.Game_Log ("---MAIN MENU---");

      Init_Position_And_Texture_Buffers;
      Init_Title (Title_Mesh);
      Init_Cursor (Title_Mesh);

      --  Credits shader not implemented
      Credits_Text_X := -715.0 / Single (Settings.Framebuffer_Width);

      End_Story_Text := Text.Add_Text (End_Story_String, Credits_Text_X,
                                       Credits_Text_Y, 30.0, 1.0, 1.0, 0.1, 1.0);
      Text.Set_Text_Visible (End_Story_Text, False);

      Texture_Manager.Load_Image_To_Texture
        ("src/textures/title_skull.png", Title_Skull_Texture, False, True);
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/victory.png", Mmenu_Credits_Texture, False, True);
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/text_bkrnd.png", Text_Background_Texture, False, True);

      for index in 1 .. Num_Menu_Entries loop
         Y := Mmenu_Text_Yoffs - Mmenu_Big_Text_Sz * Single (index - 1) /
           Single (Settings.Framebuffer_Height);
         Menu_Text (index) :=
           Text.Add_Text (Menu_Strings.Menu_String_Items (index),
                          0.0, Y, 40.0, 1.0, 1.0, 1.0, 1.0);
         Text.Centre_Text (Menu_Text (index), 0.0, Y);
         Text.Set_Text_Visible (Menu_Text (index), False);
      end loop;

      Init_Graphic_Value_Strings;
      Init_Graphic_Text;
      Init_Audio_Value_Strings;
      Init_Input_Text;
      Init_Various;
      Init_Input_Actions;
      Init_Quit_Text;

   end Init;

   --  ------------------------------------------------------------------------

   procedure Init_Audio_Value_Strings is
      use Menu_Strings;
      use Settings;
      Audio_Value_Strings : array (1 .. Num_Audio_Entries) of Unbounded_String
        := (others => To_Unbounded_String (""));
      X1                  : constant Single :=
                              (-512.0 + 80.0) / Single (Framebuffer_Width);
      X2                  : constant Single :=
                              (512.0 - 330.0) / Single (Framebuffer_Width);
      Y                   : constant Single :=
                              760.0 / Single (Framebuffer_Height);
   begin
      --        Audio_Value_Strings (1) := To_Unbounded_String (Get_Audio_Device_Name);
      Audio_Value_Strings (2) := GL_Utils.To_UB_String (10 * Audio_Volume);
      Audio_Value_Strings (3) := GL_Utils.To_UB_String (10 * Music_Volume);

      for index in 1 .. Num_Audio_Entries loop
         if Audio_Strings (index) /= "" and
           Audio_Value_Strings (index) /= ""then
            Audio_Text (index) :=
              Text.Add_Text (Audio_Strings (index), X1, Single (index + 1) * Y,
                             20.0, 1.0, 1.0, 1.0, 1.0);
            Text.Set_Text_Visible (Graphics_Text (index), False);

            Audio_Value_Text (index) :=
              Text.Add_Text (To_String (Audio_Value_Strings (index)), X2,
                             Single (index + 1) * Y, 20.0, 1.0, 1.0, 1.0, 1.0);
            Text.Set_Text_Visible (Audio_Text (index), False);
         end if;
      end loop;

   end Init_Audio_Value_Strings;

   --  ------------------------------------------------------------------------

   procedure Init_Cursor  (Title_Mesh : Integer) is
      Camera_Position : Singles.Vector3 := (0.0, 0.0, 10.0);
      Camera_Target   : Singles.Vector3 := (0.0, 0.0, 0.0);
      Cursor_Mesh     : Integer := 0;
   begin
         Cursor_Shader_Manager.Init (Cursor_Shader_Program);
      Cursor_M := Singles.Identity4;

      Maths.Init_Lookat_Transform (Camera_Position, Camera_Target,
                                   (0.0, 1.0, 0.0), Title_V);
      Cursor_Shader_Manager.Set_Model_Matrix (Cursor_M);
      Cursor_Shader_Manager.Set_View_Matrix (Cursor_V);
      Cursor_Shader_Manager.Set_Perspective_Matrix (Camera.GUI_Proj_Matrix);

      Cursor_Mesh := Mesh_Loader.Load_Managed_Mesh
        ("src/meshes/skull_helmet.apg", True, True, True, False, False);

      if Cursor_Mesh <= 0 then
         raise MMenu_Exception with
           "MMenu.Init_MMenu Load_Managed_Mesh failed to load src/meshes/skull_helmet.apg";
      elsif not Mesh_Loader.Loaded_Mesh_VAO (Cursor_Mesh, Cursor_VAO) then
         raise MMenu_Exception with
           "MMenu.Init_MMenu failed to initialize VAO for Cursor_Mesh";
      end if;

      Texture_Manager.Load_Image_To_Texture
        ("src/textures/skull_small_helmet_painterv_shade.png",
         Mmenu_Cursor_Texture, False, True);
      Cursor_Point_Count := Mesh_Loader.Point_Count (Title_Mesh);

   end Init_Cursor;

   --  ------------------------------------------------------------------------

   procedure Init_Graphic_Value_Strings is
      use Menu_Strings;
      Graphic_Int     : Integer;
   begin
      Graphic_Int := Settings.Gfx_Preset_Type'Enum_Rep (Settings.Graphic_Preset);
      Append (Graphic_Value_Strings (1), Character'Val (Graphic_Int));
      Graphic_Value_Strings (2) := To_Unbounded_String ("3.2");
      Graphic_Value_Strings (3) := To_Unbounded_String
        (Integer'Image (Settings.Window_Width_To_Save) & 'x' &
           Integer'Image (Settings.Window_Height_To_Save));
      Graphic_Value_Strings (4) := GL_Utils.To_UB_String (Settings.Full_Screen);
      Graphic_Value_Strings (5) := GL_Utils.To_UB_String  (Settings.V_Sync);
      Graphic_Value_Strings (6) := GL_Utils.To_UB_String  (Settings.Shadows_Enabled);
      Graphic_Value_Strings (7) := To_Unbounded_String
        (Integer'Image (Settings.Shadows_Size));
      Graphic_Value_Strings (8) := GL_Utils.To_UB_String (Settings.Render_OLS);
      Graphic_Value_Strings (9) := GL_Utils.To_UB_String (Settings.Fb_Effects_Enabled);
      Graphic_Value_Strings (10) := To_Unbounded_String (Integer'Image (Settings.Texf));
      Graphic_Value_Strings (11) := To_Unbounded_String
        (Float'Image (Settings.Anisotroic_Texturing_Factor));
      Graphic_Value_Strings (12) := To_Unbounded_String
        (Integer'Image (Settings.Multi_Sample_Anti_Aliasing));
      Graphic_Value_Strings (13) := To_Unbounded_String
        (Single'Image (Settings.Super_Sample_Anti_Aliasing));
      Graphic_Value_Strings (14) := To_Unbounded_String
        (Integer'Image (Settings.Render_Distance));
      Graphic_Value_Strings (15) := To_Unbounded_String
        (Single'Image (Settings.Far_Clip));
      Graphic_Value_Strings (16) :=
        (Enabled_Strings (GL_Utils.To_Integer (Settings.Auto_Blood_Wipe) + 1));
      Graphic_Value_Strings (17) :=
        (Enabled_Strings (GL_Utils.To_Integer (Settings.Show_FPS) + 1));

   end Init_Graphic_Value_Strings;

   --  ------------------------------------------------------------------------

   procedure Init_Graphic_Text is
      use Menu_Strings;
      X1  : constant Single :=
              (-512.0 + 80.0) / Single (Settings.Framebuffer_Width);
      X2  : constant Single :=
              (512.0 - 330.0) / Single (Settings.Framebuffer_Width);
      Y   : constant Single :=
              760.0 / Single (Settings.Framebuffer_Height);
   begin
      for index in 1 .. Num_Graphic_Entries loop
         Graphics_Text (index) :=
           Text.Add_Text (Graphic_Strings (index), X1, Single (index + 1) * Y,
                          20.0, 1.0, 1.0, 1.0, 1.0);
         Text.Set_Text_Visible (Graphics_Text (index), False);

         Graphic_Value_Text (index) :=
           Text.Add_Text (To_String (Graphic_Value_Strings (index)), X2,
                          Single (index + 1) * Y, 20.0, 1.0, 1.0, 1.0, 1.0);
         Text.Set_Text_Visible (Graphic_Value_Text (index), False);
      end loop;

   end Init_Graphic_Text;

   --  ------------------------------------------------------------------------

   procedure Init_Input_Actions is
      use Menu_Strings;
      X1      : Single :=
                  (-512.0 + 80.0) / Single (Settings.Framebuffer_Width);
      X2      : Single :=
                  (512.0 - 465.0) / Single (Settings.Framebuffer_Width);
      Y       : Single :=
                  760.0 / Single (Settings.Framebuffer_Height);
      K_Index : Integer;
   begin
      for index in 1 .. Input_Handler.Num_Actions loop
         if To_String (Input_Handler.Action_Name (index)) /= "" then
            Cal_KB_Text (index) :=
              Text.Add_Text (To_String (Input_Handler.Action_Name (index)),
                             X1, Single (index + 1) * Y,
                             20.0, 1.0, 1.0, 1.0, 1.0);
            Text.Set_Text_Visible (Cal_KB_Text (index), False);

            Cal_GP_Text (index) :=
              Text.Add_Text (To_String (Input_Handler.Action_Name (index)),
                             X1, Single (index + 1) * Y,
                             20.0, 1.0, 1.0, 1.0, 1.0);
            Text.Set_Text_Visible (Cal_GP_Text (index), False);
         end if;

         K_Index := Input_Handler.Key_Binding (index);
         if K_Index < 0 or K_Index >= Input_Handler.Max_Keys then
            raise Mmenu_Exception with
              "Mmenu.Init_Input_Actions, invalid key code " &
              Integer'Image (K_Index) & " detected.";
         end if;

         if To_String (Input_Handler.Key_Name (index)) /= "" then
            KB_Binding_Text (index) :=
              Text.Add_Text (To_String (Input_Handler.Key_Name (index)),
                             X2, Single (index + 1) * Y,
                             20.0, 1.0, 1.0, 1.0, 1.0);
            Text.Set_Text_Visible (KB_Binding_Text (index), False);

            if Input_Handler.Joy_Axis_Bindings (index) < 0 or
              Input_Handler.Joy_Axis_Bindings (index) >= 8 then
               GP_Axis_Binding_Text (index) :=
                 Text.Add_Text ("none", X2, Single (index + 1) * Y,
                                20.0, 1.0, 1.0, 1.0, 1.0);
            else
               GP_Axis_Binding_Text (index) :=
                 Text.Add_Text (Input_Handler.Joy_Axis_Sign (index) & "AXIS" &
                                  Integer'Image (Input_Handler.Joy_Axis_Bindings (index)),
                                X2, Single (index + 1) * Y, 20.0, 1.0, 1.0, 1.0, 1.0);
            end if;
            Text.Set_Text_Visible (GP_Axis_Binding_Text (index), False);

            if Input_Handler.Joy_Button_Bindings (index) < 0 or
              Input_Handler.Joy_Button_Bindings (index) >= 32 then
               GP_Buttons_Binding_Text (index) :=
                 Text.Add_Text ("none", X2, Single (index + 1) * Y,
                                20.0, 1.0, 1.0, 1.0, 1.0);
            else
               GP_Buttons_Binding_Text (index) :=
                 Text.Add_Text
                   ("B" & Integer'Image (Input_Handler.Joy_Button_Bindings (index)),
                    X2, Single (index + 1) * Y, 20.0, 1.0, 1.0, 1.0, 1.0);
            end if;
            Text.Set_Text_Visible (Input_Handler.Joy_Button_Bindings (index), False);
         end if;
      end loop;

   end Init_Input_Actions;

   --  ------------------------------------------------------------------------

   procedure Init_Input_Text is
      use Menu_Strings;
      X  : constant Single :=
             (-512.0 + 80.0) / Single (Settings.Framebuffer_Width);
      Y  : constant Single :=
             760.0 / Single (Settings.Framebuffer_Height);
   begin
      for index in 1 .. Num_Input_Entries loop
         Input_Text (index) :=
           Text.Add_Text (Input_Strings (index), X, Single (index + 1) * Y,
                          20.0, 1.0, 1.0, 1.0, 1.0);
         Text.Set_Text_Visible (Graphics_Text (index), False);
      end loop;

   end Init_Input_Text;

   --  ------------------------------------------------------------------------

   procedure Init_Position_And_Texture_Buffers is
      use GL.Objects.Buffers;
      use GL.Types;
      use GL.Types.Singles;
      Menu_VAO        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Position_Buffer : Buffer;
      Texture_Buffer  : Buffer;
      Position_Array  : constant Vector2_Array (1 .. 6) :=
                          ((-1.0, 1.0), (-1.0, -1.0),  (1.0, -1.0),
                           (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0));
      Texture_Array   : constant Vector2_Array (1 .. 6) :=
                          ((0.0, 1.0), (0.0, 0.0),  (1.0, 0.0),
                           (1.0, 0.0), (1.0, 1.0), (0.0, 1.0));
   begin
      Position_Buffer := GL_Utils.Create_2D_VBO (Position_Array);
      Texture_Buffer := GL_Utils.Create_2D_VBO (Texture_Array);

      Menu_VAO.Initialize_Id;
      Menu_VAO.Bind;

      GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);
      Array_Buffer.Bind (Position_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Shader_Attributes.Attrib_VP, 2, Single_Type, False, 0, 0);

      GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);
      Array_Buffer.Bind (Texture_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Shader_Attributes.Attrib_VT, 2, Single_Type, False, 0, 0);

   end Init_Position_And_Texture_Buffers;

   --  ------------------------------------------------------------------------

   procedure Init_Quit_Text is
      use Menu_Strings;
      X        : constant Single :=
                   (512.0 - 330.0) / Single (Settings.Framebuffer_Width);
      Y1       : Single := 4.0 * 380.0 / Single (Settings.Framebuffer_Height);
      Y2       : Single := 40.0 / Single (Settings.Framebuffer_Height);
      ES_Index : Integer;
   begin
      if Settings.Disable_Joystick then
         ES_Index := 1;
      else
         ES_Index := 2;
      end if;
      --  only 1 input text in right hand column
      Input_Value_Text (1) :=
        Text.Add_Text (To_String (Enabled_Strings (ES_Index)), X, Y1,
                       20.0, 1.0, 1.0, 1.0, 1.0);
      Text.Set_Text_Visible (Input_Value_Text (1), False);

      for index in 1 .. Num_Quit_Entries loop
         Confirm_Quit_Text (index) :=
           Text.Add_Text (Graphic_Strings (index), X, Single (index) * Y2,
                          20.0, 1.0, 1.0, 1.0, 1.0);
         Text.Set_Text_Visible (Confirm_Quit_Text (index), False);
         Text.Centre_Text (Confirm_Quit_Text (index), 0.0, Y2);
      end loop;

   end Init_Quit_Text;

   --  ------------------------------------------------------------------------

   procedure Init_Title (Title_Mesh : out Integer) is
      use GL.Types.Singles;
      Camera_Position : Vector3 := (0.0, -6.5, 3.0);
      Camera_Target   : Vector3 :=
                          Camera_Position + (0.0, 1.0, -1.0);
      X               : constant Single := 319.0 / Single (Settings.Framebuffer_Width);
      Y               : Single := 19.0 / Single (Settings.Framebuffer_Height);
   begin
      Title_Mesh := Mesh_Loader.Load_Managed_Mesh
        ("src/meshes/3dtitle_idea.apg", True, True, False, False, False);
      Title_VAO.Initialize_Id;
      Title_Point_Count := Mesh_Loader.Point_Count (Title_Mesh);
      Title_Author_Text := Text.Add_Text ("a game by anton gerdelan",
                                          0.0, -0.4, 30.0, 0.75, 0.75, 0.75, 1.0);
      Text.Centre_Text (Title_Author_Text, 0.0, -0.8);
      Text.Set_Text_Visible (Title_Author_Text, False);

      Title_Buildstamp_Text := Text.Add_Text ("v1.4 (core)",
                                              X, Y, 10.0, 0.5, 0.5, 0.5, 1.0);
      Text.Set_Text_Visible (Title_Buildstamp_Text, False);

      Title_Version_Text := Text.Add_Text ("pre-release demo",
                                           0.0, -0.2, 20.0, 1.0, 1.0, 0.0, 1.0);
      Text.Centre_Text (Title_Version_Text, 0.0, -0.8);
      Text.Set_Text_Visible (Title_Version_Text, False);

      Title_Shader_Manager.Init (Title_Shader_Program);
      Maths.Init_Lookat_Transform (Camera_Position, Camera_Target,
                                   (0.0, 1.0, 0.0), Title_V);
      Title_M := Maths.Translation_Matrix ((-0.4, -3.0, -1.0));
      Title_M := Maths.Scaling_Matrix ((0.5, 0.5, 0.5)) * Title_M;
      Title_Shader_Manager.Set_Model_Matrix (Title_M);
      Title_Shader_Manager.Set_View_Matrix (Title_V);
      Title_Shader_Manager.Set_Perspective_Matrix (Camera.GUI_Proj_Matrix);

   end Init_Title;

   --  ------------------------------------------------------------------------

   procedure Init_Various is
      use Menu_Strings;
      X  : constant Single :=
             (-512.0 + 80.0) / Single (Settings.Framebuffer_Width);
      Y  : Single :=
             (-512.0 + 1500.0) / Single (Settings.Framebuffer_Height);
   begin

      Joystick_Detected_Text  :=
        Text.Add_Text ("joystick detected: " & To_String (Joy_Name) & CRLF,
                       X,  Y, 20.0, 1.0, 1.0, 1.0, 1.0);
      Text.Set_Text_Visible (Joystick_Detected_Text, False);

      Greatest_Text_Axis  :=
        Text.Add_Text ("axis: ", X,  Y,
                       20.0, 1.0, 1.0, 0.0, 1.0);
      Text.Set_Text_Visible (Greatest_Text_Axis, False);

      Restart_Graphics_Text  :=
        Text.Add_Text ("the game must be restarted" & CRLF &
                         "for some changes to be applied", X,  Y,
                       20.0, 1.0, 1.0, 0.0, 1.0);
      Text.Set_Text_Visible (Restart_Graphics_Text, False);

      Y := (-512.0 + 300.0) / Single (Settings.Framebuffer_Height);
      Already_Bound_Text  :=
        Text.Add_Text ("key is already bound!", X,  Y,
                       20.0, 1.0, 1.0, 0.0, 1.0);
      Text.Set_Text_Visible (Already_Bound_Text, False);

      for index in 1 .. Num_Input_Entries loop
         Input_Text (index) :=
           Text.Add_Text (Input_Strings (index), X, Single (index + 1) * Y,
                          20.0, 1.0, 1.0, 1.0, 1.0);
         Text.Set_Text_Visible (Graphics_Text (index), False);
      end loop;

   end Init_Various;

   --  ------------------------------------------------------------------------

   function Menu_Open return Boolean is
   begin
      return MMenu_Open;
   end Menu_Open;

   --  ------------------------------------------------------------------------

   function Menu_Was_Closed return Boolean is
   begin
      return Mmenu_Was_Closed;
   end Menu_Was_Closed;

   --  ------------------------------------------------------------------------

   procedure Set_MMenu_Open (State : Boolean) is
   begin
      MMenu_Open := State;
   end Set_MMenu_Open;

   --  ------------------------------------------------------------------------

   procedure Start_Mmenu_Title_Bounce is
   begin
      Title_Bounce_Timer := 0.0;
   end Start_Mmenu_Title_Bounce;

   --  ------------------------------------------------------------------------

   function Update_MMenu (Delta_Time : Float) return Boolean is
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
      Mmenu_Was_Closed := False;
      User_Chose_Custom_Maps := False;
      User_Chose_New_Game := False;
      --  Joystick processsing
      Result := Since_Last_Key < 0.15;
      if not Result then
         Result := Mmenu_Gr_Open;
         if Result then
            Result := Was_Key_Pressed (Escape) or
              Was_Action_Pressed (Open_Menu_Action) or
              Was_Action_Pressed (Menu_Back_Action);
            if Result then
               Mmenu_Gr_Open := False;
            else
               Result := Was_Key_Pressed (Enter) or
                 Was_Action_Pressed (OK_Action) or
                 Was_Action_Pressed (Attack_Action);
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
   end Update_MMenu;

   --  ------------------------------------------------------------------------

end MMenu;
