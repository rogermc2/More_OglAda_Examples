
with Glfw.Input.Keys;

with GL.Attributes;
with GL.Objects.Textures.Targets;
with GL.Toggles;
with GL.Types.Colors;

with Glfw;

with Maths;
with Utilities;

with Camera;
with Cursor_Shader_Manager;
with Game_Utils;
with GL_Utils;
with Input_Handler;
with Menu_Credits_Shader_Manager;
with Menu_Strings;
with Mesh_Loader;
with Settings;
with Shader_Attributes;
with Text;
with Texture_Manager;
with Title_Shader_Manager;

package body MMenu_Initialization is
   use GL.Types;
   use Menu_Strings;

   Menu_Text_Yoffs    : constant Single := 300.0; -- pixels above horizontal
   --  for text to start
   Menu_Big_Text_Sz   : constant Single := 80.0;  -- height of subseq lines to
   --  offset below that
   CRLF               : constant String := ASCII.LF & ASCII.CR;
   CRLF2              : constant String := CRLF & CRLF;
   CRLF3              : constant String := CRLF2 & CRLF;
   CRLF4              : constant String := CRLF2 & CRLF2;


   Credits_String   : constant String :=
                        "ANTON GERDELAN" & CRLF2 &
                        "       DAVID ERMAN" & CRLF2 &
                        "       ANDREA DOMENICHINI" & CRLF2 &
                        "     ROMEO DOMENICHINI" & CRLF2 &
                        " ERIK WADSTEIN" & CRLF4 &
                        "WARLOCK SYMPHONY" & CRLF &
                        "written and performed by" & CRLF &
                        "HORNANVASARA" & CRLF &
                        "licenced courtesy of devil creations" & CRLF2 &
                        "PROTAGONIST THROUGH PAIN" & CRLF &
                        "written and performed by" & CRLF &
                        "HORNANVASARA" & CRLF &
                        "licenced courtesy of devil creations" & CRLF2 &
                        "WAR DRUMS" & CRLF &
                        "licenced courtesy of" & CRLF &
                        "partners in rhyme" & CRLF4 &
                        "crongdor the barbarian was" & CRLF &
                        "originally programmed in c++ with" & CRLF &
                        "OPENGL (graphics) and" & CRLF &
                        "IRRKLANG (audio) libraries." & CRLF2 &
                        "all of the major programme" & CRLF &
                        "components were written by hand" & CRLF &
                        "in ANTON's after-work hours" & CRLF &
                        "over 4~6 years. no engine or" & CRLF &
                        "framework was used because it's" & CRLF &
                        "more fun to write it yourself!" & CRLF2 &
                        "ERIK teaches technical artists" & CRLF &
                        "in sweden, and painted concept" & CRLF &
                        "art and made some game elements." & CRLF2 &
                        "ROMEO is a drama student and" & CRLF &
                        "produced the promotional video." & CRLF2 &
                        "ANDREA has been supporting" & CRLF &
                        "development and making some elements" & CRLF &
                        "since the first prototype, 6 years" & CRLF &
                        "ago." & CRLF2 &
                        "DAVID ported the first version to" & CRLF &
                        "OS X and recorded some of the sound" & CRLF &
                        "effects." & CRLF2 &
                        "Special thanks to EMMA CARRIGAN" & CRLF &
                        "for extensive pre-release testing" & CRLF2 &
                        "BLENDER was used to build the" & CRLF &
                        "3D models, and GIMP was used to" & CRLF &
                        "draw all of the art with a" & CRLF &
                        "WACOM tablet. AUDACITY was used" & CRLF &
                        "for audio work." & CRLF2 &
                        "UNIVERSAL SOUND EFFECTS library" & CRLF &
                        "is used under licence." & CRLF2;

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

   Credits_Text_X             : constant Single := 0.0;
   Credits_Text_Y             : constant Single := -1.0;
   Title_Version_Text         : Integer := -1;
   Joystick_Detected_Text     : Integer := -1;
   Greatest_Text_Axis         : Integer := -1;
   Restart_Graphics_Text      : Integer := -1;
   Already_Bound_Text         : Integer := -1;

   Menu_Cursor_Curr_Item   : Integer := -1;
   Cursor_Current_Item     : Integer := -1;
   Cursor_V                : Singles.Matrix4 := GL.Types.Singles.Identity4;

   --  ------------------------------------------------------------------------

   procedure Init1 (Menu_Text           : in out GL_Maths.Integer_Array;
                    End_Story_Text      :in out Integer;
                    Text_Background_Texture, Menu_Credits_Texture,
                    Title_Skull_Texture, Menu_Cursor_Texture :
                    in out GL.Objects.Textures.Texture) is
      use GL.Types;
      use GL.Types.Singles;
      X               : constant Single := 319.0 / Single (Settings.Framebuffer_Width);
      Y               : Single := 19.0 / Single (Settings.Framebuffer_Height);
      Menu_Colour     : constant Singles.Vector4 := (1.0, 1.0, 1.0, 1.0);
   begin
      End_Story_Text := Text.Add_Text (End_Story_String, Credits_Text_X,
                                       Credits_Text_Y, 30.0, 1.0, 1.0, 0.1, 1.0);
      Text.Set_Text_Visible (End_Story_Text, False);

      Texture_Manager.Load_Image_To_Texture
        ("src/textures/skull_small_helmet_painterv_shade.png", Menu_Cursor_Texture, False, True);
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/title_skull.png", Title_Skull_Texture, False, True);
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/victory.png", Menu_Credits_Texture, False, True);
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/text_bkrnd.png", Text_Background_Texture, False, True);

      for index in 1 .. Num_Menu_Entries loop
         Y := Menu_Text_Yoffs - Menu_Big_Text_Sz * Single (index - 1) /
           Single (Settings.Framebuffer_Height);
         Menu_Text (index) :=
           Text.Add_Text (Menu_Strings.Menu_String_Items (index),
                          0.0, Y, 40.0, 1.0, 1.0, 1.0, 1.0);
         Text.Centre_Text (Menu_Text (index), 0.0, Y);
         Text.Set_Text_Visible (Menu_Text (index), False);
      end loop;
   end Init1;

   --  ------------------------------------------------------------------------

   procedure Init_Audio_Value_Strings
     (Audio_Text, Audio_Value_Text : in out GL_Maths.Integer_Array) is
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
            Text.Set_Text_Visible (Audio_Text (index), False);

            Audio_Value_Text (index) :=
              Text.Add_Text (To_String (Audio_Value_Strings (index)), X2,
                             Single (index + 1) * Y, 20.0, 1.0, 1.0, 1.0, 1.0);
            Text.Set_Text_Visible (Audio_Text (index), False);
         end if;
      end loop;

   end Init_Audio_Value_Strings;

   --  ------------------------------------------------------------------------

   procedure Init_Credits
     (Credits_Shader_Program : in out GL.Objects.Programs.Program;
      Text_Background_Scale, Text_Background_Pos : in out Singles.Vector2;
      Credits_Text_ID        : in out Integer) is
      use GL.Objects.Programs;
      use GL.Types;
      use Menu_Credits_Shader_Manager;
      use Settings;
      FB_Width       : constant Single := Single (Settings.Framebuffer_Width);
      FB_Height      : constant Single := Single (Settings.Framebuffer_Height);
      Scale          : Single := 2048.0;
      Credits_S      : Singles.Vector2;
      Credits_P      : constant Singles.Vector2 := (0.0, 0.0);
      Credits_Text_X : constant Single := -715.0 / FB_Width;
   begin
      Menu_Credits_Shader_Manager.Init (Credits_Shader_Program);
      if Framebuffer_Width < 1024 or Framebuffer_Height < 1024 then
         Scale := 512.0;
      elsif Framebuffer_Width < 2048 or Framebuffer_Height < 2048 then
         Scale := 1024.0;
      end if;
      Credits_S := (Scale / FB_Width, Scale / FB_Height);
      Use_Program (Credits_Shader_Program);
      Set_Scale (Credits_S);
      Set_Position (Credits_P);
      Text_Background_Scale := (512.0 / FB_Width, 400.0 / FB_Height);
      Text_Background_Pos := Credits_P;
      Credits_Text_ID := Text.Add_Text (Credits_String, Credits_Text_X,
                                        Credits_Text_Y, 30.0, 1.0, 1.0, 1.0, 1.0);
      Text.Set_Text_Visible (Credits_Text_ID, False);
   end Init_Credits;

   --  --------------------------- ---------------------------------------------

   procedure Init_Cursor
     (Cursor_Shader_Program : in out GL.Objects.Programs.Program;
      Cursor_VAO            : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Cursor_M              : in out GL.Types.Singles.Matrix4;
      Cursor_Point_Count    : in out Integer) is
      Camera_Position       : constant Singles.Vector3 := (0.0, 0.0, 10.0);
      Camera_Target         : constant Singles.Vector3 := (0.0, 0.0, 0.0);
      Cursor_V              : Singles.Matrix4;
      Cursor_Mesh_ID        : Integer := -1;
   begin
      Cursor_M := Singles.Identity4;
      Maths.Init_Lookat_Transform (Camera_Position, Camera_Target,
                                   (0.0, 1.0, 0.0), Cursor_V);
      Cursor_Shader_Manager.Init (Cursor_Shader_Program);
      GL.Objects.Programs.Use_Program (Cursor_Shader_Program);
      Cursor_Shader_Manager.Set_Model_Matrix (Cursor_M);
      Cursor_Shader_Manager.Set_View_Matrix (Cursor_V);
      Cursor_Shader_Manager.Set_Perspective_Matrix (Camera.GUI_Proj_Matrix);

      Cursor_Mesh_ID := Mesh_Loader.Load_Managed_Mesh
        ("src/meshes/skull_helmet.apg", True, True, True, False, False);
      if Cursor_Mesh_ID <= 0 then
         raise MMenu_Exception with
           "MMenu.Init_Cursor Load_Managed_Mesh failed to load src/meshes/skull_helmet.apg";
         --  Save Cursor_VAO
      elsif not Mesh_Loader.Loaded_Mesh_VAO (Cursor_Mesh_ID, Cursor_VAO) then
         raise MMenu_Exception with
           "MMenu.Init_Cursor failed to initialize VAO for Cursor_Mesh";
      end if;

      Cursor_Point_Count := Mesh_Loader.Point_Count (Cursor_Mesh_ID);
   end Init_Cursor;

   --  ------------------------------------------------------------------------

   procedure Init_Graphic_Value_Strings (Enabled_Strings, Graphic_Value_Strings :
                                         in out Menu_String_Array) is
      Graphic_Int : Integer;
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
        (Integer'Image (Settings.Anisotroic_Texturing_Factor));
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

   procedure Init_Graphic_Text
     (Graphics_Text, Graphic_Value_Text : in out GL_Maths.Integer_Array;
      Graphic_Value_Strings             : in out Menu_String_Array) is
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

   procedure Init_Input_Actions
     (Cal_KB_Text, Cal_GP_Text, KB_Binding_Text, GP_Axis_Binding_Text,
      GP_Buttons_Binding_Text : in out GL_Maths.Integer_Array) is
      X1      : constant Single :=
                  (-512.0 + 80.0) / Single (Settings.Framebuffer_Width);
      X2      : constant Single :=
                  (512.0 - 465.0) / Single (Settings.Framebuffer_Width);
      Y       : constant Single :=
                  760.0 / Single (Settings.Framebuffer_Height);
      K_Index : Integer;
   begin
      for index in 1 .. Input_Handler.Num_Actions loop
         if Input_Handler.Action_Name (index) /= "" then
            Cal_KB_Text (index) :=
              Text.Add_Text (Input_Handler.Action_Name (index),
                             X1, Single (index + 1) * Y,
                             20.0, 1.0, 1.0, 1.0, 1.0);
            Text.Set_Text_Visible (Cal_KB_Text (index), False);

            Cal_GP_Text (index) :=
              Text.Add_Text (Input_Handler.Action_Name (index),
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

   procedure Init_Input_Text (Input_Text : in out GL_Maths.Integer_Array) is
      X  : constant Single :=
             (-512.0 + 80.0) / Single (Settings.Framebuffer_Width);
      Y  : constant Single :=
             760.0 / Single (Settings.Framebuffer_Height);
   begin
      for index in 1 .. Num_Input_Entries loop
         Input_Text (index) :=
           Text.Add_Text (Input_Strings (index), X, Single (index + 1) * Y,
                          20.0, 1.0, 1.0, 1.0, 1.0);
         Text.Set_Text_Visible (Input_Text (index), False);
      end loop;

   end Init_Input_Text;

   --  --------------------------- ---------------------------------------------

   procedure Init_Position_And_Texture_Buffers
     (Menu_VAO                        : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Position_Buffer, Texture_Buffer : in out GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;
      use GL.Types;
      use GL.Types.Singles;
      Position_Array  : constant Vector2_Array (1 .. 6) :=
                          ((-1.0, 1.0), (-1.0, -1.0),  (1.0, -1.0),
                           (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0));
      Texture_Array   : constant Vector2_Array (1 .. 6) :=
                          ((0.0, 1.0), (0.0, 0.0),  (1.0, 0.0),
                           (1.0, 0.0), (1.0, 1.0), (0.0, 1.0));
   begin
      --  Each attribute stated in a Vertex Array Objects state vector may
      --  refer to a different Vertex Buffer Object.
      --  This reference is stored when Set_Vertex_Attrib_Pointer is called;
      --  the buffer which is currently bound to the target ARRAY_BUFFER is
      --  associated to the attribute and the name (value) of the object is
      --  stored in the state vector of the VAO.
      --  The ARRAY_BUFFER binding is a global state.
      Position_Buffer := GL_Utils.Create_2D_VBO (Position_Array);
      Texture_Buffer := GL_Utils.Create_2D_VBO (Texture_Array);

      Menu_VAO.Initialize_Id;
      Menu_VAO.Bind;

      Array_Buffer.Bind (Position_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Shader_Attributes.Attrib_VP, 2, Single_Type, False, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

      Array_Buffer.Bind (Texture_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Shader_Attributes.Attrib_VT, 2, Single_Type, False, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);

   end Init_Position_And_Texture_Buffers;

   --  ------------------------------------------------------------------------

   procedure Init_Quit_Text
     (Input_Value_Text, Confirm_Quit_Text : in out GL_Maths.Integer_Array;
      Enabled_Strings                     : in out Menu_String_Array) is
      X        : constant Single :=
                   (512.0 - 330.0) / Single (Settings.Framebuffer_Width);
      Y1       : constant Single := 4.0 * 380.0 / Single (Settings.Framebuffer_Height);
      Y2       : constant Single := 40.0 / Single (Settings.Framebuffer_Height);
      ES_Index : Integer;
   begin
      if Settings.Joystick_Disabled then
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

   procedure Init_Title
     (Title_Author_Text, Title_Buildstamp_Text : in out Integer;
      Title_M, Title_V                         : in out GL.Types.Singles.Matrix4;
      Title_Shader_Program                     : in out GL.Objects.Programs.Program;
      Title_VAO                                : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Title_Point_Count                        : in out Integer) is
      use GL.Types.Singles;
      Title_Mesh_ID   : Integer := -1;
      Camera_Position : constant Vector3 := (0.0, -6.5, 3.0);
      Camera_Target   : constant Vector3 := Camera_Position + (0.0, 1.0, -1.0);
      X               : constant Single := 400.0 / Single (Settings.Framebuffer_Width);
      Y               : constant Single := 40.0 / Single (Settings.Framebuffer_Height);
   begin
      Title_Mesh_ID := Mesh_Loader.Load_Managed_Mesh
        ("src/meshes/3dtitle_idea.apg", True, True, False, False, False);
      if Title_Mesh_ID <= 0 then
         raise MMenu_Exception with
           "MMenu.Init_Cursor Load_Managed_Mesh failed to load src/meshes/3dtitle_idea.apg";
         --  Save Title_VAO
      elsif not Mesh_Loader.Loaded_Mesh_VAO (Title_Mesh_ID, Title_VAO) then
         raise MMenu_Exception with
           "MMenu.Init_Title failed to initialize VAO for Title_Mesh";
      end if;
      Title_Point_Count := Mesh_Loader.Point_Count (Title_Mesh_ID);

      Title_Author_Text := Text.Add_Text ("a game by anton gerdelan",
                                          0.0, -0.4, 30.0, 0.75, 0.75, 0.75, 1.0);
      Text.Centre_Text (Title_Author_Text, 0.0, -0.8);
      Text.Set_Text_Visible (Title_Author_Text, False);

      Title_Buildstamp_Text := Text.Add_Text ("Ada v1.0 (alpha)",
                                              X, Y, 10.0, 0.5, 0.5, 0.5, 1.0);
      --        Text.Centre_Text (Title_Buildstamp_Text, 0.0, 0.0);
      Text.Set_Text_Visible (Title_Buildstamp_Text, False);

      Title_Version_Text := Text.Add_Text ("pre-release demo",
                                           0.0, -0.2, 20.0, 1.0, 1.0, 0.0, 1.0);
      Text.Centre_Text (Title_Version_Text, 0.0, -0.8);
      Text.Set_Text_Visible (Title_Version_Text, False);

      Title_Shader_Manager.Init (Title_Shader_Program);
      Maths.Init_Lookat_Transform (Camera_Position, Camera_Target,
                                   (0.0, 1.0, 0.0), Title_V);

      Utilities.Print_Matrix ("Initialize Title_V", Title_V);
      Title_M := Maths.Translation_Matrix ((-0.4, -3.0, -1.0)) * Identity4;
      Title_M := Maths.Scaling_Matrix ((0.5, 0.5, 0.5)) * Title_M;
      Utilities.Print_Matrix ("Initialize Title_M", Title_M);

      GL.Objects.Programs.Use_Program (Title_Shader_Program);
      Title_Shader_Manager.Set_Model_Matrix (Title_M);
      Title_Shader_Manager.Set_View_Matrix (Title_V);
      Title_Shader_Manager.Set_Perspective_Matrix (Camera.GUI_Proj_Matrix);

   end Init_Title;

   --  ------------------------------------------------------------------------

   procedure Init_Various
     (Input_Text : in out GL_Maths.Integer_Array; Joy_Name : String) is
      X  : constant Single :=
             (-512.0 + 80.0) / Single (Settings.Framebuffer_Width);
      Y  : Single :=
             (-512.0 + 1500.0) / Single (Settings.Framebuffer_Height);
   begin
      Joystick_Detected_Text  :=
        Text.Add_Text ("joystick detected: " & Joy_Name & CRLF,
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
         Text.Set_Text_Visible (Input_Text (index), False);
      end loop;

   end Init_Various;

   --  ------------------------------------------------------------------------

end MMenu_Initialization;
