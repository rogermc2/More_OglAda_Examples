
with Ada.Text_IO; use Ada.Text_IO;

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
with Input_Callback;
with Input_Handler;
with Menu_Credits_Shader_Manager;
with Mesh_Loader;
with Settings;
with Shader_Attributes;
with Text;
with Texture_Manager;
with Title_Shader_Manager;

package body Main_Menu_Initialization is
    use GL.Types;
    use Menu_Strings;

    White              : constant Colors.Color := (1.0, 1.0, 1.0, 1.0);
    Menu_Text_Yoffs    : constant Single := 300.0; -- pixels above horizontal
    --  for text to start
    Menu_Big_Text_Sz   : constant Single := 80.0;  -- height of subseq lines to
    --  offset below that
    CRLF               : constant Character := Character'Val (13);
    --     CRLF2              : constant String := CRLF & CRLF;
    --     CRLF3              : constant String := CRLF2 & CRLF;
    --     CRLF4              : constant String := CRLF2 & CRLF2;

    Credits_Strings    : constant array (1 .. Num_Credits_Strings) of
      String (1 .. 36) :=
                           ("ANTON GERDELAN                      ",
                            "DAVID ERMAN                         ",
                            "ANDREA DOMENICHINI                  ",
                            "ROMEO DOMENICHINI                   ",
                            "ERIK WADSTEIN                       ",
                            "WARLOCK SYMPHONY                    ",
                            "written and performed by            ",
                            "HORNANVASARA                        ",
                            "licenced courtesy of devil creations",
                            "PROTAGONIST THROUGH PAIN            ",
                            "written and performed by            ",
                            "HORNANVASARA                        ",
                            "Licenced courtesy of Devil Creations",
                            "WAR DRUMS                           ",
                            "licenced courtesy of                ",
                            "Partners In Rhyme                   ",
                            "crongdor the barbarian was          ",
                            "originally programmed in c++ with   ",
                            "OPENGL (graphics) and               ",
                            "IRRKLANG (audio) libraries.         ",
                            "all of the major program            ",
                            "components were written by hand     ",
                            "in ANTON's after-work hours         ",
                            "over 4~6 years. No engine or        ",
                            "framework was used because it's     ",
                            "more fun to write it yourself!      ",
                            "ERIK teaches technical artists      ",
                            "in sweden, and painted concept      ",
                            "art and made some game elements.    ",
                            "ROMEO is a drama student and        ",
                            "produced the promotional video.     ",
                            "ANDREA has been supporting          ",
                            "development and making some elements",
                            "since the first prototype, 6 years  ",
                            "ago.                                ",
                            "DAVID ported the first version to   ",
                            "OS X and recorded some of the sound ",
                            "effects.                            ",
                            "Special thanks to EMMA CARRIGAN     ",
                            "for extensive pre-release testing   ",
                            "BLENDER was used to build the       ",
                            "3D models, and GIMP was used to     ",
                            "draw all of the art with a          ",
                            "WACOM tablet. AUDACITY was used     ",
                            "for audio work.                     ",
                            "UNIVERSAL SOUND EFFECTS library     ",
                            "is used under licence.              ");

    type End_Story_String_Array is array (1 .. Num_End_Story_Strings) of String (1 .. 38);
    End_Story_Strings : constant End_Story_String_Array :=
                          ("Crongdor glanced back at the temple   ",
                           "through the shadowy palm groves. a    ",
                           "power, its time long gone, would not  ",
                           "rise again. the place of his barbarian",
                           "people assured for now.               ",
                           "his friend the merchant was waiting   ",
                           "with two camels. the merchant eyed    ",
                           "the crown in crongdor's hand.         ",
                           "you are rich! what will you do now?   ",
                           "HAH! he said, running his fingers     ",
                           "through his blood-matted mane.        ",
                           "with this i will hire a company of    ",
                           "men to PLUNDER THE WORLD!             ",
                           "but first, there is a tavern i know   ",
                           "nearby where the meat is good, and    ",
                           "the drink is strong. gods know, I     ",
                           "have earned it today!                 ",
                           "COME!                                 ");

    Title_Version_Text      : Integer := -1;
    Restart_Graphics_Text   : Integer := -1;

    Menu_Cursor_Curr_Item   : Integer := -1;
    Cursor_Current_Item     : Integer := -1;

    --  ------------------------------------------------------------------------

    procedure Init1 (End_Story_Text                           : in out End_Story_Array;
                     Credits_Text_Pos                         : GL.Types.Singles.Vector2;
                     Text_Background_Texture, Menu_Credits_Texture,
                     Title_Skull_Texture, Menu_Cursor_Texture :
                     in out GL.Objects.Textures.Texture) is
        use GL.Types;
        use GL.Types.Singles;
        X               : constant Single := 319.0 / Single (Settings.Framebuffer_Width);
        Y               : Single := 19.0 / Single (Settings.Framebuffer_Height);
        Menu_Colour     : constant Singles.Vector4 := (1.0, 1.0, 1.0, 1.0);
    begin
        for index in End_Story_Array'Range loop
            End_Story_Text (index) := Text.Add_Text (End_Story_Strings (index),
                                                     Credits_Text_Pos (GL.X),
                                                     Credits_Text_Pos (GL.Y) + 10.0 * Single (index - 1),
                                                     30.0, (1.0, 1.0, 0.1, 1.0));
            Text.Set_Text_Visible (End_Story_Text (index), False);
        end loop;

        Texture_Manager.Load_Image_To_Texture
          ("src/textures/skull_small_helmet_painterv_shade.png", Menu_Cursor_Texture, False, True);
        Texture_Manager.Load_Image_To_Texture
          ("src/textures/title_skull.png", Title_Skull_Texture, False, True);
        Texture_Manager.Load_Image_To_Texture
          ("src/textures/victory.png", Menu_Credits_Texture, False, True);
        Texture_Manager.Load_Image_To_Texture
          ("src/textures/text_bkrnd.png", Text_Background_Texture, False, True);
    end Init1;

    --  ------------------------------------------------------------------------

    procedure Init_Audio_Value_Strings
      (Audio_Text       : in out Audio_Text_Array;
       Audio_Value_Text : in out Audio_Text_Array) is
        use Settings;
        use Menu_Strings;
        Audio_Value_Strings : array (Audio_Choice_Type'Range) of Unbounded_String
          := (others => To_Unbounded_String (""));
        X1                  : constant Single :=
                                (-512.0 + 80.0) / Single (Framebuffer_Width);
        X2                  : constant Single :=
                                (512.0 - 330.0) / Single (Framebuffer_Width);
        Y                   : constant Single :=
                                760.0 / Single (Framebuffer_Height);
        Y_Step              : Single := 0.0;
    begin
        --        Audio_Value_Strings (Audio_Strings_Audio_Device) := To_Unbounded_String (Get_Audio_Device_Name);
        Audio_Value_Strings (Audio_Strings_Master_Volume) := GL_Utils.To_UB_String (10 * Audio_Volume);
        Audio_Value_Strings (Audio_Strings_Music_Volume) := GL_Utils.To_UB_String (10 * Music_Volume);

        for index in Audio_Choice_Type'Range loop
            Y_Step := Y_Step + 1.0;
            if Audio_Value_Strings (index) /= "" then
                Audio_Text (index) :=
                  Text.Add_Text (Audio_Strings (index), X1, Y_Step * Y,
                                 20.0,  (1.0, 1.0, 1.0, 1.0));
                Text.Set_Text_Visible (Audio_Text (index), False);

                Audio_Value_Text (index) :=
                  Text.Add_Text (To_String (Audio_Value_Strings (index)), X2,
                                 Y_Step * Y, 20.0,  (1.0, 1.0, 1.0, 1.0));
                Text.Set_Text_Visible (Audio_Text (index), False);
            end if;
        end loop;

    end Init_Audio_Value_Strings;

    --  ------------------------------------------------------------------------

    procedure Init_Credits
      (Credits_Shader_Program                       : in out GL.Objects.Programs.Program;
       Text_Background_Scale                        : in out Singles.Vector2;
       Credits_Text_ID                              : in out Credits_Text_Array;
       Credits_Scale, Credits_Pos, Credits_Text_Pos : in out
         GL.Types.Singles.Vector2) is
        use GL.Objects.Programs;
        use GL.Types;
        use Menu_Credits_Shader_Manager;
        use Settings;
        FB_Width       : constant Single := Single (Settings.Framebuffer_Width);
        FB_Height      : constant Single := Single (Settings.Framebuffer_Height);
        Scale          : Single := 2048.0;
    begin
        Menu_Credits_Shader_Manager.Init (Credits_Shader_Program);
        if Framebuffer_Width < 1024 or Framebuffer_Height < 1024 then
            Scale := 512.0;
        elsif Framebuffer_Width < 2048 or Framebuffer_Height < 2048 then
            Scale := 1024.0;
        end if;

        Credits_Scale := (Scale / FB_Width, Scale / FB_Height);
        Credits_Pos := (0.0, 0.2);
        Credits_Text_Pos := (-0.9, -0.9);
        Use_Program (Credits_Shader_Program);
        Set_Scale (Credits_Scale);
        Set_Position (Credits_Pos);
        Text_Background_Scale := (512.0 / FB_Width, 400.0 / FB_Height);
        for index in Credits_Strings'Range loop
            Credits_Text_ID (index) :=
              Text.Add_Text (Credits_Strings (index), Credits_Text_Pos (GL.X),
                             Credits_Text_Pos (GL.Y),
                             20.0,  (1.0, 1.0, 1.0, 1.0));  --  orig pixels 30.0
            Text.Set_Text_Visible (Credits_Text_ID (index), False);
        end loop;
    end Init_Credits;

    --  --------------------------- ---------------------------------------------

    procedure Init_Cursor
      (Cursor_Shader_Program : in out GL.Objects.Programs.Program;
       Cursor_VAO            : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
       Cursor_M, Cursor_V    : in out GL.Types.Singles.Matrix4;
       Cursor_Point_Count    : in out Integer) is
        Cursor_Mesh_ID        : Integer := -1;
    begin
        Cursor_Shader_Manager.Init (Cursor_Shader_Program);
        Cursor_M := Singles.Identity4;
        Maths.Init_Lookat_Transform ((0.0, 0.0, 10.0), (0.0, 0.0, 0.0),
                                     (0.0, 1.0, 0.0), Cursor_V);
        GL.Objects.Programs.Use_Program (Cursor_Shader_Program);
        Cursor_Shader_Manager.Set_Model_Matrix (Cursor_M);
        Cursor_Shader_Manager.Set_View_Matrix (Cursor_V);
        Cursor_Shader_Manager.Set_Perspective_Matrix (Camera.GUI_Proj_Matrix);

        Cursor_Mesh_ID := Mesh_Loader.Load_Managed_Mesh
          ("src/meshes/skull_helmet.apg", True, True, True, False, False);
        if Cursor_Mesh_ID <= 0 then
            raise MMenu_Exception with
              "Main_Menu_Initialization.Init_Cursor Load_Managed_Mesh failed to load src/meshes/skull_helmet.apg";
            --  Save Cursor_VAO
        elsif not Mesh_Loader.Loaded_Mesh_VAO (Cursor_Mesh_ID, Cursor_VAO) then
            raise MMenu_Exception with
              "Main_Menu_Initialization.Init_Cursor failed to initialize VAO for Cursor_Mesh";
        end if;

        Cursor_Point_Count := Mesh_Loader.Point_Count (Cursor_Mesh_ID);

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Menu_Initialization.Init_Cursor.");
            raise;

    end Init_Cursor;

    --  ------------------------------------------------------------------------

    procedure Init_Graphic_Value_Strings (Enabled_Strings       : in out Menu_String_Array;
                                          Graphic_Value_Strings : in out Graphic_Value_String_Array) is
        use GL_Utils;
        use Settings;
    begin
        Graphic_Value_Strings (Graphic_Presets) :=
          To_Unbounded_String (Graphic_Preset_Strings (Graphic_Preset_Dire));
        Graphic_Value_Strings (Graphic_Opengl_Version) := To_Unbounded_String ("3.2");
        Graphic_Value_Strings (Graphic_Windowed_Size) := To_Unbounded_String
          (Integer'Image (Window_Width_To_Save) & 'x' &
             Integer'Image (Window_Height_To_Save));
        Graphic_Value_Strings (Graphic_Full_Screen) := To_UB_String (Full_Screen);
        Graphic_Value_Strings (Graphic_Vsync) := To_UB_String (V_Sync);
        Graphic_Value_Strings (Graphic_Shadows) := To_UB_String (Shadows_Enabled);
        Graphic_Value_Strings (Graphic_Shadow_Size) := To_Unbounded_String
          (Integer'Image (Shadows_Size));
        Graphic_Value_Strings (Graphic_Outlines) := To_UB_String (Render_OLS);
        Graphic_Value_Strings (Graphic_Framebuffer_Fx) :=
          To_UB_String (Fb_Effects_Enabled);
        Graphic_Value_Strings (Graphic_Texture_Filter) := To_Unbounded_String
          (Integer'Image (Texture_Filter));
        Graphic_Value_Strings (Graphic_Anisotropy) := To_Unbounded_String
          (Integer'Image (Anisotroic_Texturing_Factor));
        Graphic_Value_Strings (Graphic_Msaa) := To_Unbounded_String
          (Integer'Image (Multi_Sample_Anti_Aliasing));
        Graphic_Value_Strings (Graphic_Ssaa) := To_Unbounded_String
          (Single'Image (Super_Sample_Anti_Aliasing));
        Graphic_Value_Strings (Graphic_Render_Dist) := To_Unbounded_String
          (Integer'Image (Render_Distance));
        Graphic_Value_Strings (Graphic_Far_Clip) := To_Unbounded_String
          (Single'Image (Far_Clip));
        Graphic_Value_Strings (Graphic_Auto_Blood_Wipe) :=
          Enabled_Strings (GL_Utils.To_Integer (Auto_Blood_Wipe) + 1);
        Graphic_Value_Strings (Graphic_Show_Fps) :=
          Enabled_Strings (GL_Utils.To_Integer (Show_FPS) + 1);

    end Init_Graphic_Value_Strings;

    --  ------------------------------------------------------------------------

    procedure Init_Graphic_Text
      (Graphics_Text, Graphic_Value_Text : in out Graphic_Value_Array;
       Graphic_Value_Strings             : Graphic_Value_String_Array) is
        X1     : constant Single :=
                   (-512.0 + 80.0) / Single (Settings.Framebuffer_Width);
        X2     : constant Single :=
                   (512.0 - 400.0) / Single (Settings.Framebuffer_Width);
        Y      :  Single;
    begin
        for index in Graphic_Choice_Type'Range loop
            Y := 40.0 * (10.0 - Single (index'Enum_Rep + 2)) /
              Single (Settings.Framebuffer_Height);
            Graphics_Text (index) :=
              Text.Add_Text (Graphic_Strings (index), X1, Y,
                             20.0, White);
            Text.Set_Text_Visible (Graphics_Text (index), False);

            Graphic_Value_Text (index) :=
              Text.Add_Text (To_String (Graphic_Value_Strings (index)),
                             X2, Y, 20.0, White);
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
        Y_Step  : Single := 0.0;
    begin
        for index in 1 .. Input_Handler.Num_Actions loop
            Y_Step := Y_Step + 1.0;
            if Input_Handler.Action_Name (index) /= "" then
                Cal_KB_Text (index) :=
                  Text.Add_Text (Input_Handler.Action_Name (index),
                                 X1, Y_Step * Y, 20.0, White);
                Text.Set_Text_Visible (Cal_KB_Text (index), False);

                Cal_GP_Text (index) :=
                  Text.Add_Text (Input_Handler.Action_Name (index),
                                 X1, Y_Step * Y, 20.0, White);
                Text.Set_Text_Visible (Cal_GP_Text (index), False);
            end if;

            K_Index := Input_Handler.Key_Binding (index);
            if K_Index < 0 or K_Index >= Input_Callback.Max_Keys then
                raise Mmenu_Exception with
                  "Main_Menu_Initialization.Init_Input_Actions, invalid key code " &
                  Integer'Image (K_Index) & " detected.";
            end if;

            if To_String (Input_Callback.Key_Name (index)) /= "" then
                KB_Binding_Text (index) :=
                  Text.Add_Text (To_String (Input_Callback.Key_Name (index)),
                                 X2, Y_Step * Y, 20.0, White);
                Text.Set_Text_Visible (KB_Binding_Text (index), False);

                if Input_Handler.Joy_Axis_Bindings (index) < 0 or
                  Input_Handler.Joy_Axis_Bindings (index) >= 8 then
                    GP_Axis_Binding_Text (index) :=
                      Text.Add_Text ("none", X2, Y_Step * Y, 20.0, White);
                else
                    GP_Axis_Binding_Text (index) :=
                      Text.Add_Text (Input_Handler.Joy_Axis_Sign (index) & "AXIS" &
                                       Integer'Image (Input_Handler.Joy_Axis_Bindings (index)),
                                     X2, Y_Step * Y, 20.0, White);
                end if;
                Text.Set_Text_Visible (GP_Axis_Binding_Text (index), False);

                if Input_Handler.Joy_Button_Bindings (index) < 0 or
                  Input_Handler.Joy_Button_Bindings (index) >= 32 then
                    GP_Buttons_Binding_Text (index) :=
                      Text.Add_Text ("none", X2, Y_Step * Y, 20.0, White);
                else
                    GP_Buttons_Binding_Text (index) :=
                      Text.Add_Text
                        ("B" & Integer'Image (Input_Handler.Joy_Button_Bindings (index)),
                         X2, Y_Step * Y, 20.0, White);
                end if;
                Text.Set_Text_Visible (Input_Handler.Joy_Button_Bindings (index), False);
            end if;
        end loop;

    end Init_Input_Actions;

    --  ------------------------------------------------------------------------

    procedure Init_Input_Text (Input_Text, Input_Value_Text : in out Input_Text_Array;
                               Enabled_Strings              : Menu_String_Array) is
        X                    : Single :=
                                 (-512.0 + 80.0) / Single (Settings.Framebuffer_Width);
        Y                    : Single :=
                                 760.0 / Single (Settings.Framebuffer_Height);
        Y_Step               : Single := 0.0;
        Enabled_String_Index : Integer;
    begin
        for index in Input_Choice_Type'Range loop
            Y_Step := Y_Step + 1.0;
            Input_Text (index) :=
              Text.Add_Text (Input_Strings (index), X, Y_Step * Y,
                             20.0, White);
            Text.Set_Text_Visible (Input_Text (index), False);
        end loop;
        X := (512.0 - 330.0) / Single (Settings.Framebuffer_Width);
        Y := 320.0 / Single (Settings.Framebuffer_Height);
        if Settings.Joystick_Disabled then
            Enabled_String_Index := 1;
        else
            Enabled_String_Index := 2;
        end if;
        Input_Value_Text (Input_Choice_Type'First) :=
          Text.Add_Text (To_String (Enabled_Strings (Enabled_String_Index)),
                         X, Y, 20.0, White);
        Text.Set_Text_Visible (Input_Value_Text (Input_Choice_Type'First), False);
    end Init_Input_Text;

    --  --------------------------- ---------------------------------------------

    procedure Init_Main_Menu_Text (Menu_Text : in out Main_Text_Array) is
        use GL.Types;
        use GL.Types.Singles;
        Y      : Single;
        Y_Step : Single := 0.0;
    begin
        for index in Main_Choice_Type'Range loop
            Y_Step := Y_Step + 1.0;
            Y := (Menu_Text_Yoffs -
                    Y_Step * Menu_Big_Text_Sz) / Single (Settings.Framebuffer_Height);
            --           Game_Utils.Game_Log ("Init_Main_Menu_Text, Menu_String_Item " &
            --                                 Menu_Strings.Menu_String_Items (index));
            Menu_Text (index) :=
              Text.Add_Text (Menu_Strings.Menu_String_Items (index),
                             1.3, Y, 30.0, White);
            Text.Set_Text_Visible (Menu_Text (index), False);
            Text.Centre_Text (Menu_Text (index), 0.5, Y);  --  orig X: 0.0
        end loop;
    end Init_Main_Menu_Text;

    --  ------------------------------------------------------------------------

    procedure Init_Position_And_Texture_Buffers
      (Menu_VAO                        : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
       Position_Buffer, Texture_Buffer : in out GL.Objects.Buffers.Buffer) is
        use GL.Objects.Buffers;
        use GL.Types;
        use GL.Types.Singles;
        Position_Array  : constant Vector2_Array (1 .. 6) :=
                            ((-1.0, 1.0), (-1.0, -1.0), (1.0, -1.0),
                             (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0));
        Texture_Array   : constant Vector2_Array (1 .. 6) :=
                            ((0.0, 1.0), (0.0, 0.0), (1.0, 0.0),
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

    procedure Init_Quit_Text (Confirm_Quit_Text : in out Quit_Text_Array) is
        X        : constant Single :=
                     (512.0 - 330.0) / Single (Settings.Framebuffer_Width);
        Y1       : constant Single := 4.0 * 380.0 / Single (Settings.Framebuffer_Height);
        Y2       : constant Single := 40.0 / Single (Settings.Framebuffer_Height);
        Y_Step   : Single := 0.0;
    begin
        for index in Quit_Choice_Type'Range loop
            Y_Step := Y_Step + 1.0;
            Confirm_Quit_Text (index) :=
              Text.Add_Text (Quit_Strings (index), X, Y_Step * Y2, 20.0, White);
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
              "Main_Menu_Initialization.Init_Cursor Load_Managed_Mesh failed to load src/meshes/3dtitle_idea.apg";
            --  Save Title_VAO
        elsif not Mesh_Loader.Loaded_Mesh_VAO (Title_Mesh_ID, Title_VAO) then
            raise MMenu_Exception with
              "Main_Menu_Initialization.Init_Title failed to initialize VAO for Title_Mesh";
        end if;
        Title_Point_Count := Mesh_Loader.Point_Count (Title_Mesh_ID);

        Title_Author_Text := Text.Add_Text ("a game by anton gerdelan",
                                            0.0, -0.4, 30.0, (0.75, 0.75, 0.75, 1.0));
        Text.Centre_Text (Title_Author_Text, 0.0, -0.8);
        Text.Set_Text_Visible (Title_Author_Text, False);

        Title_Buildstamp_Text := Text.Add_Text ("Ada v1.0 (alpha)",
                                                X, Y, 10.0, (0.5, 0.5, 0.5, 1.0));
        Text.Centre_Text (Title_Buildstamp_Text, 0.0, 0.0);
        Text.Set_Text_Visible (Title_Buildstamp_Text, False);

        Title_Version_Text := Text.Add_Text ("pre-release demo",
                                             0.0, -0.2, 20.0, (1.0, 1.0, 0.0, 1.0));
        Text.Centre_Text (Title_Version_Text, 0.0, -0.8);
        Text.Set_Text_Visible (Title_Version_Text, False);

        Title_Shader_Manager.Init (Title_Shader_Program);
        Maths.Init_Lookat_Transform (Camera_Position, Camera_Target,
                                     (0.0, 1.0, 0.0), Title_V);

        Title_M := Maths.Translation_Matrix ((-0.4, -3.0, -1.0)) * Identity4;
        Title_M := Maths.Scaling_Matrix ((0.5, 0.5, 0.5)) * Title_M;

        GL.Objects.Programs.Use_Program (Title_Shader_Program);
        Title_Shader_Manager.Set_Model_Matrix (Title_M);
        Title_Shader_Manager.Set_View_Matrix (Title_V);
        Title_Shader_Manager.Set_Perspective_Matrix (Camera.GUI_Proj_Matrix);

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Menu_Initialization.Init_Title.");
            raise;
    end Init_Title;

    --  ------------------------------------------------------------------------

    procedure Init_Various
      (Input_Text         : in out Input_Text_Array; Joy_Name : String;
       Joystick_Detected_Text, Greatest_Axis_Text,
       Already_Bound_Text : in out Integer) is
        X      : constant Single :=
                   (-512.0 + 80.0) / Single (Settings.Framebuffer_Width);
        Y      : Single :=
                   (-512.0 + 1500.0) / Single (Settings.Framebuffer_Height);
        Y_Step : Single := 0.0;
    begin
        Joystick_Detected_Text  :=
          Text.Add_Text ("joystick detected: " & Joy_Name & CRLF, X, Y, 20.0, White);
        Text.Set_Text_Visible (Joystick_Detected_Text, False);

        Greatest_Axis_Text  :=
          Text.Add_Text ("axis: ", X,  Y,
                         20.0, (1.0, 1.0, 0.0, 1.0));
        Text.Set_Text_Visible (Greatest_Axis_Text, False);

        Restart_Graphics_Text  :=
          Text.Add_Text ("the game must be restarted" & CRLF &
                           "for some changes to be applied", X,  Y,
                         20.0, (1.0, 1.0, 0.0, 1.0));
        Text.Set_Text_Visible (Restart_Graphics_Text, False);

        Y := (-512.0 + 300.0) / Single (Settings.Framebuffer_Height);
        Already_Bound_Text  :=
          Text.Add_Text ("key is already bound!", X,  Y,
                         20.0, (1.0, 1.0, 0.0, 1.0));
        Text.Set_Text_Visible (Already_Bound_Text, False);

        for index in Input_Choice_Type'Range loop
            Input_Text (index) :=
              Text.Add_Text (Input_Strings (index), X, Y_Step * Y, 20.0, White);
            Text.Set_Text_Visible (Input_Text (index), False);
            Y_Step := Y_Step + 1.0;
        end loop;

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Menu_Initialization.Init_Various.");
            raise;
    end Init_Various;

    --  ------------------------------------------------------------------------

end Main_Menu_Initialization;
