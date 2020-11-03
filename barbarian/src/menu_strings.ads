
package Menu_Strings is

   type Main_Choice_Type is (Main_New_Game, Main_Custom_Map, Main_Graphics,
                             Main_Audio, Main_Input, Main_Credits, Main_Quit);
   pragma Ordered (Main_Choice_Type);
   type Graphic_Choice_Type is
     (Graphic_Presets, Graphic_Opengl_Version, Graphic_Windowed_Size,
      Graphic_Full_Screen, Graphic_Vsync, Graphic_Shadows, Graphic_Shadow_Size,
      Graphic_Outlines, Graphic_Framebuffer_Fx, Graphic_Texture_Filter,
      Graphic_Anisotropy, Graphic_Msaa, Graphic_Ssaam, Graphic_Render_Dist,
      Graphic_Far_Clip, Graphic_Auto_Blood_Wipe, Graphic_Show_Fps);
   pragma Ordered (Graphic_Choice_Type);
   type Graphic_Preset_Choice_Type is
     (Graphic_Preset_Dire, Graphic_Preset_Low, Graphic_Preset_Medium,
      Graphic_Preset_High, Graphic_Preset_Ultra,Graphic_Preset_Custom);
   pragma Ordered (Graphic_Preset_Choice_Type);
   type Audio_Choice_Type is
     (Audio_Strings_Audio_Device, Audio_Strings_Master_Volume,
      Audio_Strings_Music_Volume);
   pragma Ordered (Audio_Choice_Type);
   type Input_Choice_Type is
     (Input_Gamepad_Joystick, Input_Calibrate_Keyboard,
      Input_Calibrate_Gamepad_Buttons, Input_Calibrate_Gamepad_Axes_Triggers);
   pragma Ordered (Input_Choice_Type);
   type Quit_Choice_Type is (Quit_Sure, Quit_Yes_Or_No);
   pragma Ordered (Quit_Choice_Type);

   Num_Main_Menu_Entries           : constant Integer := Main_Choice_Type'Size;
   Num_Graphic_Menu_Entries        : constant Integer := Graphic_Choice_Type'Size;
   Num_Graphic_Preset_Menu_Entries : constant Integer := Graphic_Preset_Choice_Type'Size;
   Num_Audio_Menu_Entries          : constant Integer := Audio_Choice_Type'Size;
   Num_Input_Menu_Entries          : constant Integer := Input_Choice_Type'Size;
   Num_Quit_Menu_Entries           : constant Integer := Quit_Choice_Type'Size;

   type Main_Menu_Array is array (Main_Choice_Type'Range) of String (1 .. 16);
   type Graphic_Menu_Array is array (Graphic_Choice_Type'Range)
     of String (1 .. 16);
   type Graphic_Preset_Menu_Array is array (Graphic_Preset_Choice_Type'Range)
     of String (1 .. 6);
   type Audio_Menu_Array is array (Audio_Choice_Type'Range) of String (1 .. 13);
   type Input_Menu_Array is array (Input_Choice_Type'Range) of String (1 .. 31);
   type Quit_Menu_Array is array (Quit_Choice_Type'Range) of String (1 .. 30);

   type Main_Text_Array is array (Main_Choice_Type'Range) of Integer;
   type Graphic_Value_Array is array (Graphic_Choice_Type'Range) of Integer;
   type Audio_Text_Array is array (Audio_Choice_Type'Range) of Integer;
   type Input_Text_Array is array (Input_Choice_Type'Range) of Integer;
   type Quit_Text_Array is array (Quit_Choice_Type'Range) of Integer;

   Menu_String_Items : constant Main_Menu_Array :=
                      ("new game        ",
                       "custom maps     ",
                       "graphics        ",
                       "audio           ",
                       "input           ",
                       "credits         ",
                       "quit            ");

   Graphic_Strings   : constant Graphic_Menu_Array :=
                        ("graphics presets",
                         "opengl version  ",
                         "windowed size   ",
                         "full-screen     ",
                         "vsync           ",
                         "shadows         ",
                         "shadow size     ",
                         "outlines        ",
                         "framebuffer fx  ",
                         "texture filter  ",
                         "anisotropy      ",
                         "msaa            ",
                         "ssaa            ",
                         "render dist     ",
                         "far clip        ",
                         "auto blood wipe ",
                         "show fps        ");

   Graphic_Preset_Strings  : constant Graphic_Preset_Menu_Array :=
                               ("dire  ",
                                "low   ",
                                "medium",
                                "high  ",
                                "ultra ",
                                "custom");

   Audio_Strings    : constant Audio_Menu_Array :=
                        ("audio device ",
                         "master volume",
                         "music volume ");

   Input_Strings    : constant Input_Menu_Array :=
                        ("gamepad/joystick               ",
                         "calibrate keyboard             ",
                         "calibrate gamepad buttons      ",
                         "calibrate gamepad axes/triggers");

   Quit_Strings     : constant Quit_Menu_Array :=
                        ("are you sure you want to quit?",
                         "(y/n)                         ");
end Menu_Strings;
