
--  with GL.Types; use GL.Types;

package Menu_Strings is

   Num_Menu_Entries           : constant Integer := 7;
   Num_Graphic_Entries        : constant Integer := 17;
   Num_Graphic_Preset_Entries : constant Integer := 6;
   Num_Audio_Entries          : constant Integer := 3;
   Num_Input_Entries          : constant Integer := 4;
   Num_Quit_Entries           : constant Integer := 2;

   type Menu_Text_Array is array (1 .. Num_Menu_Entries) of String (1 .. 16);
   type Graphic_Text_Array is array (1 .. Num_Graphic_Entries) of String (1 .. 16);
   type Graphic_Preset_Array is array (1 .. Num_Graphic_Preset_Entries) of
     String (1 .. 6);
   type Audio_Text_Array is array (1 .. Num_Audio_Entries) of String (1 .. 13);
   type Input_Text_Array is array (1 .. Num_Input_Entries) of String (1 .. 31);
   type Quit_Text_Array is array (1 .. Num_Quit_Entries) of String (1 .. 30);

   Menu_String_Items : constant Menu_Text_Array :=
                      ("new game        ",
                       "custom maps     ",
                       "graphics        ",
                       "audio           ",
                       "input           ",
                       "credits         ",
                       "quit            ");

   Graphic_Strings   : constant Graphic_Text_Array :=
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

   Graphic_Preset_Strings  : constant Graphic_Preset_Array :=
                               ("dire  ",
                                "low   ",
                                "medium",
                                "high  ",
                                "ultra ",
                                "custom");

   Audio_Strings    : constant Audio_Text_Array :=
                        ("audio device ",
                         "master volume",
                         "music volume ");

   Input_Strings    : constant Input_Text_Array :=
                        ("gamepad/joystick               ",
                         "calibrate keyboard             ",
                         "calibrate gamepad buttons      ",
                         "calibrate gamepad axes/triggers");

   Quit_Strings     : constant Quit_Text_Array :=
                        ("are you sure you want to quit?",
                         "(y/n)                         ");
end Menu_Strings;
