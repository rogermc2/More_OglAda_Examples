
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with Input_Callback; use Input_Callback;

with GL_Maths;

with Menu_Strings; use Menu_Strings;

package Menu_Support is

   Menu_Support_Error : Exception;

   procedure Check_Close_Menu_Credits (Window      : in out Barbarian_Window;
                                       Credits_Open, End_Story_Open,
                                       Menu_Closed : in out Boolean;
                                       Text_Timer  : in out Float);
   function Confirm_Quit_Open (Window            : in out Barbarian_Window;
                               Confirm_Quit_Open : in out Boolean)
                               return Boolean;
   procedure Do_Bounce (Title_Bounce_Timer : in out Float; Elapsed : Float;
                        Title_V            : in out GL.Types.Singles.Matrix4);
   procedure Draw_Skull_Cursor
     (Menu_Cursor_Texture   : GL.Objects.Textures.Texture;
      Cursor_VAO            : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Cursor_Shader_Program : GL.Objects.Programs.Program;
      Cursor_M              : in out GL.Types.Singles.Matrix4;
      Cursor_V              : GL.Types.Singles.Matrix4;
      Cursor_Pos            : GL.Types.Singles.Vector2;
      Cursor_Scale          : GL.Types.Single;
      Cursor_Point_Count    : Integer; Elapsed : Float);
   procedure General_Menu_Support
     (Window                    : in out Barbarian_Window;
      Joystick_Detected_Text    : Integer;
      Joy_Name                  : String;
      Menu_Was_Closed, Graphics_Open, Audio_Open,
      Input_Open, Confirm_Quit_Open,
      Credits_Open, New_Game, In_Custom_Map,
      Custom_Maps               : in out Boolean;
      Since_Last_Key            : in out Float;
      Menu_Cursor_Item          : in out Menu_Strings.Main_Choice_Type);

   procedure Process_Menu_Audio (Window            : in out Barbarian_Window;
                                 Audio_Value_Text  : Audio_Text_Array;
                                 Menu_Audio_Open   : in out Boolean;
                                 Since_Last_Key    : in out Float;
                                 Audio_Cursor_Item : in out Audio_Choice_Type);
   procedure Process_Menu_Cal_GP;
   procedure Process_Menu_Cal_KB
     (Window                          : in out Barbarian_Window;
      KB_Binding_Text                 : GL_Maths.Integer_Array;
      Greatest_Axis_Text              : Integer;
      Already_Bound_Text              : Integer;
      Modify_Binding_Mode,
      Already_Bound, Menu_Cal_KB_Open : in out Boolean;
      Since_Last_Key                  : in out Float);
   function Process_Menu_Graphics
     (Window                     : in out Barbarian_Window;
      Graphic_Value_Text         : Menu_Strings.Graphic_Value_Array;
      Menu_Gr_Open, Restart_Flag : in out Boolean;
      Since_Last_Key             : in out Float;
      Cursor_Item                : in out Menu_Strings.Graphic_Choice_Type;
      Video_Mode                 : in out Integer)
      return Boolean;
   procedure Process_Menu_Input (Window                  : in out Barbarian_Window;
                                 Joy_Name                : String;
                                 Since_Last_Key          : in out Float;
                                 Menu_Input_Open, Menu_Cal_KB_Open,
                                 Menu_Cal_Gp_Axes_Open,
                                 Menu_Cal_Gp_Butts_Open  : in out Boolean;
                                 Joystick_Detected_Text  : Integer;
                                 Input_Cursor_Item       : in out Input_Choice_Type);

end Menu_Support;

