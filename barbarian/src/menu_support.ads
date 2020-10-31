
with Glfw.Windows;

with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with GL_Maths;

package Menu_Support is

   type Menu_Choice_Type is (Menu_New_Game, Menu_Custom_Map, Menu_Graphics,
                             Menu_Audio, Menu_Input, Menu_Credits, Menu_Quit);
   pragma Ordered (Menu_Choice_Type);

   function Confirm_Quit_Open (Window : in out Glfw.Windows.Window;
                               Confirm_Quit_Open : in out Boolean)
                               return Boolean;
   procedure Do_Bounce (Title_Bounce_Timer : in out Float; Elapsed : Float;
                        Title_V : in out GL.Types.Singles.Matrix4);
   procedure Draw_3D_Menu_Items
     (Menu_Cursor_Texture : GL.Objects.Textures.Texture;
      Cursor_VAO          : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Cursor_Shader_Program : GL.Objects.Programs.Program;
      Cursor_M   : in out GL.Types.Singles.Matrix4;
      Cursor_V  : GL.Types.Singles.Matrix4;
      Cursor_Pos          : GL.Types.Singles.Vector2;
      Cursor_Scale : GL.Types.Single;
      Cursor_Point_Count  : Integer; Elapsed : Float);
   function General_Menu_Support (Window  : in out Glfw.Windows.Window;
                                 Joystick_Detected_Text    : Integer;
                                 Joy_Name : String;
                                  Menu_Was_Closed, Graphics_Open, Audio_Open,
                                  Input_Open, Confirm_Quit_Open,
                                  Credits_Open, New_Game, In_Custom_Map,
                                  Custom_Maps  : in out Boolean;
                                  Since_Last_Key  : in out Float;
                                  Menu_Cursor_Item : in out Menu_Choice_Type)
                                  return Boolean;
   procedure Process_Menu_Audio (Window                    : in out Glfw.Windows.Window;
                                 Audio_Value_Text          : GL_Maths.Integer_Array;
                                 Menu_Audio_Open           : in out Boolean;
                                 Since_Last_Key            : in out Float;
                                 Audio_Cursor_Item : in out Integer);
   procedure Process_Menu_Cal_GP;
   procedure Process_Menu_Cal_KB (Window                    : in out Glfw.Windows.Window;
                                  KB_Binding_Text          : GL_Maths.Integer_Array;
                                  Greatest_Axis_Text        : Integer;
                                  Already_Bound_Text         : Integer;
                                  Modify_Binding_Mode,
                                  Already_Bound, Menu_Cal_KB_Open : in out Boolean;
                                  Since_Last_Key            : in out Float);
   procedure Process_Menu_Credits (Window: in out Glfw.Windows.Window;
                                   Credits_Open, End_Story_Open,
                                   Menu_Closed : in out Boolean;
                                  Text_Timer : in out Float);
   function Process_Menu_Graphics (Window                     : in out Glfw.Windows.Window;
                             Graphic_Value_Text         : GL_Maths.Integer_Array;
                             Menu_Gr_Open, Restart_Flag : in out Boolean;
                             Since_Last_Key             : in out Float;
                             Cursor_Item, Video_Mode      : in out Integer)
                             return Boolean;
   procedure Process_Menu_Input (Window  : in out Glfw.Windows.Window;
                                 Joy_Name : String;
                                 Since_Last_Key        : in out Float;
                                 Menu_Input_Open, Menu_Cal_KB_Open,
                                 Menu_Cal_Gp_Axes_Open,
                                 Menu_Cal_Gp_Butts_Open : in out Boolean;
                                 Joystick_Detected_Text  : Integer;
                                 Input_Cursor_Item : in out Integer);

end Menu_Support;

