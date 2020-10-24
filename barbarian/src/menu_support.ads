
with Glfw.Windows;

with GL_Maths;

package Menu_Support is

   procedure Process_Menu_Cal_GP (Window  : in out Glfw.Windows.Window);
   procedure Process_Menu_Cal_KB (Window                    : in out Glfw.Windows.Window;
                                  KB_Binding_Text          : GL_Maths.Integer_Array;
                                  Greatest_Axis_Text        : Integer;
                                  Already_Bound_Text         : Integer;
                                  Modify_Binding_Mode, Already_Bound : in out Boolean;
                                  Since_Last_Key            : in out Float);
   procedure Process_Menu_Audio (Window                    : in out Glfw.Windows.Window;
                                 Audio_Value_Text          : GL_Maths.Integer_Array;
                                 Menu_Audio_Open           : in out Boolean;
                                 Since_Last_Key            : in out Float;
                                 Audio_Cursor_Current_Item : in out Integer);
   function Process_Menu_Gr (Window                     : in out Glfw.Windows.Window;
                             Graphic_Value_Text         : GL_Maths.Integer_Array;
                             Menu_Gr_Open, Restart_Flag : in out Boolean;
                             Since_Last_Key             : in out Float;
                             Cursor_Current_Item,
                             Current_Video_Mode         : in out Integer)
                             return Boolean;
   procedure Process_Menu_Input (Window  : in out Glfw.Windows.Window;
                                 Joy_Name : String;
                                 Menu_Input_Open,
                                 Menu_Cal_Gp_Butts_Open : in out Boolean;
                                 Joystick_Detected_Text  : in out Integer;
                                 Input_Cursor_Current_Item : in out Integer);

end Menu_Support;

