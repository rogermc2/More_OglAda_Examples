
with Glfw.Windows;

with GL_Maths;

package Menu_Support is

   procedure Process_Menu_Audio (Window : in out Glfw.Windows.Window;
                                 Menu_Audio_Open : in out Boolean;
                                 Audio_Cursor_Current_Item : Integer := -1);
   function Process_Menu_Gr (Window              : in out Glfw.Windows.Window;
                             Graphic_Value_Text  : GL_Maths.Integer_Array;
                             Menu_Gr_Open, Restart_Flag : in out Boolean;
                             Since_Last_Key      : in out Float;
                             Cursor_Current_Item,
                             Current_Video_Mode  : in out Integer) return Boolean;
end Menu_Support;

