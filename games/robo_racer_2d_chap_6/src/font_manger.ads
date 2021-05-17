
with Glfw.Windows;

with GL.Types;

package Font_Manger is

   procedure Draw_Text (Text : String; X, Y, R, G, B : GL.Types.Single);
   procedure Initialize  (Window  : in out Glfw.Windows.Window);

end Font_Manger;
