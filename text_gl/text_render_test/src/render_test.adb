
with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Render_Test is
   Main_Window  : Glfw.Windows.Window;
   Window_Title : constant String := "Render Test";
begin
   Glfw.Init;
   Initialize (Main_Window, Window_Title);
   Main_Loop (Main_Window);
   Glfw.Shutdown;

end Render_Test;
