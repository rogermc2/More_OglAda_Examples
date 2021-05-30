
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with GL.Buffers;
with GL.Types.Colors;
with GL.Window;

with Utilities;

with Model;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Back          : constant GL.Types.Colors.Color :=
                                (0.6, 0.6, 0.6, 0.0);
   Border_Width  : constant GL.Types.Size := 2;
   Ship          : Model.Model_Data;
   Ship_Colour   : constant GL.Types.Colors.Basic_Color := (0.0, 0.0, 1.0);
   Asteriods     : array (1 .. 3) of Model.Model_Data;
   Last_Time     : Float := Float (Glfw.Time);
--     Rotation                 : Maths.Degree := 0.0;
   --      Full_Screen                  : Boolean := False;

   procedure Resize_GL_Scene  (Screen : in out Glfw.Windows.Window);

   --  ------------------------------------------------------------------------

   procedure Render (Screen : in out Glfw.Windows.Window) is
   begin
      Utilities.Clear_Colour;
      Resize_GL_Scene (Screen);
      Model.Render (Ship);
   end Render;

   --  ------------------------------------------------------------------------

   procedure Resize_GL_Scene (Screen : in out Glfw.Windows.Window) is
      use GL.Types;
      Screen_Width      : Glfw.Size;
      Screen_Height     : Glfw.Size;
      VP_Width          : Size;
      VP_Height         : Size;
--        Projection_Matrix : Singles.Matrix4 := Singles.Identity4;
   begin
      Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      VP_Width := Size (Screen_Width) - 2 * Border_Width;
      VP_Height := Size (Screen_Height) - 2 * Border_Width;
      GL.Window.Set_Viewport (Border_Width, Border_Width, VP_Width, VP_Height);

--        Maths.Init_Perspective_Transform
--          (Maths.Degree (45.0), Single (Screen_Width), Single (Screen_Height),
--           0.1, 100.0,
--           Projection_Matrix);
--        Use_Program (Game_Program);
--        Shader_Manager_Game.Set_Projection_Matrix (Projection_Matrix);

   end Resize_GL_Scene;

   --  ------------------------------------------------------------------------

   procedure Start_Game (Screen : in out Glfw.Windows.Window) is
      use GL.Types;
      Window_Width       : Glfw.Size;
      Window_Height      : Glfw.Size;
   begin
      Screen'Access.Get_Size (Window_Width, Window_Height);

      Utilities.Clear_Background_Colour_And_Depth (Back);
      GL.Buffers.Set_Depth_Function (LEqual);

      Model.Initialize (Ship, "src/tri_ship.obj", Ship_Colour);
      Model.Set_Is_Ship (Ship, True);
      Model.Set_Base_Rotation (Ship, (90.0, 0.0, 0.0));
      Model.Set_Velocity (Ship, 1.0);

      Model.Initialize (Asteriods (1), "src/tri_asteroid.obj",  (1.0, 0.0, 0.0));
      Model.Set_Position (Asteriods (1), (0.0, 0.0, -10.0));

      Model.Initialize (Asteriods (2), "src/tri_asteroid.obj",  (0.0, 1.0, 0.0));
      Model.Set_Position (Asteriods (2), (5.0, 0.0, -15.0));

      Model.Initialize (Asteriods (3), "src/tri_asteroid.obj",  (0.0, 1.0, 1.0));
      Model.Set_Position (Asteriods (3), (5.0, 5.0, -20.0));

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Start_Game.");
         Put_Line (Exception_Information (anError));
         raise;
   end Start_Game;

   --  -------------------------------------------------------------------------

   procedure Update is

   Current_Time : constant Float := Float (Glfw.Time);
   Delta_Time   : constant Float := Current_Time - Last_Time;
   begin
      Last_Time := Current_Time;
      Model.Update (Ship, Delta_Time);
      for index in Asteriods'Range loop
         Model.Update (Asteriods (index), Delta_Time);
      end loop;
   end Update;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running  : Boolean := True;
begin
   Start_Game (Main_Window);
   while Running loop
      Update;
      Render (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;

exception
   when anError : others =>
      Put_Line ("An exception occurred in Main_Loop.");
      Put_Line (Exception_Information (anError));
      raise;

end Main_Loop;
