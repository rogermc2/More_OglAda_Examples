
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with Controls;
with Utilities;

with Particle_System;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Background               : constant GL.Types.Colors.Color := (0.6, 0.6, 1.0, 0.0);
   Billboard_Vertices_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Billboard_Buffer         : GL.Objects.Buffers.Buffer;

   --  ------------------------------------------------------------------------

   procedure Load_Billboard_Buffer is
      use GL.Objects.Buffers;
      use GL.Types;
      Vertex_Data  : constant Singles.Vector3_Array (1 .. 4) :=
                       ((-0.5, -0.5, 0.0),
                        (0.5, -0.5, 0.0),
                        (-0.5, 0.5, 0.0),
                        (0.5, 0.5, 0.0));
   begin
      Billboard_Buffer.Initialize_Id;
      Array_Buffer.Bind (Billboard_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Billboard_Buffer.");
         raise;
   end Load_Billboard_Buffer;

   --  ------------------------------------------------------------------------

   procedure Load_Matrices (Window  : in out Glfw.Windows.Window) is
      use GL.Types;
      use GL.Types.Singles;
      View_Matrix       : Matrix4;
      Projection_Matrix : Matrix4;
      VP_Matrix         : Matrix4;
   begin
      Controls.Compute_Matrices_From_Inputs (Window, Projection_Matrix, View_Matrix);
      VP_Matrix :=  Projection_Matrix * View_Matrix;
      Particle_System.Set_IDs (VP_Matrix);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Load_Matrices.");
         raise;
   end Load_Matrices;

   --  ------------------------------------------------------------------------

   procedure Render (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      use GL.Types;
   begin
      Utilities.Clear_Background_Colour_And_Depth (Background);

      Load_Matrices (Window);
      Particle_System.Update_Particles;

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      Array_Buffer.Bind (Billboard_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, 0, 0);
      GL.Attributes.Vertex_Attrib_Divisor (0, 0);

      Particle_System.Render_Particles;
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window) is
      use GL.Types;
      use Glfw.Input;
      Window_Width    : constant Glfw.Size := 1024;
      Window_Height   : constant Glfw.Size := 768;

   begin
      Window.Set_Input_Toggle (Sticky_Keys, True);
      Window.Set_Cursor_Mode (Mouse.Disabled);

      Window'Access.Set_Size (Window_Width, Window_Height);
      Window'Access.Set_Cursor_Pos (Mouse.Coordinate (0.5 * Single (Window_Width)),
                                    Mouse.Coordinate (0.5 * Single (Window_Height)));
      Utilities.Clear_Background_Colour_And_Depth (Background);

      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.Less);

      Billboard_Vertices_Array.Initialize_Id;
      Billboard_Vertices_Array.Bind;

      Particle_System.Init;
      Load_Billboard_Buffer;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running         : Boolean := True;
begin
   Setup (Main_Window);
   while Running loop
      Render (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and then
        not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and then not Main_Window.Should_Close;
   end loop;

exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
