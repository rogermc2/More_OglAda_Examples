
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Window;

with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with Controls;
with Maths;
with Utilities;

with Buffers_Manager;
with Shader_Manager;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Black                    : constant GL.Types.Colors.Color :=
                                (0.0, 0.0, 0.0, 0.0);
   Screen_Width             : constant Glfw.Size := 800;
   Screen_Height            : constant Glfw.Size := 600;
   --      Text_Width               : constant GL.Types.Int := 8;
   --      Text_Height              : constant GL.Types.Int := 13;
   Camera_Distance          : constant GL.Types.Single := 4.0;
   thePosition              : constant GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
   --      Camera_Position          : GL.Types.Singles.Vector3 := (0.0, 0.0, Camera_Distance);
   --      Draw_Mode : constant GL.Types.Int := 0;

   Vertices_Array_Object    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer_1          : GL.Objects.Buffers.Buffer;
   Index_Buffer_1           : GL.Objects.Buffers.Buffer;
   Render_Program           : GL.Objects.Programs.Program;

   --  ------------------------------------------------------------------------

   procedure Draw (Window_Width, Window_Height : Glfw.Size);
   procedure Set_Matrices (Window_Width, Window_Height : Glfw.Size;
                           Render_Program              : GL.Objects.Programs.Program);

   --  ------------------------------------------------------------------------

   procedure Init_GL is
   begin
      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.LEqual);
      GL.Culling.Set_Cull_Face (GL.Culling.Back);
      GL.Toggles.Enable (GL.Toggles.Cull_Face);

      Utilities.Clear_Background_Colour (Black);
      GL.Buffers.Clear_Stencil_Buffer (0);
      GL.Buffers.Clear_Depth_Buffer (1.0);
      GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Words);

   end Init_GL;

   --  ------------------------------------------------------------------------

   procedure Display (Window      : in out Glfw.Windows.Window) is
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
   begin
      --  Clear (GL_DEPTH_BUFFER_BIT, GL_STENCIL_BUFFER_BIT, COLOR_BUFFER_BIT)
      GL.Buffers.Clear ((True, False, True, True));
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, Int (Window_Width), Int (Window_Height));

      GL.Objects.Programs.Use_Program (Render_Program);

      Draw (Window_Width, Window_Height);

   exception
      when others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   procedure Draw  (Window_Width, Window_Height : Glfw.Size) is
      use GL.Objects.Buffers;
      Stride        : constant Int := Maths.Vector8'Size / 8;
   begin
      Set_Matrices (Window_Width, Window_Height, Render_Program);
      GL.Objects.Programs.Use_Program (Render_Program);
      Array_Buffer.Bind (Vertex_Buffer_1);
      Element_Array_Buffer.Bind (Index_Buffer_1);
      Shader_Manager.Set_Texture_Used (False);
      --  First attribute buffer : vertices
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, Stride, 0);

      --  Second attribute buffer : Tex Coords
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, False, Stride, 3);

      --  Second attribute buffer Normals:
      GL.Attributes.Enable_Vertex_Attrib_Array (2);
      GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, False, Stride, 5);

      GL.Objects.Buffers.Draw_Elements (Triangles, 3, UInt_Type, 0);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);
      GL.Attributes.Disable_Vertex_Attrib_Array (2);

   end Draw;

   --  ------------------------------------------------------------------------

   procedure Set_Matrices (Window_Width, Window_Height : Glfw.Size;
                           Render_Program              : GL.Objects.Programs.Program) is
      use GL.Types.Singles;
      use Maths;
      --          Camera_Position     : Vector3;
      Camera_Angle_X      : constant Single := 0.0;
      Camera_Angle_Y      : constant Single := 0.0;
      --          Up                  : constant Vector3 := (0.0, 1.0, 0.0);
      Matrix_Model        : Matrix4 := Identity4;
      View_Matrix         : constant Matrix4 :=
                              Translation_Matrix ((0.0, 0.0, -Camera_Distance));
      Matrix_Model_View   : Matrix4 := Identity4;
      Normal_Matrix       : Matrix4 := Identity4;
      Projection_Matrix   : Matrix4;
      MVP_Matrix          : Matrix4;
   begin
      Matrix_Model :=
        Rotation_Matrix (Degree (90.0), (1.0, 0.0, 0.0)) * Matrix_Model;
      Matrix_Model :=
        Rotation_Matrix (Degree (Camera_Angle_Y), (0.0, 1.0, 0.0)) * Matrix_Model;
      Matrix_Model :=
        Rotation_Matrix (Degree (Camera_Angle_X), (1.0, 0.0, 0.0)) * Matrix_Model;
      Init_Perspective_Transform (Degree (45.0), Single (Window_Width),
                                  Single (Window_Height),
                                  0.1, 100.0, Projection_Matrix);

      Matrix_Model := Translation_Matrix (thePosition) * Matrix_Model;

      --          Camera_Position := Target_Position + (0.0, 0.0, -Camera_Distance);
      --          Utilities.Print_Vector ("Camera_Position", Camera_Position);
      --          Init_Lookat_Transform (Camera_Position, Target_Position, Up, View_Matrix);
      Matrix_Model_View := View_Matrix * Matrix_Model;

      Normal_Matrix := Matrix_Model_View;
      Normal_Matrix (GL.X, GL.W) := 0.0;
      Normal_Matrix (GL.Y, GL.W) := 0.0;
      Normal_Matrix (GL.Z, GL.W) := 0.0;
      Normal_Matrix (GL.W, GL.X) := 0.0;
      Normal_Matrix (GL.W, GL.Y) := 0.0;
      Normal_Matrix (GL.W, GL.Z) := 0.0;
      Normal_Matrix (GL.W, GL.W) := 1.0;

      MVP_Matrix :=  Projection_Matrix * Matrix_Model_View;

      GL.Objects.Programs.Use_Program (Render_Program);
      Shader_Manager.Set_Matrix_Model_View (Matrix_Model_View);
      Shader_Manager.Set_Matrix_Model_View_Projection (MVP_Matrix);
      Shader_Manager.Set_Matrix_Normal (Normal_Matrix);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Set_Matrices.");
         raise;
   end Set_Matrices;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window) is
   begin
      Window.Set_Size (Screen_Width, Screen_Height);
      Init_GL;
      Vertices_Array_Object.Initialize_Id;
      Vertices_Array_Object.Bind;

      Shader_Manager.Init (Render_Program);

      Buffers_Manager.Create_Vertex_Buffer (Vertex_Buffer_1);
      Buffers_Manager.Create_Index_Buffer (Index_Buffer_1);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup (Main_Window);
   while Running loop
      Display (Main_Window);
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
