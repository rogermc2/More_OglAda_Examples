

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
--  with GL.Tessellation;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;
with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;
   use GL.Uniforms;

   Shader_Program        : GL.Objects.Programs.Program;
   Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer         : GL.Objects.Buffers.Buffer;
   Element_Buffer        : GL.Objects.Buffers.Buffer;
   Position_Attribute_ID : GL.Attributes.Attribute;
   MV_Matrix_ID          : Uniform := -1;
   Projection_Matrix_ID  : Uniform := -1;
   Inner_Location_ID     : Uniform := -1;
   Outer_Location_ID     : Uniform := -1;
   Model_View_Matrix     : GL.Types.Singles.Matrix4;

   Inner                : Single := 10.0;
   Outer                : Single := 10.0;
   Mode                 : GL.Rasterization.Polygon_Mode_Type :=
                            GL.Rasterization.Line;
   Background           : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);

   --  ------------------------------------------------------------------------

   procedure Process_Keyboard (Window : in out Glfw.Windows.Window) is
      use Glfw.Input;
      use GL.Rasterization;
   begin
      if Window'Access.Key_State (Keys.K) = Pressed then
         Inner := Inner - 1.0;
         if Inner < 1.0 then
            Inner := 1.0;
         end if;
         Set_Single (Inner_Location_ID, Inner);
      elsif Window'Access.Key_State (Keys.I) = Pressed then
         Inner := Inner + 1.0;
         if Inner > 64.0 then
            Inner := 64.0;
         end if;
         Set_Single (Inner_Location_ID, Inner);
      elsif Window'Access.Key_State (Keys.L) = Pressed then
         Outer := Outer - 1.0;
         if Outer < 1.0 then
            Outer := 1.0;
         end if;
         Set_Single (Outer_Location_ID, Outer);
      elsif Window'Access.Key_State (Keys.O) = Pressed then
         Outer := Outer + 1.0;
         if Outer > 64.0 then
            Outer := 64.0;
         end if;
         Set_Single (Outer_Location_ID, Outer);
      elsif Window'Access.Key_State (Keys.R) = Pressed then
         Inner := 10.0;
         Outer := 10.0;
         Set_Single (Inner_Location_ID, Inner);
         Set_Single (Outer_Location_ID, Outer);
      elsif Window'Access.Key_State (Keys.M) = Pressed then
         if Mode = Fill then
            Mode := Line;
         else
            Mode := Fill;
         end if;
         GL.Rasterization.Set_Polygon_Mode (Mode);
      end if;

   end Process_Keyboard;

   --  -------------------------------------------------------------------------

   procedure Setup is
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
      Flat_Array : constant GL.Types.Int_Array (1 .. 16 * Vertex_Data.Num_Teapot_Patches)
        := Utilities.Flatten (Vertex_Data.Teapot_Indices);
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Utilities.Clear_Background_Colour_And_Depth (Background);

      Shader_Program := Program_From
        ((Src ("../media/shaders/teapot/teapot.vert", Vertex_Shader),
         Src ("../media/shaders/teapot/teapot.cont", Tess_Control_Shader),
         Src ("../media/shaders/teapot/teapot.eval", Tess_Evaluation_Shader),
         Src ("../media/shaders/teapot/teapot.frag", Fragment_Shader)));
      GL.Objects.Programs.Use_Program (Shader_Program);

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data.Teapot_Vertices,
                                    Static_Draw);

      Element_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Element_Buffer);
      Utilities.Load_Element_Buffer (Element_Array_Buffer,
                                     Flat_Array, Static_Draw);

      Position_Attribute_ID := GL.Objects.Programs.Attrib_Location
        (Shader_Program, "vPosition");
      GL.Attributes.Enable_Vertex_Attrib_Array (Position_Attribute_ID);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Position_Attribute_ID, 3,
                                               Single_Type, True, 0, 0);

      MV_Matrix_ID := Uniform_Location (Shader_Program, "MV");
      Projection_Matrix_ID := Uniform_Location (Shader_Program, "Proj");
      Inner_Location_ID := Uniform_Location (Shader_Program, "Inner");
      Outer_Location_ID := Uniform_Location (Shader_Program, "Outer");

      GL.Uniforms.Set_Single (Inner_Location_ID, Inner);
      GL.Uniforms.Set_Single (Outer_Location_ID, Outer);

      Model_View_Matrix := Maths.Translation_Matrix ((-0.2625, -1.575, -1.0));
      Model_View_Matrix := Maths.Translation_Matrix ((0.0, 0.0, -7.5)) * Model_View_Matrix;

      GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
--        GL.Tessellation.Set_Patch_Vertices (Vertex_Data.Num_Teapot_Vertices_Per_Patch);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   procedure Render (Main_Window : in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      use GL.Types.Singles;
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
      Projection_Matrix : Matrix4;
   begin
      Main_Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Process_Keyboard (Main_Window);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Colour_Buffer_And_Depth;
      Delay (0.5);

      --  Set up the projection matrix
      Maths.Init_Perspective_Transform (Maths.Degree (60), Single (Window_Width),
                                        Single (Window_Height), 1.0, 10.0,
                                        Projection_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      Draw_Elements (Patches, Vertex_Data.Num_Teapot_Vertices, UInt_Type);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   use Glfw.Input;   Running : Boolean := True;
begin
   Setup;
   while Running loop
      Render (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;
exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
