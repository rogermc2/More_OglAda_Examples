

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Tessellation;
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

   Shader_Program        : GL.Objects.Programs.Program;
   Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer         : GL.Objects.Buffers.Buffer;
   Element_Buffer        : GL.Objects.Buffers.Buffer;
   Position_Attribute_ID : GL.Attributes.Attribute;
   Projection_Matrix_ID  : GL.Uniforms.Uniform;
   Inner_Location_ID     : GL.Uniforms.Uniform;
   Outer_Location_ID     : GL.Uniforms.Uniform;
   Model_View_Matrix     : GL.Types.Singles.Matrix4;

    Inner                : constant Single := 10.0;
    Outer                : constant Single := 10.0;
    Background           : constant GL.Types.Colors.Color := (0.7, 0.7, 0.7, 1.0);

   --  ------------------------------------------------------------------------

    procedure Initialize is
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data.Teapot_Vertices,
                                     Static_Draw);

      Element_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Element_Buffer);
      Utilities.Load_Element_Buffer (Element_Array_Buffer,
                                     Utilities.Flatten (Vertex_Data.Teapot_Indices),
                                     Static_Draw);

      Shader_Program := Program_From
        ((Src ("../media/shaders/teapot/teapot.vert", Vertex_Shader),
          Src ("../media/shaders/teapot/teapot.cont", Tess_Control_Shader),
          Src ("../media/shaders/teapot/teapot.eval", Tess_Evaluation_Shader),
          Src ("../media/shaders/teapot/teapot.frag", Fragment_Shader)));
       GL.Objects.Programs.Use_Program (Shader_Program);

      Position_Attribute_ID := GL.Objects.Programs.Attrib_Location
        (Shader_Program, "vPosition");
      GL.Attributes.Enable_Vertex_Attrib_Array (Position_Attribute_ID);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Position_Attribute_ID, 3, Single_Type, 0, 0);

      Projection_Matrix_ID := Uniform_Location (Shader_Program, "p");
      Inner_Location_ID := Uniform_Location (Shader_Program, "Inner");
      Outer_Location_ID := Uniform_Location (Shader_Program, "Outer");

      GL.Uniforms.Set_Single (Inner_Location_ID, Inner);
      GL.Uniforms.Set_Single (Outer_Location_ID, Outer);

        Model_View_Matrix := Maths.Translation_Matrix ((-0.2625, -1.575, -1.0));
        Model_View_Matrix := Maths.Translation_Matrix ((0.0, 0.0, -7.5)) * Model_View_Matrix;
      GL.Uniforms.Set_Single (Uniform_Location (Shader_Program, "MV"), Model_View_Matrix);
        GL.Tessellation.Set_Patch_Vertices (Vertex_Data.Num_Teapot_Vertices_Per_Patch);

      Utilities.Clear_Background_Colour_And_Depth (Background);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Initialize.");
         raise;
   end Initialize;

   --  ------------------------------------------------------------------------

   procedure Render (Window :  in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      use GL.Objects.Vertex_Arrays;
      use GL.Types.Singles;
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
      Projection_Matrix : Matrix4;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (10, 10, GL.Types.Int (Window_Width) - 20,
                              GL.Types.Int (Window_Height) - 20);
      Utilities.Clear_Colour_Buffer_And_Depth;

      GL.Objects.Programs.Use_Program (Shader_Program);

      --  Set up the projection matrix
      --  Top, Bottom, Left, Right, Near, Far
      Maths.Init_Perspective_Transform (Maths.Degree (60), Single (Window_Width), Single (Window_Height), 5.0, 10.0, Projection_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      --  Set up for the Draw_Elements call
      Vertex_Array.Bind;
      Element_Array_Buffer.Bind (Element_Buffer);

      Draw_Elements (Patches, Vertex_Data.Num_Teapot_Vertices, UInt_Type);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------


   use Glfw.Input;
   Running : Boolean := True;
begin
   Initialize;
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
      Put_Line ("An exceptiom occurred in Main_Loop.");
      raise;
end Main_Loop;
