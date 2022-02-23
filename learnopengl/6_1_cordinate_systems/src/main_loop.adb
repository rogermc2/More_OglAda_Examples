
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Uniforms;
with GL.Types;
with GL.Types.Colors;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with Textures_61;
with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Vertex_Array   : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer  : GL.Objects.Buffers.Buffer;
   Stride         : constant Int := Maths.Vector5'Size / 8;
   Skip_Size      : constant Int := Single'Size / 8;
   Indices_Buffer : GL.Objects.Buffers.Buffer;
   Texture_1      : GL.Objects.Textures.Texture;
   Texture_2      : GL.Objects.Textures.Texture;
   Render_Program : GL.Objects.Programs.Program;
   Model_ID       : GL.Uniforms.Uniform;
   View_ID        : GL.Uniforms.Uniform;
   Projection_ID  : GL.Uniforms.Uniform;
   Texture_1_ID   : GL.Uniforms.Uniform;
   Texture_2_ID   : GL.Uniforms.Uniform;

   --  ------------------------------------------------------------------------

   procedure Render  (Window  : in out Glfw.Windows.Window) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use GL.Types.Singles;
      use Maths;
      Background        : constant GL.Types.Colors.Color := (0.2, 0.3, 0.3, 1.0);
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
      Model_Matrix      : Matrix4 := Singles.Identity4;
      View_Matrix       : Matrix4 := Singles.Identity4;
      Projection_Matrix : Matrix4 := Singles.Identity4;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, Int (Window_Width), Int (Window_Height));
      Utilities.Clear_Background_Colour (Background);

      Set_Active_Unit (0);
      Texture_2D.Bind (Texture_1);
      Set_Active_Unit (1);
      Texture_2D.Bind (Texture_2);

      Model_Matrix := Rotation_Matrix (Degree (-55.0), (1.0, 0.0, 0.0));
      View_Matrix := Translation_Matrix ((0.0, 0.0, -3.0));
      Init_Perspective_Transform
        (Degree (45.0), Single (Window_Width), Single (Window_Height),
         0.1, 100.0, Projection_Matrix);

      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Uniforms.Set_Single (Model_ID, Model_Matrix);
      GL.Uniforms.Set_Single (View_ID, View_Matrix);
      GL.Uniforms.Set_Single (Projection_ID, Projection_Matrix);
      GL.Uniforms.Set_Int (Texture_1_ID, 0);
      GL.Uniforms.Set_Int (Texture_2_ID, 1);

      Vertex_Array.Bind;
      GL.Objects.Buffers.Draw_Elements (Triangles, 6, UInt_Type, 0);

   exception
      when  others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup is
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vector5_Buffer (Array_Buffer,
                                     Vertex_Data.Vertices, Static_Draw);

      Indices_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Indices_Buffer);
      Utilities.Load_Element_Buffer (Element_Array_Buffer,
                                     Vertex_Data.Indices, Static_Draw);

      Render_Program := Program_From
        ((Src ("src/shaders/6.1.coordinate_systems.vs", Vertex_Shader),
         Src ("src/shaders/6.1.coordinate_systems.fs", Fragment_Shader)));
      Model_ID := Uniform_Location (Render_Program, "model");
      View_ID := Uniform_Location (Render_Program, "view");
      Projection_ID := Uniform_Location (Render_Program, "projection");
      Texture_1_ID := Uniform_Location (Render_Program, "texture1");
      Texture_2_ID := Uniform_Location (Render_Program, "texture2");

      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, True, Stride, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, True,
                                               Stride, 3 * Skip_Size);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);

      Textures_61.Load_Texture (Texture_1,
                                "../resources/textures/container.jpg");
      Textures_61.Load_Texture (Texture_2,
                                "../resources/textures/awesomeface.png");

      Use_Program (Render_Program);
      GL.Uniforms.Set_Int (Texture_1_ID, 0);
      GL.Uniforms.Set_Int (Texture_2_ID, 1);

   exception
      when others =>
         Put_Line ("An exception occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
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
