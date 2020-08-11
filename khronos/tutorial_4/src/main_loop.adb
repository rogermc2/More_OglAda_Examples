
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Toggles;
with GL.Uniforms;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with Vertex_Data;

--  Main_Loop produces a 512x512 window centered on the display
--  showing a rotating tetrahedron
procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is

   Vertex_Array        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer       : GL.Objects.Buffers.Buffer;
   Indices_Buffer      : GL.Objects.Buffers.Buffer;
   Colour_Buffer       : GL.Objects.Buffers.Buffer;
   Model_Matrix        : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
   Projection_Matrix   : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
   Render_Program      : GL.Objects.Programs.Program;
   MVP_Uniform         : GL.Uniforms.Uniform;

   Background : constant GL.Types.Colors.Color := (0.6, 0.6, 0.6, 1.0);

   --  ------------------------------------------------------------------------

   procedure Draw_Scene (Index : Maths.Degree) is
      use Maths;
      use GL.Objects.Buffers;
      use GL.Types.Singles;
   begin
      GL.Objects.Programs.Use_Program (Render_Program);
      Utilities.Clear_Background_Colour (Background);

      Model_Matrix := Identity4;
      Model_Matrix := Rotation_Matrix (-Index, (1.0, 0.0, 0.0)) * Model_Matrix;
      Model_Matrix := Rotation_Matrix (Index, (0.0, 1.0, 0.0)) * Model_Matrix;
      Model_Matrix := Rotation_Matrix (0.5 * Index, (0.0, 0.0, 1.0)) * Model_Matrix;
--        Model_Matrix := Scaling_Matrix ((3.0, 3.0, 3.0)) * Model_Matrix;
      Model_Matrix := Translation_Matrix ((0.0, 0.0, -2.5)) * Model_Matrix;
      Model_Matrix := Projection_Matrix * Model_Matrix;
      GL.Uniforms.Set_Single (MVP_Uniform, Model_Matrix);

      Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, 0, 0);

      Array_Buffer.Bind (Colour_Buffer);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, False, 0, 0);

      GL.Objects.Buffers.Draw_Elements (Triangle_Strip, 6, UInt_Type, 0);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);

   exception
      when  others =>
         Put_Line ("An exceptiom occurred in Draw_Scene.");
         raise;
   end Draw_Scene;

   --  ------------------------------------------------------------------------

   procedure Setup is
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Render_Program := Program_From
        ((Src ("src/shaders/tutorial4.vert", Vertex_Shader),
         Src ("src/shaders/tutorial4.frag", Fragment_Shader),
         Src ("src/shaders/tutorial4.geom", Geometry_Shader)));
      Utilities.Set_Uniform_Location (Render_Program, "mvp_matrix",
                                      MVP_Uniform);

      GL.Toggles.Enable (GL.Toggles.Multisample);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      --  Enable Z depth testing so objects closest to the viewpoint
      --  are in front of objects further away
      --  (Never, Less, Equal, LEqual, Greater, Not_Equal, GEqual, Always)
      GL.Buffers.Set_Depth_Function (Not_Equal);

      Use_Program (Render_Program);

      Projection_Matrix :=
        Maths.Perspective_Matrix (Maths.Degrees (65.0), 1.0, 0.1, -100.0);

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vertex_Buffer
        (Array_Buffer, Vertex_Data.Tetrahedron_Vertices, Static_Draw);

      Indices_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Indices_Buffer);
      Utilities.Load_Element_Buffer
        (Element_Array_Buffer, Vertex_Data.Tetrahedron_Indices, Static_Draw);

      Colour_Buffer.Initialize_Id;
      Array_Buffer.Bind (Colour_Buffer);
      Utilities.Load_Vertex_Buffer
        (Array_Buffer, Vertex_Data.Colour_Data, Static_Draw);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   use Maths;
   Running : Boolean := True;
   Index   : Maths.Degree := 0.0;
begin
   Setup;
   while Running loop
      Draw_Scene (Index);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Delay (0.05);
      Index := Index + 1.0;
      Running := Running and Index < 360.0 and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;
exception
   when others =>
      Put_Line ("An exceptiom occurred in Main_Loop.");
      raise;
end Main_Loop;
