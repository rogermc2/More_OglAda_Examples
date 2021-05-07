
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

with Vertex_Data;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Background         : constant GL.Types.Colors.Color := ((0.6, 0.6, 0.6, 1.0));
   Triangle_Program   : GL.Objects.Programs.Program;
   Triangle_VAO       : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Gradient_VAO       : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Triangle_Buffer    : GL.Objects.Buffers.Buffer;
   Gradient_Buffer    : GL.Objects.Buffers.Buffer;
   Colour_Buffer      : GL.Objects.Buffers.Buffer;
   Grad_Colour_Buffer : GL.Objects.Buffers.Buffer;

   --  ----------------------------------------------------------------------------

   procedure Draw_Gradient_Triangle is
   begin
      GL.Objects.Programs.Use_Program (Triangle_Program);
      Gradient_VAO.Bind;
      GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Triangles, 0, 3);

   end Draw_Gradient_Triangle;

   --  ----------------------------------------------------------------------------

   procedure Draw_Solid_Triangle is
   begin
      GL.Objects.Programs.Use_Program (Triangle_Program);
      Triangle_VAO.Bind;
      GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Triangles, 0, 3);

   end Draw_Solid_Triangle;

   --  ----------------------------------------------------------------------------

   procedure Setup_Graphic is
      use GL.Attributes;
      use GL.Types;
      use Program_Loader;
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use Vertex_Data;
   begin
      GL.Buffers.Set_Color_Clear_Value (Background);

      Triangle_Program := Program_From
        ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));

      Triangle_VAO.Initialize_Id;
      Gradient_VAO.Initialize_Id;

      GL.Objects.Programs.Use_Program (Triangle_Program);
      Triangle_VAO.Bind;
      Triangle_Buffer.Initialize_Id;
      Array_Buffer.Bind (Triangle_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Solid_Vertices, Static_Draw);
      Enable_Vertex_Attrib_Array (0);
      Set_Vertex_Attrib_Pointer (0, 2, Single_Type, False, 0, 0);

      Colour_Buffer.Initialize_Id;
      Array_Buffer.Bind (Colour_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Solid_Colours, Static_Draw);
      Enable_Vertex_Attrib_Array (1);
      Set_Vertex_Attrib_Pointer (1, 3, Single_Type, False, 0, 0);

      Gradient_VAO.Bind;
      Gradient_Buffer.Initialize_Id;
      Array_Buffer.Bind (Gradient_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Gradient_Vertices,
                                    Static_Draw);
      Enable_Vertex_Attrib_Array (0);
      Set_Vertex_Attrib_Pointer (0, 2, Single_Type, False, 0, 0);

      Grad_Colour_Buffer.Initialize_Id;
      Array_Buffer.Bind (Grad_Colour_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Gradient_Colours,
                                    Static_Draw);
      Enable_Vertex_Attrib_Array (1);
      Set_Vertex_Attrib_Pointer (1, 3, Single_Type, False, 0, 0);

exception
      when anError : others =>
         Put_Line ("An exception occurred in Setup_Graphic.");
         Put_Line (Exception_Information (anError));
         raise;
   end Setup_Graphic;

   --  ----------------------------------------------------------------------------

   procedure Update is
   begin
      Utilities.Clear_Colour;
      Draw_Solid_Triangle;
      Draw_Gradient_Triangle;
   end Update;

   --  ----------------------------------------------------------------------------

   use Glfw.Input;
   Running  : Boolean := True;
begin
   Setup_Graphic;
   while Running loop
      Update;
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;

exception
   when Program_Loader.Shader_Loading_Error =>
      -- message was already written to stdout
      null;
   when anError : others =>
      Put_Line ("An exception occurred in Main_Loop.");
      Put_Line (Exception_Information (anError));
      raise;
end Main_Loop;
