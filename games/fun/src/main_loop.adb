
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Toggles;
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
   Rendering_Program  : GL.Objects.Programs.Program;
   Points_VAO         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Lines_VAO          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer      : GL.Objects.Buffers.Buffer;
--     Num_Point_Bytes    : GL.Types.Int;
   --      Num_Dimension      : constant GL.Types.Int := 2;

   --  ----------------------------------------------------------------------------

   procedure Draw_Lines is
--        use GL.Types;
--        Num_Points    : constant Int := 3;
   begin
      --        GL.Objects.Vertex_Arrays.Draw_Arrays (Lines, Num_Point_Bytes, Num_Points);
      null;
   exception
      when anError : others =>
         Put_Line ("An exceptiom occurred in Draw_Lines.");
         Put_Line (Exception_Information (anError));
         raise;
   end Draw_Lines;

   --  ----------------------------------------------------------------------------

   procedure Draw_Points is
      use GL.Types;
      Num_Points    : constant Int := 3;
   begin
      -- Point size is set in the vertex shader
      GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, Num_Points);
      GL.Toggles.Disable (GL.Toggles.Vertex_Program_Point_Size);

   exception
      when anError : others =>
         Put_Line ("An exceptiom occurred in Draw_Points.");
         Put_Line (Exception_Information (anError));
         raise;
   end Draw_Points;

   --  ----------------------------------------------------------------------------

   procedure Setup_Graphic is
      use GL.Attributes;
      use GL.Types;
      use Program_Loader;
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use Vertex_Data;
      Point_Bytes : constant Long := Points_Data'Size / 8;
      Line_Bytes  : constant Long := Lines_Data'Size / 8;
      Buffer_Size : constant Long := Point_Bytes + Line_Bytes;
      Stride      : constant Int := Singles.Vector2'Size / 8;
   begin
--        Num_Point_Bytes := Int (Point_Bytes);
      GL.Buffers.Set_Color_Clear_Value (Background);
      Rendering_Program := Program_From
        ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));
      GL.Objects.Programs.Use_Program (Rendering_Program);

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Array_Buffer.Allocate (Buffer_Size, Static_Draw);
      Utilities.Load_Vertex_Sub_Buffer (Array_Buffer, 0, Points_Data);
      Utilities.Load_Vertex_Sub_Buffer (Array_Buffer, Int (Point_Bytes),
                                        Lines_Data);

      Points_VAO.Initialize_Id;
      Lines_VAO.Initialize_Id;

      Points_VAO.Bind;
      Enable_Vertex_Attrib_Array (0);
      Enable_Vertex_Attrib_Array (1);
      Array_Buffer.Bind (Vertex_Buffer);
      Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, Stride, 0);
      Set_Vertex_Attrib_Pointer (1, 4, Single_Type, False, Stride, Int (Point_Bytes));

      Lines_VAO.Bind;
      Enable_Vertex_Attrib_Array (0);
      Enable_Vertex_Attrib_Array (1);
      Array_Buffer.Bind (Vertex_Buffer);
      Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, Stride, 0);
      Set_Vertex_Attrib_Pointer (1, 4, Single_Type, False, Stride, Int (Point_Bytes));
   exception
      when anError : others =>
         Put_Line ("An exceptiom occurred in Setup_Graphic.");
         Put_Line (Exception_Information (anError));
         raise;
   end Setup_Graphic;

   --  ----------------------------------------------------------------------------

   procedure Update is
   begin
      Utilities.Clear_Colour;
      Draw_Points;
      Draw_Lines;

   exception
      when anError : others =>
         Put_Line ("An exceptiom occurred in Update.");
         Put_Line (Exception_Information (anError));
         raise;
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
      Put_Line ("An exceptiom occurred in Setup_Graphic.");
      Put_Line (Exception_Information (anError));
      raise;
end Main_Loop;
