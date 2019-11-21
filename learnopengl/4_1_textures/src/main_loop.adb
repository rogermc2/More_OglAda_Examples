

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
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

with Textures_41;
with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is

   Vertex_Array   : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer  : GL.Objects.Buffers.Buffer;
   Indices_Buffer : GL.Objects.Buffers.Buffer;
   theTexture     : GL.Objects.Textures.Texture;
   Render_Program : GL.Objects.Programs.Program;

   --  ------------------------------------------------------------------------

   procedure Render  (Window  : in out Glfw.Windows.Window) is
      use GL.Types;
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      Background    : constant GL.Types.Colors.Color := (0.2, 0.3, 0.3, 1.0);
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Background_Colour (Background);
      GL.Objects.Programs.Use_Program (Render_Program);

      Texture_2D.Bind (theTexture);
      Array_Buffer.Bind (Vertex_Buffer);

      GL.Objects.Buffers.Draw_Elements (Triangles, 6, UInt_Type, 0);

   exception
      when  others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup is
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use GL.Types;
      use Program_Loader;
      Stride : constant Int := Maths.Vector8'Size / 8;
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vector8_Buffer (Array_Buffer,
                                     Vertex_Data.Vertices, Static_Draw);

      Indices_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Indices_Buffer);
      Utilities.Load_Element_Buffer (Element_Array_Buffer,
                                     Vertex_Data.Indices, Static_Draw);

      Render_Program := Program_From
        ((Src ("src/shaders/4.1.texture.vs", Vertex_Shader),
         Src ("src/shaders/4.1.texture.fs", Fragment_Shader)));

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, True, Stride, 0);

      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, True, Stride, 3);

      GL.Attributes.Enable_Vertex_Attrib_Array (2);
      GL.Attributes.Set_Vertex_Attrib_Pointer (2, 2, Single_Type, True, Stride, 6);

      Textures_41.Load_Texture (theTexture,
                                "../resources/textures/container.jpg");

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
