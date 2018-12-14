
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with Buffers;
with Teapot_Data;
with MT_Teapot;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Vertex_Array_Size  : GL.Types.Int := Teapot_Data.Num_Patchs * MT_Teapot.Res_U * MT_Teapot.Res_V;
   Element_Array_Size : GL.Types.Int
     := 2 * 3 * Teapot_Data.Num_Patchs * (MT_Teapot.Res_U - 1) * (MT_Teapot.Res_V - 1);

   Shader_Program   : GL.Objects.Programs.Program;
   MVP_Location     : GL.Uniforms.Uniform;
   VAO              : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Indices_Buffer   : GL.Objects.Buffers.Buffer;
   CP_Indices_Buffer   : GL.Objects.Buffers.Buffer;
   Vertices_Buffer  : GL.Objects.Buffers.Buffer;
   CP_Vertices_Buffer : GL.Objects.Buffers.Buffer;
   Colours_Buffer   : GL.Objects.Buffers.Buffer;
   CP_Colours_Buffer   : GL.Objects.Buffers.Buffer;
   Teapot_Colours   :  GL.Types.Singles.Vector3_Array (1 .. Vertex_Array_Size);
   CP_Teapot_Colours   :  GL.Types.Singles.Vector3_Array (1 .. 269);
   Teapot_Vertices  :  GL.Types.Singles.Vector3_Array (1 .. Vertex_Array_Size);
   Teapot_Elements :  GL.Types.Int_Array (1 .. Element_Array_Size);
   CP_Teapot_Elements : Buffers.CP_Element_Array;

   Background       : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 0.0);

   procedure Update_Lighting_Intensity (Window : in out Glfw.Windows.Window);

   --  ------------------------------------------------------------------------

   function Build_Shader_Program return Boolean is
      use GL.Objects.Shaders;
      use Program_Loader;
      OK : Boolean := False;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders/teapot.vs", Vertex_Shader),
         Src ("src/shaders/teapot.fs", Fragment_Shader)));

      OK := GL.Objects.Programs.Link_Status (Shader_Program);
      if not OK then
         Put_Line ("Build_Shader_Program, Shader_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Shader_Program));
      else
         GL.Objects.Programs.Use_Program (Shader_Program);
         MVP_Location :=
           GL.Objects.Programs.Uniform_Location (Shader_Program, "mvp");
      end if;
      return OK;

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Build_Shader_Program.");
         raise;
   end Build_Shader_Program;

   --  ------------------------------------------------------------------------

   procedure Display (Window : in out Glfw.Windows.Window)is
      Window_Width   : Glfw.Size;
      Window_Height  : Glfw.Size;
   begin
--        Update_Lighting_Intensity (Window);
--           Put_Line ("Main_Loop.Init Lighting_Intensity updated.");
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));

      Utilities.Clear_Colour_Buffer_And_Depth;
--        Render_Teapot (8.0, 8.0, 0.5, 0.0, 0.0, 0.1,
--                       0.1, 0.6, 0.9, 0.1, 0.1, 0.8);
   end Display;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window) is
      Ambient        : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
      Diffuse        : constant GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
      Specular       : constant GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
      Position       : constant GL.Types.Singles.Vector4 := (-6.0, -3.0, 3.0, 0.0);
      Local_View     : constant GL.Types.Single := 0.0;
      Local_Ambient  : constant GL.Types.Colors.Color := (0.2, 0.2, 0.2, 1.0);
      Window_Width   : Glfw.Size;
      Window_Height  : Glfw.Size;
      Result         : Boolean;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Utilities.Clear_Background_Colour (Background);
      Result := Build_Shader_Program;
      if Result then
         VAO.Initialize_Id;
         VAO.Bind;
         Buffers.Create_Vertex_Buffer (Vertices_Buffer, Teapot_Vertices);
         Buffers.Create_Vertex_Buffer (Colours_Buffer, Teapot_Colours);
         Buffers.Create_Index_Buffer (Indices_Buffer, Teapot_Elements);
         Buffers.Create_Vertex_Buffer (CP_Vertices_Buffer, Teapot_Data.CP_Vertices);
         Buffers.Create_Vertex_Buffer (CP_Colours_Buffer, CP_Teapot_Colours);
         Buffers.Create_CP_Index_Buffer (CP_Indices_Buffer, CP_Teapot_Elements);
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

    procedure Update_Lighting_Intensity (Window : in out Glfw.Windows.Window) is
      use Glfw.Input;
   begin
      null;
--        if Window'Access.Key_State (Keys.A) = Pressed then
--           Light_Direction.Ambient_Intensity := Light_Direction.Ambient_Intensity + 0.05;
--        elsif Window'Access.Key_State (Keys.S) = Pressed then
--           Light_Direction.Ambient_Intensity := Light_Direction.Ambient_Intensity - 0.05;
--        elsif Window'Access.Key_State (Keys.Z) = Pressed then
--           Light_Direction.Diffuse_Intensity := Light_Direction.Diffuse_Intensity + 0.05;
--        elsif Window'Access.Key_State (Keys.X) = Pressed then
--           Light_Direction.Diffuse_Intensity := Light_Direction.Diffuse_Intensity  - 0.05;
--        end if;
   end Update_Lighting_Intensity;

   --  -------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
      Put_Line (" Main_Loop calling init.");
   Init (Main_Window);
   while Running loop
      Put_Line (" Main_Loop calling Display.");
      Display (Main_Window);
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
