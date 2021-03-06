
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Culling;
with GL.Objects;
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

with Program_Loader;
with Utilities;

with Ogldev_Math;
with Ogldev_Pipeline;

with Buffers;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Background             : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 0.0);
   Shader_Program         : GL.Objects.Programs.Program;
   WVP_Location           : GL.Uniforms.Uniform;

   VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   IBO                    : GL.Objects.Buffers.Buffer;
   VBO                    : GL.Objects.Buffers.Buffer;
   Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;
   Scale                  : Single := 0.0;

   --  ------------------------------------------------------------------------

   function Build_Shader_Program return Boolean is
      use GL.Objects.Shaders;
      use Program_Loader;
      OK : Boolean := False;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders/shader.vs", Vertex_Shader),
         Src ("src/shaders/shader.fs", Fragment_Shader)));

      OK := GL.Objects.Programs.Link_Status (Shader_Program);
      if not OK then
         Put_Line ("Build_Shader_Program, Shader_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Shader_Program));
      else
         GL.Objects.Programs.Use_Program (Shader_Program);
         WVP_Location :=
           GL.Objects.Programs.Uniform_Location (Shader_Program, "gWVP");
      end if;
      return OK;

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Build_Shader_Program.");
         raise;
   end Build_Shader_Program;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window; Result : out Boolean) is

      Window_Width        : Glfw.Size;
      Window_Height       : Glfw.Size;
   begin
      Result := Build_Shader_Program;
      if Result then
         VAO.Initialize_Id;
         VAO.Bind;

         Window.Get_Framebuffer_Size (Window_Width, Window_Height);
         Utilities.Clear_Background_Colour (Background);
         GL.Culling.Set_Front_Face (Counter_Clockwise);
         GL.Culling.Set_Cull_Face (GL.Culling.Back);
         GL.Toggles.Enable (GL.Toggles.Cull_Face);

         Buffers.Create_Vertex_Buffer (VBO);
         Buffers.Create_Index_Buffer (IBO);


         Ogldev_Math.Set_Perspective_Info (Info   => Perspective_Proj_Info,
                                           FOV    => 60.0,
                                           Width  => GL.Types.UInt (Window_Width),
                                           Height => GL.Types.UInt (Window_Height),
                                           Near   => 1.0, Far    => 100.0);

         Window.Set_Input_Toggle (Glfw.Input.Sticky_Keys, True);
         Window.Set_Cursor_Mode (Glfw.Input.Mouse.Disabled);
         Glfw.Input.Poll_Events;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   procedure Render_Scene (Window : in out Glfw.Windows.Window) is
      Window_Width         : Glfw.Size;
      Window_Height        : Glfw.Size;
      Pipe                 : Ogldev_Pipeline.Pipeline;
      Camera_Position      : constant GL.Types.Singles.Vector3 := (1.0, 1.0, 3.0);
      Camera_Target        : constant GL.Types.Singles.Vector3 := (0.0, 0.0, 2.0);
      Camera_Up            : constant GL.Types.Singles.Vector3 := (0.0, 1.0, 0.0);
   begin
      Utilities.Clear_Background_Colour_And_Depth (Background);
      Scale := Scale + 0.2;

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Ogldev_Math.Set_Perspective_Width
        (Perspective_Proj_Info, GL.Types.UInt (Window_Width));
      Ogldev_Math.Set_Perspective_Height
        (Perspective_Proj_Info, GL.Types.UInt (Window_Height));

      Ogldev_Pipeline.Set_Rotation (Pipe, 0.0, Scale, 0.0);
      Ogldev_Pipeline.Set_World_Position (Pipe, 0.0, 0.0, -3.0);
      Ogldev_Pipeline.Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Set_Camera (Pipe, Camera_Position, Camera_Target, Camera_Up);
      Ogldev_Pipeline.Init_Transforms (Pipe);

      GL.Uniforms.Set_Single (WVP_Location, Ogldev_Pipeline.Get_WVP_Transform (Pipe));
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

      GL.Objects.Buffers.Array_Buffer.Bind (VBO);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                               Kind   => Single_Type,
                                               Stride => 0, Offset => 0);

      GL.Objects.Buffers.Element_Array_Buffer.Bind (IBO);

      GL.Objects.Buffers.Draw_Elements (Triangles, 12, UInt_Type, 0);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Scene.");
         raise;
   end Render_Scene;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean;
begin
   Init (Main_Window, Running);
   while Running loop
      Render_Scene (Main_Window);
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
