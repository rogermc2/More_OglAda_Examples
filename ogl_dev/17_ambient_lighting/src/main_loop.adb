
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Culling;
with GL.Low_Level.Enums;
with GL.Objects;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
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

with Ogldev_Camera;
with Ogldev_Math;
with Ogldev_Pipeline;
with Ogldev_Texture;

with Buffers;
with Lighting_Technique_17;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Background             : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 0.0);
   Shader_Program         : GL.Objects.Programs.Program;

   VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Indices_Buffer         : GL.Objects.Buffers.Buffer;
   Vertex_Buffer          : GL.Objects.Buffers.Buffer;
   Texture_Buffer         : GL.Objects.Buffers.Buffer;
   Game_Camera            : Ogldev_Camera.Camera;
   theTexture             : Ogldev_Texture.Ogl_Texture;
   Direct_Light           : Lighting_Technique_17.Directional_Light;
   Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;
   Scale                  : Single := 0.0;

   procedure Update_Lighting_Intensity (Window : in out Glfw.Windows.Window);

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window; Result : out Boolean) is
      use Lighting_Technique_17;
      Window_Width        : Glfw.Size;
      Window_Height       : Glfw.Size;
      Position            : Singles.Vector3 := (0.0, 0.0, 3.0); --  Normalized by Camera.Init
      Target              : Singles.Vector3 := (0.0, 0.0, -1.0);  --  Normalized by Camera.Init
      Up                  : Singles.Vector3 := (0.0, 1.0, 0.0);
   begin
      Result := Lighting_Technique_17.Init (Shader_Program);
      if Result then
         VAO.Initialize_Id;
         VAO.Bind;

         Window.Get_Framebuffer_Size (Window_Width, Window_Height);
         Ogldev_Camera.Init_Camera (Game_Camera, Window, Position, Target, Up);
         Utilities.Clear_Background_Colour (Background);

         Buffers.Create_Buffers (Vertex_Buffer, Indices_Buffer);

         GL.Objects.Programs.Use_Program (Shader_Program);

         Set_Texture_Unit (0);
         if  Ogldev_Texture.Init_Texture (theTexture, GL.Low_Level.Enums.Texture_2D,
                                          "../content/test.png") then
            Ogldev_Texture.Load (theTexture);
         else
            Put_Line ("Main_Loop.Init test.png failed to load");
         end if;

         Set_Directional_Colour (Direct_Light, (1.0, 1.0, 1.0));
         Set_Directional_Ambient (Direct_Light, 0.5);

         Ogldev_Math.Set_Perspective_FOV (Perspective_Proj_Info, 60.0);
         Ogldev_Math.Set_Perspective_Height (Perspective_Proj_Info, GL.Types.UInt (Window_Height));
         Ogldev_Math.Set_Perspective_Width (Perspective_Proj_Info, GL.Types.UInt (Window_Width));
         Ogldev_Math.Set_Perspective_Near (Perspective_Proj_Info, 1.0);
         Ogldev_Math.Set_Perspective_Far (Perspective_Proj_Info, 100.0);

         GL.Culling.Set_Front_Face (Counter_Clockwise);
         GL.Culling.Set_Cull_Face (GL.Culling.Back);
         GL.Toggles.Enable (GL.Toggles.Cull_Face);
         Window.Set_Input_Toggle (Glfw.Input.Sticky_Keys, True);
         Window.Set_Cursor_Mode (Glfw.Input.Mouse.Disabled);
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
   begin
      Update_Lighting_Intensity (Window);
      Ogldev_Camera.Update_Camera (Game_Camera, Window);
      Utilities.Clear_Colour_Buffer_And_Depth;
      Scale := Scale + 0.02;

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Ogldev_Math.Set_Perspective_Width (Perspective_Proj_Info, GL.Types.UInt (Window_Width));
      Ogldev_Math.Set_Perspective_Height (Perspective_Proj_Info, GL.Types.UInt (Window_Height));

      Ogldev_Pipeline.Set_Scale (Pipe, 1.0);
      Ogldev_Pipeline.Set_Rotation (Pipe, 0.0, Scale, 0.0);
      Ogldev_Pipeline.Set_World_Position (Pipe, 0.0, 0.0, 3.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Game_Camera);
      Ogldev_Pipeline.Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);

      Lighting_Technique_17.Set_WVP (Ogldev_Pipeline.Get_WVP_Transform (Pipe));
      Lighting_Technique_17.Set_Directional_Light_Locations (Direct_Light);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);

      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);

      --  First attribute buffer : Vertices
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 5, 0);

      --  Second attribute buffer : Textures
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 5, 3);

      GL.Objects.Buffers.Element_Array_Buffer.Bind (Indices_Buffer);

      GL.Objects.Textures.Set_Active_Unit (0);
      GL.Objects.Buffers.Draw_Elements (Triangles, 12, UInt_Type, 0);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Scene.");
         raise;
   end Render_Scene;

   --  ------------------------------------------------------------------------

   procedure Update_Lighting_Intensity (Window : in out Glfw.Windows.Window) is
      use Glfw.Input;
      use Lighting_Technique_17;
   begin
      if Window'Access.Key_State (Keys.A) = Pressed then
         Set_Directional_Ambient (Direct_Light,
                                  Get_Directional_Ambient (Direct_Light) + 0.001);
      elsif Window'Access.Key_State (Keys.S) = Pressed then
         Set_Directional_Ambient (Direct_Light,
                                  Get_Directional_Ambient (Direct_Light) - 0.001);
      end if;
   end Update_Lighting_Intensity;

   --  -------------------------------------------------------------------------

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
