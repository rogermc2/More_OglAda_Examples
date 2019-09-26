
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;
with GL.Objects;
with GL.Objects.Buffers;with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Types.Colors;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

--  with Maths;
with Utilities;

with Ogldev_Basic_Lighting;
with Ogldev_Camera;
with Ogldev_Lights_Common;
with Ogldev_Math;
with Ogldev_Pipeline;
with Ogldev_Texture;

with Buffers;
with Meshes_27;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Background             : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 0.0);

   VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer          : GL.Objects.Buffers.Buffer;
   Lighting_Technique     : Ogldev_Basic_Lighting.Basic_Lighting_Technique;
   Game_Camera            : Ogldev_Camera.Camera;
   Ground_Mesh            : Meshes_27.Mesh_27;
   theTexture             : Ogldev_Texture.Ogl_Texture;
   Normal_Map             : Ogldev_Texture.Ogl_Texture;
   Direct_Light           : Ogldev_Lights_Common.Directional_Light;
   Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;
   Scale                  : Single := 0.0;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window; Result : out Boolean) is

      Window_Width   : Glfw.Size;
      Window_Height  : Glfw.Size;
      Camera_Position : constant Singles.Vector3 := (0.0, 1.0, 1.0);   --  Normalized by Camera.Init
      Target          : constant Singles.Vector3 := (0.0, -0.5, 1.0);  --  Normalized by Camera.Init
      Up              : constant Singles.Vector3 := (0.0, 1.0, 0.0);
   begin
      Result :=
        Ogldev_Basic_Lighting.Init (Lighting_Technique);
      if Result then
         VAO.Initialize_Id;
         VAO.Bind;

         Window.Get_Framebuffer_Size (Window_Width, Window_Height);
         Utilities.Clear_Background_Colour_And_Depth (Background);

         Ogldev_Lights_Common.Init_Directional_Light
           (Light          => Direct_Light,
            Amb_Intensity  => 0.2,
            Diff_Intensity => 0.8,
            theColour      => Ogldev_Lights_Common.Colour_White,
            Dir            => (1.0, 0.0, 0.0));

         Ogldev_Math.Set_Perspective_FOV (Perspective_Proj_Info, 60.0);
         Ogldev_Math.Set_Perspective_Height (Perspective_Proj_Info, GL.Types.UInt (Window_Height));
         Ogldev_Math.Set_Perspective_Width (Perspective_Proj_Info, GL.Types.UInt (Window_Width));
         --  The near plane should be between the camera and the target?
         --  or at the target?
         Ogldev_Math.Set_Perspective_Near (Perspective_Proj_Info, 1.0);
         Ogldev_Math.Set_Perspective_Far (Perspective_Proj_Info, 100.0);

         Ogldev_Camera.Init_Camera (Game_Camera, Window,
                                    Camera_Position, Target, Up);
         Ogldev_Basic_Lighting.Use_Program (Lighting_Technique);
         Ogldev_Basic_Lighting.Set_Directional_Light_Location
           (Lighting_Technique, Direct_Light);
         Ogldev_Basic_Lighting.Set_Color_Texture_Unit_Location (Lighting_Technique, 0);

         Buffers.Create_Vertex_Buffer (Vertex_Buffer);

         Meshes_27.Load_Mesh (Ground_Mesh, "quad.obj");
         Result := Ogldev_Texture.Init_Texture
           (theTexture, GL.Low_Level.Enums.Texture_2D, "../content/test.png");
         if Result then
            Ogldev_Texture.Load (theTexture);
         else
            Put_Line ("Main_Loop.Init. Init_Texture failed");
         end if;

         Glfw.Input.Poll_Events;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   procedure Render_Scene (Window : in out Glfw.Windows.Window) is
--        use GL.Types.Singles;
--        use Maths.Single_Math_Functions;
      Window_Width         : Glfw.Size;
      Window_Height        : Glfw.Size;
      Pipe                 : Ogldev_Pipeline.Pipeline;
   begin
      Scale := Scale + 0.0057;
      Ogldev_Camera.Update_Camera (Game_Camera, Window);
      Utilities.Clear_Background_Colour (Background);

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));

      Ogldev_Pipeline.Set_World_Position (Pipe, 0.0, 0.0, -1.0);  --  orig 0,0,1
      Ogldev_Pipeline.Set_Camera (Pipe, Ogldev_Camera.Get_Position (Game_Camera),
                                  Ogldev_Camera.Get_Target (Game_Camera),
                                  Ogldev_Camera.Get_Up (Game_Camera));
      Ogldev_Pipeline.Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);

      Ogldev_Basic_Lighting.Set_WVP_Location
        (Lighting_Technique, Ogldev_Pipeline.Get_WVP_Transform (Pipe));

      Ogldev_Basic_Lighting.Set_World_Matrix_Location
        (Lighting_Technique, Ogldev_Pipeline.Get_World_Transform (Pipe));
      Ogldev_Basic_Lighting.Set_Directional_Light_Location
        (Lighting_Technique, Direct_Light);
      Ogldev_Basic_Lighting.Set_Eye_World_Pos_Location
        (Lighting_Technique, Ogldev_Camera.Get_Position (Game_Camera));
      Ogldev_Basic_Lighting.Set_Specular_Intensity_Location (Lighting_Technique, 0.0);
      Ogldev_Basic_Lighting.Set_Specular_Power_Location (Lighting_Technique, 0);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Enable_Vertex_Attrib_Array (2);

      --  First attribute buffer : Vertices
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 8, 0);
      --  Second attribute buffer : Textures
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 8, 3);
      --  Third attribute buffer : Normals
      GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 8, 5);

      GL.Objects.Textures.Set_Active_Unit (0);
      GL.Objects.Textures.Targets.Texture_2D.Bind (theTexture.Texture_Object);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 6);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);
      GL.Attributes.Disable_Vertex_Attrib_Array (2);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Scene.");
         raise;
   end Render_Scene;

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
