
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

with Utilities;

with Ogldev_Basic_Lighting;
with Ogldev_Camera;
with Ogldev_Engine_Common;
with Ogldev_Lights_Common;
with Ogldev_Math;
with Ogldev_Pipeline;
with Ogldev_Texture;

with Billboard_List;
with Meshes_27;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer          : GL.Objects.Buffers.Buffer;
   Lighting_Technique     : Ogldev_Basic_Lighting.Basic_Lighting_Technique;
   Game_Camera            : Ogldev_Camera.Camera;
   Ground_Mesh            : Meshes_27.Mesh_27;
   Bricks_Texture         : Ogldev_Texture.Ogl_Texture;
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

         Ogldev_Lights_Common.Init_Directional_Light
           (Light          => Direct_Light,
            Amb_Intensity  => 0.2,
            Diff_Intensity => 0.8,
            theColour      => Ogldev_Lights_Common.Colour_White,
            Dir            => (1.0, 0.0, 0.0));

         --  The near plane should be between the camera and the target?
         --  or at the target?
         Ogldev_Math.Set_Perspective_Info (Info   => Perspective_Proj_Info,
                                           FOV    => 60.0,
                                           Width  => GL.Types.UInt (Window_Height),
                                           Height => GL.Types.UInt (Window_Width),
                                           Near   => 1.0,
                                           Far    => 100.0);
         Ogldev_Camera.Init_Camera (Game_Camera, Window,
                                    Camera_Position, Target, Up);

         Ogldev_Basic_Lighting.Use_Program (Lighting_Technique);
         Ogldev_Basic_Lighting.Set_Directional_Light_Location
              (Lighting_Technique, Direct_Light);
         Ogldev_Basic_Lighting.Set_Color_Texture_Unit_Location (Lighting_Technique, 0);

--           Buffers.Create_Vertex_Buffer (Vertex_Buffer);

         Meshes_27.Load_Mesh (Ground_Mesh, "src/quad.obj");
         Result := Billboard_List.Init ("../content/monster_hellknight.png");
         if Result then
                Result := Ogldev_Texture.Init_Texture
                  (Bricks_Texture, GL.Low_Level.Enums.Texture_2D, "../content/bricks.jpg");
                if Result then
                    Ogldev_Texture.Load (Bricks_Texture);
                    Ogldev_Texture.Bind (Bricks_Texture, Ogldev_Engine_Common.Colour_Texture_Unit);

                    if Result then
                        Result := Ogldev_Texture.Init_Texture
                          (Normal_Map, GL.Low_Level.Enums.Texture_2D, "../content/normal_map.jpg");
                        if Result then
                            Ogldev_Texture.Load (Normal_Map);
                        else
                            Put_Line ("Main_Loop.Init Normal_Map failed");
                        end if;
                    else
                        Put_Line ("Main_Loop.Init Bricks_Texture failed");
                    end if;
                else
                    Put_Line ("Main_Loop.Init Billboard_List.Init failed");
                end if;
            end if;
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
      Delay (0.2);
      Ogldev_Camera.Update_Camera (Game_Camera, Window);
      Utilities.Clear_Colour_Buffer_And_Depth;

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Ogldev_Basic_Lighting.Use_Program (Lighting_Technique);
      Ogldev_Texture.Bind (Bricks_Texture, Ogldev_Engine_Common.Colour_Texture_Unit);
      Ogldev_Texture.Bind (Normal_Map, Ogldev_Engine_Common.Normal_Texture_Unit);

      Ogldev_Pipeline.Set_Scale (Pipe, 20.0, 20.0, 1.0);
      Ogldev_Pipeline.Set_Rotation (Pipe, 90.0, 0.0, 0.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Ogldev_Camera.Get_Position (Game_Camera),
                                  Ogldev_Camera.Get_Target (Game_Camera),
                                  Ogldev_Camera.Get_Up (Game_Camera));
      Ogldev_Pipeline.Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);

      Ogldev_Basic_Lighting.Set_WVP_Location
        (Lighting_Technique, Ogldev_Pipeline.Get_WVP_Transform (Pipe));
      Ogldev_Basic_Lighting.Set_World_Matrix_Location
        (Lighting_Technique, Ogldev_Pipeline.Get_World_Transform (Pipe));

      Meshes_27.Render (Ground_Mesh);
      Billboard_List.Render (Ogldev_Pipeline.Get_VP_Transform (Pipe),
                             Ogldev_Camera.Get_Position (Game_Camera));
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
