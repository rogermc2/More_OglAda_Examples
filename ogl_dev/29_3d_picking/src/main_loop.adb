
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Low_Level.Enums;
with GL.Objects;
with GL.Objects.Framebuffers;
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

with Ogldev_Basic_Lighting;
with Ogldev_Camera;
with Ogldev_Engine_Common;
with Ogldev_Lights_Common;
with Ogldev_Math;
with Ogldev_Pipeline;

with Meshes_29;
with Picking_Technique;
with Picking_Texture;
with Simple_Colour_Technique;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   type Mouse_Status is record
      X              : GL.Types.Int := 0;
      Y              : GL.Types.Int := 0;
      Button_Pressed : Boolean := False;
   end record;

   Background             : constant GL.Types.Colors.Color := (0.7, 0.7, 0.7, 0.0);
   VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Window_Width           : constant GL.Types.Int := 1680;
   Window_Height          : constant GL.Types.Int := 1050;
   Lighting_Technique     : Ogldev_Basic_Lighting.Basic_Lighting_Technique;
   Picking_Effect         : Picking_Technique.Pick_Technique;
   Colour_Effect          : Simple_Colour_Technique.Colour_Technique;
   Game_Camera            : Ogldev_Camera.Camera;
   Dir_Light              : Ogldev_Lights_Common.Directional_Light;
   Mesh                   : Meshes_29.Mesh_29;
   Pick_Texture           : Picking_Texture.Pick_Texture;
   Mouse_Button           : Mouse_Status;
   Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;
   World_Position         : GL.Types.Singles.Vector3_Array (1 .. 2) :=
                              ((-10.0, 0.0, 5.0),
                               (10.0, 0.0, 5.0));
   First                  : Boolean := True;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window) is
      Position            : constant Singles.Vector3 := (5.0, 5.0, -22.0);  --  0.0, 5.0, -22.0
      Target              : constant Singles.Vector3 := (0.0, -0.2, 1.0);
      Up                  : constant Singles.Vector3 := (0.0, 1.0, 0.0);
   begin
      VAO.Initialize_Id;
      VAO.Bind;
      Ogldev_Camera.Init_Camera (Game_Camera, Window_Width, Window_Height,
                                 Position, Target, Up);

      Ogldev_Lights_Common.Init_Directional_Light
        (Light          => Dir_Light,
         Amb_Intensity  => 1.0,
         Diff_Intensity => 0.01,
         theColour      => (1.0, 1.0, 1.0),
         Dir            => (1.0, -1.0, 0.0));

      Ogldev_Math.Set_Perspective_Info
        (Perspective_Proj_Info, 60.0, UInt (Window_Width), UInt (Window_Height),
         1.0, 100.0);

      if Ogldev_Basic_Lighting.Init (Lighting_Technique) then
         GL.Objects.Programs.Use_Program
           (Ogldev_Basic_Lighting.Lighting_Program (Lighting_Technique));
         Ogldev_Basic_Lighting.Set_Color_Texture_Unit_Location
           (Lighting_Technique, UInt (Ogldev_Engine_Common.Colour_Texture_Unit));
         Ogldev_Basic_Lighting.Set_Directional_Light_Location
           (Lighting_Technique, Dir_Light);

         Picking_Texture.Init (Pick_Texture, Window_Width, Window_Height);
         Picking_Technique.Init (Picking_Effect);
         Picking_Technique.Use_Program (Picking_Effect);
         Simple_Colour_Technique.Init (Colour_Effect);
         Simple_Colour_Technique.Use_Program (Colour_Effect);

        GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);
         Meshes_29.Load_Mesh (Mesh, "../content/spider.obj");
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   procedure Mouse_Handler (Window : in out Glfw.Windows.Window) is
      use Glfw.Input;
      use Mouse;
      use Maths;
      Cursor_X       : Coordinate;
      Cursor_Y       : Coordinate;
   begin
      Window'Access.Get_Cursor_Pos (Cursor_X, Cursor_Y);
      Mouse_Button.X := Int (Cursor_X);
      Mouse_Button.Y := Int (Cursor_Y);
      Mouse_Button.Button_Pressed :=
        Window'Access.Mouse_Button_State (Left_Button) = Pressed or
        Window'Access.Mouse_Button_State (Middle_Button) = Pressed or
        Window'Access.Mouse_Button_State (Right_Button) = Pressed;
   end Mouse_Handler;

   --  ------------------------------------------------------------------------

   procedure Picking_Phase (Window : in out Glfw.Windows.Window) is
      use Ogldev_Camera;
      use Ogldev_Basic_Lighting;
      Pipe             : Ogldev_Pipeline.Pipeline;
   begin
      Utilities.Clear_Colour_Buffer_And_Depth;
      Utilities.Clear_Background_Colour_And_Depth (Background);

      Picking_Technique.Use_Program (Picking_Effect);
      Ogldev_Pipeline.Set_Scale (Pipe, 0.1, 0.1, 0.1);
      Ogldev_Pipeline.Set_Rotation (Pipe, 0.0, 90.0, 0.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Get_Position (Game_Camera),
                                  Get_Target (Game_Camera), Get_Up (Game_Camera));
      Ogldev_Pipeline.Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);

      Picking_Texture.Enable_Writing (Pick_Texture);
      for index in Int range 1 .. World_Position'Length loop
         Ogldev_Pipeline.Set_World_Position (Pipe, World_Position (index));
         Picking_Technique.Set_WVP (Picking_Effect, Ogldev_Pipeline.Get_WVP_Transform (Pipe));
--           GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);
         Meshes_29.Render (Mesh);
      end loop;
      Picking_Texture.Disable_Writing;

   end Picking_Phase;

   --  ------------------------------------------------------------------------

   procedure Render_Phase (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Framebuffers;
      use GL.Types.Singles;
      use Ogldev_Basic_Lighting;
      Pipe           : Ogldev_Pipeline.Pipeline;
      Pixel_Data     : Picking_Texture.Pixel_Info;
      PT_Object_ID   : Single;
      PT_Draw_ID     : Single;
      PT_Prim_ID     : Single;
   begin
      Utilities.Clear_Colour_Buffer_And_Depth;
      Utilities.Clear_Background_Colour_And_Depth (Background);
      Draw_Target.Bind (Default_Framebuffer);

      Ogldev_Pipeline.Set_Scale (Pipe, 0.1, 0.1, 0.1);
      Ogldev_Pipeline.Set_Rotation (Pipe, 0.0, 90.0, 0.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Ogldev_Camera.Get_Position (Game_Camera),
                                  Ogldev_Camera.Get_Target (Game_Camera),
                                  Ogldev_Camera.Get_Up (Game_Camera));
      Ogldev_Pipeline.Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);

      if Mouse_Button.Button_Pressed then
         Put_Line ("Main_Loop.Render_Phase, Mouse_Button.Button_Pressed ");
         Pixel_Data := Picking_Texture.Read_Pixel
           (Window, Pick_Texture, Mouse_Button.X,
            Int (Window_Height) - Mouse_Button.Y - 1);
         PT_Prim_ID := Picking_Texture.Prim_ID (Pixel_Data);
         if PT_Prim_ID /= 0.0 then
            Simple_Colour_Technique.Use_Program (Colour_Effect);
            PT_Object_ID := Picking_Texture.Object_ID (Pixel_Data);
            if Integer (PT_Object_ID) /= World_Position'Length then
               PT_Draw_ID := Picking_Texture.Draw_ID (Pixel_Data);
               Ogldev_Pipeline.Set_World_Position
                 (Pipe, World_Position (Int (PT_Object_ID)));
               Simple_Colour_Technique.Set_WVP
                 (Colour_Effect, Ogldev_Pipeline.Get_WVP_Transform (Pipe));
               Simple_Colour_Technique.Use_Program (Colour_Effect);
               GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);
               Meshes_29.Render (Mesh, UInt (PT_Draw_ID), UInt (PT_Prim_ID) - 1);
            else
               Put_Line ("Main_Loop.Render_Phase, invalid Object_ID: " &
                          Single'Image (PT_Object_ID));
            end if;
         end if;
      end if;

      Use_Program (Lighting_Technique);
      Set_Eye_World_Pos_Location
        (Lighting_Technique, Ogldev_Camera.Get_Position (Game_Camera));
      for index in Int range 1 .. World_Position'Length loop
         Ogldev_Pipeline.Set_World_Position (Pipe, World_Position (index));
         Set_WVP_Location
           (Lighting_Technique, Ogldev_Pipeline.Get_WVP_Transform (Pipe));
         Set_World_Matrix_Location
           (Lighting_Technique, Ogldev_Pipeline.Get_World_Transform (Pipe));
         Ogldev_Pipeline.Init_Transforms (Pipe);
--           GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);
         Meshes_29.Render (Mesh);
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Phase.");
         raise;
   end Render_Phase;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Init (Main_Window);
   while Running loop
      Mouse_Handler (Main_Window);
      Ogldev_Camera.Update_Render (Game_Camera);
      Picking_Phase (Main_Window);
      Render_Phase (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
--        Delay (0.05);
   end loop;

exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;