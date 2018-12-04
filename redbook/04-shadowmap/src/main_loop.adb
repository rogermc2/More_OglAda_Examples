
with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
with GL.Toggles;
with GL.Types;
--  with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Load_VB_Object;
with Maths;
with Utilities;

with Project_Buffers;
with Shader;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Render_Light_Program : GL.Objects.Programs.Program;
   Render_Scene_Program : GL.Objects.Programs.Program;
   Light_Uniforms       : Shader.Light_Uniform_IDs;
   Scene_Uniforms       : Shader.Scene_Uniform_IDs;
   Ground_Buffer        : GL.Objects.Buffers.Buffer;
   Depth_Frame_Buffer   : GL.Objects.Framebuffers.Framebuffer;
   Depth_Texure         : GL.Objects.Textures.Texture;
   Ground_Vertex_Array  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   VBM_Object           : Load_VB_Object.VB_Object;

   Current_Width        : GL.Types.Int := 512;
   Current_Height       : GL.Types.Int := 512;
   Frustum_Depth        : constant Single := 800.0;
   Depth_Texure_Size    : constant GL.Types.Int := 2048;

   procedure Draw_Scene (Depth_Only : Boolean);
   procedure Render_Depth
     (View_Matrix, Projection_Matrix, Scene_Model_Matrix : Singles.Matrix4);

   --  ------------------------------------------------------------------------

   procedure Display (Window :  in out Glfw.Windows.Window) is
      use Ada.Numerics;
      use GL.Types.Singles;
      use Maths.Single_Math_Functions;
      Window_Width            : Glfw.Size;
      Window_Height           : Glfw.Size;
      --        X_Axis            : constant Singles.Vector3 := (1.0, 0.0, 0.0);
      Y_Axis                  : constant Singles.Vector3 := (0.0, 1.0, 0.0);
      --        Z_Axis            : constant Singles.Vector3 := (0.0, 0.0, 1.0);
      --        Background        : constant GL.Types.Colors.Color := (0.7, 0.7, 0.7, 1.0);
      Current_Time            : constant Single := Single (Glfw.Time);
      Aspect                  : Single;
      Light_Position          : constant Vector3 := (300.0 * Sin (6.0 * Pi * Current_Time), 200.0,
                                                     250.0 + 100.0 * Cos (4.0 * Pi * Current_Time));
      Light_View_Matrix       : Matrix4;
      Light_Projection_Matrix : constant Matrix4
        := Maths.Frustum_Matrix (Left   => -1.0, Right  => 1.0,
                                 Bottom => -1.0, Top    => 1.0,
                                 Near   =>  1.0, Far    => Frustum_Depth);
      Scene_Model_Matrix      : constant Matrix4
        := Maths.Rotation_Matrix (Maths.Degree (720.0 * Current_Time), Y_Axis);
      Scene_View_Matrix       : constant Matrix4
        := Maths.Translation_Matrix ((0.0, 0.0, -300.0));
      Scene_Projection_Matrix : Matrix4;
      Scale_Bias_Matrix       : constant Matrix4
        := ((0.5, 0.0, 0.0, 0.0),
            (0.0, 0.5, 0.0, 0.0),
            (0.0, 0.0, 0.5, 0.0),
            (0.5, 0.5, 0.5, 1.0));

   begin
      Maths.Init_Lookat_Transform (Light_Position, (0.0, 0.0, 0.0),
                                   Y_Axis, Light_View_Matrix);
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Current_Width := GL.Types.Int (Window_Width);
      Current_Height := GL.Types.Int (Window_Width);
      Aspect := Single (Window_Height) / Single (Window_Width);
      Scene_Projection_Matrix :=
        Maths.Frustum_Matrix (-1.0, 1.0, -Aspect, Aspect, 1.0, Frustum_Depth);

      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.LEqual);
      --  Render from the light's position into the depth buffer.
      Render_Depth (Light_View_Matrix, Light_Projection_Matrix, Scene_View_Matrix);

      GL.Window.Set_Viewport (0, 0, Current_Width, Current_Height);
      --  Render from the viewer's position
      GL.Objects.Programs.Use_Program (Render_Scene_Program);
      Utilities.Clear_Colour_Buffer_And_Depth;
      GL.Uniforms.Set_Single (Scene_Uniforms.Model_Matrix_ID, Scene_Model_Matrix);
      GL.Uniforms.Set_Single (Scene_Uniforms.View_Matrix_ID, Scene_View_Matrix);
      GL.Uniforms.Set_Single (Scene_Uniforms.Projection_Matrix_ID, Scene_Projection_Matrix);
      GL.Uniforms.Set_Single
        (Scene_Uniforms.Shadow_Matrix_ID, Scale_Bias_Matrix * Light_Projection_Matrix * Light_View_Matrix);
      GL.Uniforms.Set_Single (Scene_Uniforms.Light_Position_Matrix_ID, Light_Position);

      GL.Objects.Textures.Targets.Texture_2D.Bind (Depth_Texure);
      GL.Objects.Textures.Targets.Texture_2D.Generate_Mipmap;
      Draw_Scene (False);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   procedure Draw_Scene (Depth_Only : Boolean) is
      use GL.Types.Singles;
      Object_Ambient        : constant Vector3 := (0.1, 0.0, 0.2);
      Ground_Ambient        : constant Vector3 := (0.1, 0.1, 0.1);
      Object_Diffuse        : constant Vector3 := (0.3, 0.2, 0.8);
      Ground_Diffuse        : constant Vector3 := (0.1, 0.5, 0.1);
      Object_Spectral       : constant Vector3 := (1.0, 1.0, 1.02);
      Ground_Spectral       : constant Vector3 := (0.1, 0.1, 0.1);
   begin
      if not Depth_Only then
         GL.Uniforms.Set_Single (Scene_Uniforms.Ambient_Matrix_ID, Object_Ambient);
         GL.Uniforms.Set_Single (Scene_Uniforms.Diffuse_Matrix_ID, Object_Diffuse);
         GL.Uniforms.Set_Single (Scene_Uniforms.Specular_Matrix_ID, Object_Spectral);
         GL.Uniforms.Set_Single (Scene_Uniforms.Specular_Power_ID, 25.0);
      end if;
      Put_Line ("Main_Loop.Draw_Scene, uniforms set.");

      Load_VB_Object.Render (VBM_Object);
      Put_Line ("Main_Loop.Draw_Scene, VBM_Object rendered.");

      if not Depth_Only then
         GL.Uniforms.Set_Single (Scene_Uniforms.Ambient_Matrix_ID, Ground_Ambient);
         GL.Uniforms.Set_Single (Scene_Uniforms.Diffuse_Matrix_ID, Ground_Diffuse);
         GL.Uniforms.Set_Single (Scene_Uniforms.Specular_Matrix_ID, Ground_Spectral);
         GL.Uniforms.Set_Single (Scene_Uniforms.Specular_Power_ID, 3.0);
      end if;

      GL.Objects.Vertex_Arrays.Bind (Ground_Vertex_Array);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangle_Fan, 0, 4);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Draw_Scene.");
         raise;
   end Draw_Scene;

   --  ------------------------------------------------------------------------

   --  Render from the light's position into the depth buffer.
   procedure Render_Depth
     (View_Matrix, Projection_Matrix, Scene_Model_Matrix : Singles.Matrix4) is
      use GL.Objects.Framebuffers;
      use GL.Types.Singles;
      MVP_Matrix : constant Matrix4
        := Projection_Matrix * View_Matrix * Scene_Model_Matrix;
   begin
      GL.Objects.Programs.Use_Program (Render_Light_Program);

      GL.Uniforms.Set_Single (Light_Uniforms.MVP_Matrix_ID, MVP_Matrix);

      Read_And_Draw_Target.Bind (Depth_Frame_Buffer);
      GL.Window.Set_Viewport (0, 0, Depth_Texure_Size, Depth_Texure_Size);
      GL.Buffers.Set_Depth_Clear_Value (1.0);
      if Status (Read_And_Draw_Target) /= Complete then
         Put_Line ("Main_Loop.Display, Depth_Frame_Buffer status: " &
                     Framebuffer_Status'Image (Status (Read_And_Draw_Target)));
      end if;
      Utilities.Clear_Depth;
      Put_Line ("Main_Loop.Display, Depth cleared.");
      --   Enable polygon offset to resolve depth-fighting issues
      GL.Toggles.Enable (GL.Toggles.Polygon_Offset_Fill);
      GL.Rasterization.Set_Polygon_Offset (2.0, 4.0);
      Put_Line ("Main_Loop.Display, Polygon_Offset set.");
      Draw_Scene (True);
      Put_Line ("Main_Loop.Display, Scene drawn.");
      GL.Toggles.Disable (GL.Toggles.Polygon_Offset_Fill);
      Put_Line ("Main_Loop.Display, Polygon_Offset_Fill disabled.");

      Read_And_Draw_Target.Bind (Default_Framebuffer);
      Put_Line ("Main_Loop.Display, Default_Framebuffer bound.");

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Depth.");
         raise;
   end Render_Depth;

   --  ------------------------------------------------------------------------

   procedure Setup is
      Vertex_Attribute_ID     : constant Int := 0;
      Normal_Attribute_ID     : constant Int := 1;
      Tex_Coord0_Attribute_ID : constant Int := 2;
      VBM_Result              : Boolean := False;
   begin
      Ground_Vertex_Array.Initialize_Id;
      Ground_Vertex_Array.Bind;

      Shader.Init (Render_Light_Program, Render_Scene_Program,
                   Light_Uniforms, Scene_Uniforms);
      GL.Objects.Programs.Use_Program (Render_Scene_Program);
      GL.Uniforms.Set_Int (Scene_Uniforms.Depth_Texture, 0);

      Project_Buffers.Init_Ground_Buffer (Ground_Buffer);
      Project_Buffers.Init_Depth_Texture (Depth_Texure);
      Project_Buffers.Init_Depth_Frame_Buffer (Depth_Frame_Buffer, Depth_Texure);

      Load_VB_Object.Load_From_VBM ("../media/armadillo_low.vbm", VBM_Object,
                                    Vertex_Attribute_ID, Normal_Attribute_ID,
                                    Tex_Coord0_Attribute_ID, VBM_Result);
      VBM_Result := VBM_Result and Load_VB_Object.Get_Vertex_Count (VBM_Object) > 0;

      If not VBM_Result then
         Put_Line ("Main_Loop.Setup; Load_From_VBM failed.");
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup;
   while Running loop
      Display (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;
exception
   when others =>
      Put_Line ("An exceptiom occurred in Main_Loop.");
      raise;
end Main_Loop;
