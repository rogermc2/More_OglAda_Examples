
with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
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
   Vertex_Array         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer        : GL.Objects.Buffers.Buffer;
   Element_Buffer       : GL.Objects.Buffers.Buffer;
   VBM_Object           : Load_VB_Object.VB_Object;

   Frustum_Depth        : constant Single := 800.0;
   Depth_Texure_Size    : constant GL.Types.Int := 2048;

   procedure Draw_Scene (Depth_Only : Boolean);

   --  ------------------------------------------------------------------------

   procedure Display (Window :  in out Glfw.Windows.Window) is
      use Ada.Numerics;
      use GL.Objects.Buffers;
      use GL.Objects.Vertex_Arrays;
      use GL.Types.Singles;
      use Maths.Single_Math_Functions;
      X_Axis            : constant Singles.Vector3 := (1.0, 0.0, 0.0);
      Y_Axis            : constant Singles.Vector3 := (0.0, 1.0, 0.0);
      Z_Axis            : constant Singles.Vector3 := (0.0, 0.0, 1.0);
      Background        : constant GL.Types.Colors.Color := (0.7, 0.7, 0.7, 1.0);
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
      Current_Time      : constant Single := Single (Glfw.Time);
      Aspect            : Single;
      Scale             : constant Single := 1.5; --  Increase to reduce size
      Light_Position    : constant Vector3 := (300.0 * Sin (6.0 * Pi * Current_Time), 200.0,
                                      250.0 + 100.0 * Cos (4.0 * Pi * Current_Time));
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
      Light_View_Matrix       : Matrix4;
      Light_Projection_Matrix : constant Matrix4
        := Maths.Frustum_Matrix (Left   => -1.0, Right  => 1.0,
                                 Bottom => -1.0, Top    => 1.0,
                                 Near   =>  1.0, Far    => Frustum_Depth);
      begin
      Maths.Init_Lookat_Transform (Light_Position, (0.0, 0.0, 0.0),
                                   Y_Axis, Light_View_Matrix);
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Aspect := Single (Window_Height) / Single (Window_Width);
      Scene_Projection_Matrix :=
        Maths.Frustum_Matrix (-1.0, 1.0, -Aspect, Aspect, 1.0, Frustum_Depth);

      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.LEqual);

      --  Render from the light's position into the depth buffer.
      GL.Objects.Programs.Use_Program (Render_Light_Program);

      GL.Uniforms.Set_Single (Light_Uniforms.MVP_Matrix_ID,
                              Light_Projection_Matrix * Light_View_Matrix * Scene_Model_Matrix);
      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Depth_Frame_Buffer);
      GL.Window.Set_Viewport (0, 0, Depth_Texure_Size, Depth_Texure_Size);
      GL.Buffers.Set_Depth_Clear_Value (1.0);
      Utilities.Clear_Depth;
      GL.Toggles.Enable (GL.Toggles.Polygon_Offset_Fill);
      GL.Rasterization.Set_Polygon_Offset (2.0, 4.0);
      Draw_Scene (True);
      GL.Toggles.Disable (GL.Toggles.Polygon_Offset_Fill);

      Utilities.Clear_Background_Colour_And_Depth (Background);

      --  Set up the projection matrix
      --  Top, Bottom, Left, Right, Near, Far
      Maths.Init_Orthographic_Transform (Scale, -Scale, -Aspect, Aspect, -1.0, 500.0,
                                         Projection_Matrix);
      GL.Uniforms.Set_Single (Render_Projection_Matrix_ID, Projection_Matrix);

      --  Set up for the Draw_Elements call
      Vertex_Array.Bind;
      Element_Array_Buffer.Bind (Element_Buffer);

      --  Draw arrays
      Model_Matrix := Model_Matrix * Maths.Translation_Matrix ((0.0, -0.3, 5.0));
      GL.Uniforms.Set_Single (Render_Model_Matrix_ID, Model_Matrix);
      Draw_Arrays (Triangles, 0, 3);

      -- Draw elements
      Model_Matrix :=  Model_Matrix * Maths.Translation_Matrix ((-0.5, 0.2, 5.0));
      GL.Uniforms.Set_Single (Render_Model_Matrix_ID, Model_Matrix);
      Draw_Elements (Triangles, 3, UInt_Type);

      -- Draw elements base vertex
      Model_Matrix :=  Maths.Translation_Matrix ((1.0, 0.4, 5.0));
      GL.Uniforms.Set_Single (Render_Model_Matrix_ID, Model_Matrix);
      Draw_Elements (Triangles, 3, UInt_Type);

      --  Draw arrays instanced
      Model_Matrix :=  Maths.Translation_Matrix ((1.5, 0.5, 5.0));
      GL.Uniforms.Set_Single (Render_Model_Matrix_ID, Model_Matrix);
      Draw_Arrays_Instanced (Triangles, 0, 3, 1);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   procedure Draw_Scene (Depth_Only : Boolean) is
   begin
      null;
   end Draw_Scene;

   --  ------------------------------------------------------------------------

   procedure Setup is
      Vertex_Attribute_ID     : constant Int := 0;
      Normal_Attribute_ID     : constant Int := 1;
      Tex_Coord0_Attribute_ID : constant Int := 2;
      VBM_Result              : Boolean := False;
    begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Shader.Init (Render_Light_Program, Render_Scene_Program,
                   Light_Uniforms, Scene_Uniforms);
      GL.Objects.Programs.Use_Program (Render_Scene_Program);
      GL.Uniforms.Set_Int (Scene_Uniforms.Depth_Texture, 0);

      Project_Buffers.Init_Ground_Buffer (Ground_Buffer);
      Project_Buffers.Init_Texture (Depth_Buffer, Depth_Texure);

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
