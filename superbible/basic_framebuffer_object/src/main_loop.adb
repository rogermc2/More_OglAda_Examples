
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with My_Buffers;
with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is

   Frame_Buffer            : GL.Objects.Framebuffers.Framebuffer;
   Position_Buffer         : GL.Objects.Buffers.Buffer;
   Index_Buffer            : GL.Objects.Buffers.Buffer;
   Rendering_Program1      : GL.Objects.Programs.Program;
   Rendering_Program2      : GL.Objects.Programs.Program;
   Vertex_Array            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Projection_Location1    : GL.Uniforms.Uniform;
   Model_View_Location1    : GL.Uniforms.Uniform;
   Projection_Location2    : GL.Uniforms.Uniform;
   Model_View_Location2    : GL.Uniforms.Uniform;
   Colour_Texture          : GL.Objects.Textures.Texture;
   Draw_Buffer_List        : GL.Buffers.Explicit_Color_Buffer_List (0 .. 0);

   Set_Up_Error            : Exception;

   --  ------------------------------------------------------------------------

   function Render_Frame_Buffer (Window : in out Glfw.Windows.Window) return Boolean is
      use GL.Objects.Framebuffers;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      use GL.Types.Singles;
      use Maths;
      use Maths.Single_Math_Functions;
      Blue              : constant GL.Types.Colors.Color := (0.0, 0.0, 0.6, 1.0);
      Green             : constant GL.Types.Colors.Color := (0.0, 0.8, 0.0, 1.0);
      Window_Width      : Glfw.Size := 512;
      Window_Height     : Glfw.Size := 512;
      Current_Time      : constant Glfw.Seconds := Glfw.Time;
      Time_Factor       : constant Single := 0.3 * Single (Current_Time);
      Projection_Matrix : GL.Types.Singles.Matrix4;
      Model_View_Matrix : GL.Types.Singles.Matrix4;
   begin

      Window.Get_Size (Window_Width, Window_Height);
      Projection_Matrix := Maths.Perspective_Matrix (50.0,
                                                     Single (Window_Width) / Single (Window_Height), 0.1, 1000.0);
      Model_View_Matrix := Translation_Matrix ((0.0, 0.0, -4.0)) *
        Translation_Matrix ((0.5 * Sin (2.1 * Time_Factor),
                            0.5 * Cos (1.7 * Time_Factor), 2.0 * Sin (1.3 * Time_Factor) * Cos (1.5 * Time_Factor))) *
        Rotation_Matrix (Degree (45.0 * Single (Current_Time)), (0.0, 1.0, 0.0)) *
        Rotation_Matrix (Degree (81.0 * Single (Current_Time)), (1.0, 0.0, 0.0));

      Read_And_Draw_Target.Bind (Frame_Buffer);     -- Set current buffer
      GL.Window.Set_Viewport (0, 0, Int (Window_Width), Int (Window_Height));

      Utilities.Clear_Background_Colour_And_Depth (Green);
      GL.Buffers.Clear_Stencil_Buffer (0);
      GL.Buffers.Set_Stencil_Clear_Value (1);

      GL.Objects.Programs.Use_Program (Rendering_Program1);

      GL.Uniforms.Set_Single (Projection_Location1, Projection_Matrix);
      GL.Uniforms.Set_Single (Model_View_Location1, Model_View_Matrix);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 36);

      --  glBindFramebuffer(GL_FRAMEBUFFER, 0);
      Read_And_Draw_Target.Bind (Default_Framebuffer);

      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width), GL.Types.Int (Window_Height));

      Utilities.Clear_Background_Colour_And_Depth (Blue);
      GL.Buffers.Clear_Depth_Buffer (1.0);
      GL.Buffers.Set_Depth_Clear_Value (1.0);

      Texture_2D.Bind (Colour_Texture);
      GL.Objects.Programs.Use_Program (Rendering_Program2);

      GL.Uniforms.Set_Single (Projection_Location2, Projection_Matrix);
      GL.Uniforms.Set_Single (Model_View_Location2, Model_View_Matrix);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 36);

      --  glBindTexture (GL_TEXTURE_2D, 0);
      --  Texture_2D.Bind (Default_Framebuffer);
      return True;

   exception
      when  others =>
         Put_Line ("An exception occurred in Render_Frame_Buffer.");
         raise;

   end Render_Frame_Buffer;

   --  ------------------------------------------------------------------------

   procedure Set_Up is
      use GL.Objects.Framebuffers;
      use GL.Objects.Shaders;
      use Program_Loader;
      Status : GL.Objects.Framebuffers.Framebuffer_Status;
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Rendering_Program1 := Program_From ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
                                          Src ("src/shaders/fragment_shader1.glsl", Fragment_Shader)));
      Rendering_Program2 := Program_From ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
                                          Src ("src/shaders/fragment_shader2.glsl", Fragment_Shader)));

      -- Get locations of shader programs matrix uniforms
      Model_View_Location1 := GL.Objects.Programs.Uniform_Location (Rendering_Program1, "mv_matrix");
      Projection_Location1 := GL.Objects.Programs.Uniform_Location (Rendering_Program1, "projection_matrix");
      Model_View_Location2 := GL.Objects.Programs.Uniform_Location (Rendering_Program2, "mv_matrix");
      Projection_Location2 := GL.Objects.Programs.Uniform_Location (Rendering_Program2, "projection_matrix");

      My_Buffers.Setup_Buffers (Frame_Buffer, Position_Buffer, Index_Buffer);
      My_Buffers.Setup_Textures (Frame_Buffer, Colour_Texture, Draw_Buffer_List);

      Status := GL.Objects.Framebuffers.Status (Read_And_Draw_Target);
      if Status /= Complete then
         Put_Line ("Set_Up; Bind (Frame_Buffer), Status: ");
         case Status is
            when Undefined => Put_Line ("Undefined.");
            when Incomplete_Attachment => Put_Line ("Incomplete attachment.");
            when Incomplete_Missing_Attachment => Put_Line ("Incomplete; missing attachment.");
            when Incomplete_Draw_Buffer => Put_Line ("Incomplete draw buffer.");
            when Incomplete_Read_Buffer => Put_Line ("Incomplete read buffer.");
            when Unsupported => Put_Line ("Unsupported.");
            when Incomplete_Multisample => Put_Line ("Incomplete multisample.");
            when Incomplete_Layer_Targets => Put_Line ("Incomplete layertargets.");
            when others => null;
         end case;
         raise Set_Up_Error;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Set_Up.");
         raise Set_Up_Error;
   end Set_Up;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running  : Boolean := True;
begin
   Set_Up;
   while Running loop
      Delay (0.4);
      Running := Render_Frame_Buffer (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not (Main_Window.Key_State (Glfw.Input.Keys.Escape) =
                                    Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;

exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
