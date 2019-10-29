
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Toggles;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with Buffers;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   use GL.Types;

   VAO                  : array (1 .. 2) of GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   VBO                  : Buffers.Buffer_Array (1 .. 5);
   Index_Buffer         : GL.Objects.Buffers.Buffer;
   Position_Tex_Buffer  : Buffers.Buffer_Array (1 .. 2);

   Rendering_Program    : GL.Objects.Programs.Program;
   Update_Program       : GL.Objects.Programs.Program;

   Draw_Lines           : Boolean;
   Draw_Points          : Boolean;
   Iterations_Per_Frame : UInt;

   --  ------------------------------------------------------------------------

   procedure Load_Shaders is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
      Update_Shader   : Shader_Sources (1 .. 1);
      Update_Varyings  : constant String := "tf_position_mass,tf_velocity";
   begin
      Update_Shader (1) := (Src ("src/shaders/update_vertex_shader.glsl", Vertex_Shader));
      Update_Program := Program_From (Update_Shader);

      if not GL.Objects.Programs.Link_Status (Update_Program) then
         Put_Line ("Initialize_Shaders, Update_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Update_Program));
      end if;

      Transform_Feedback_Varyings
        (Update_Program, Update_Varyings, Separate_Attribs);
      Update_Program.Link;
      if not GL.Objects.Programs.Link_Status (Update_Program) then
         Put_Line ("Initialize_Shaders, Update_Program Transform_Feedback_Varyings Link failed.");
         Put_Line (GL.Objects.Programs.Info_Log (Update_Program));
      end if;

      Rendering_Program := Program_From
        ((Src ("src/shaders/render_vertex_shader.glsl", Vertex_Shader),
          Src ("src/shaders/render_fragment_shader.glsl", Fragment_Shader)));
   exception
      when others =>
         Put_Line ("An exceptiom occurred in Main_Loop.Load_Shaders.");
         raise;
   end Load_Shaders;

   --  ------------------------------------------------------------------------

   procedure Initialize is

   begin
      for index in VAO'Range loop
         VAO (index).Initialize_Id;
         VAO (index).Bind;
      end loop;

      Load_Shaders;

      Buffers.Setup_Buffers (VBO, Index_Buffer, Position_Tex_Buffer);

      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.Less);
      -- Point size is set in the vertex shader
      GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
   end Initialize;

   --  ----------------------------------------------------------------------------

   procedure Render_Dot (Current_Time : Glfw.Seconds) is
      use Maths.Single_Math_Functions;

      Back_Colour : constant GL.Types.Colors.Color :=
                      (0.5 * (1.0 + Sin (Single (Current_Time))),
                       0.5 * (1.0 + Cos (Single (Current_Time))), 0.0, 1.0);
   begin
      Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

      GL.Objects.Programs.Use_Program (Rendering_Program);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Render_Dot.");
         raise;
   end Render_Dot;

   --  ----------------------------------------------------------------------------

   use Glfw.Input;
   Running            : Boolean := True;
begin
   Initialize;
   while Running loop
      Render_Dot (Glfw.Time);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;
exception
   when Program_Loader.Shader_Loading_Error =>
      -- message was already written to stdout
      null;
end Main_Loop;
