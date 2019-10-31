
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Window;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

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

   Draw_Lines           : constant Boolean := True;
   Draw_Points          : constant Boolean := False;
   Iteration_Index      : Integer := 1;
   Iterations_Per_Frame : constant UInt := 16;

   procedure Update_Transform_Buffer;

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
   end Initialize;

   --  ----------------------------------------------------------------------------

   procedure Render (Window : in out Glfw.Windows.Window) is
      use GL.Toggles;
      Black         : constant Colors.Color := (0.0, 0.0, 0.0, 0.0);
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width) - 10,
                              GL.Types.Int (Window_Height) - 10);
      Utilities.Clear_Background_Colour (Black);

      Update_Transform_Buffer;

      GL.Objects.Programs.Use_Program (Rendering_Program);
      if Draw_Points then
         GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
         GL.Rasterization.Set_Point_Size (4.0);
         GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, Buffers.Total_Points);
         GL.Toggles.Disable (GL.Toggles.Vertex_Program_Point_Size);
      end if;
      if Draw_Lines then
         GL.Objects.Buffers.Element_Array_Buffer.Bind (Index_Buffer);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Index      => 0, Count  => 4, Kind  => Single_Type,
            Normalized => True, Stride => 0, Offset => 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         Put_Line ("Render Draw_Lines.");
         GL.Objects.Buffers.Draw_Elements
              (Lines, 2 * Buffers.Total_Connections, UInt_Type, 0);
         Put_Line ("Render Lines drawn.");
      end if;

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Render.");
         raise;
   end Render;

   --  ----------------------------------------------------------------------------

   procedure Update_Transform_Buffer is
      use GL.Toggles;
      Buffer_Index  : Integer;
   begin
      GL.Objects.Programs.Use_Program (Update_Program);
      Enable (Rasterizer_Discard);
      for index in reverse 1 .. Iterations_Per_Frame loop
         Buffer_Index :=  Iteration_Index Mod 2 + 1;
         GL.Objects.Vertex_Arrays.Bind (VAO (Buffer_Index));
         GL.Objects.Buffers.Texture_Buffer.Bind (Position_Tex_Buffer (Buffer_Index));
         Iteration_Index := Iteration_Index + 1;

         Buffer_Index :=  Iteration_Index Mod 2 + 1;
         GL.Objects.Buffers.Transform_Feedback_Buffer.Bind_Buffer_Base
           (0, VBO (Buffers.Position_A + Buffer_Index));
         GL.Objects.Buffers.Transform_Feedback_Buffer.Bind_Buffer_Base
           (1, VBO (Buffers.Velocity_A + Buffer_Index));

         GL.Objects.Programs.Begin_Transform_Feedback (Points);
           GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);
         GL.Objects.Programs.End_Transform_Feedback;
      end loop;
      Disable (Rasterizer_Discard);

   end Update_Transform_Buffer;

   --  ----------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Initialize;
   while Running loop
      Render (Main_Window);
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
