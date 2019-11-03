
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Uniforms;
with GL.Objects.Vertex_Arrays;
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

   VAO                  : Buffers.Vertex_Buffer_Array (1 .. 2);
   VBO                  : Buffers.Buffer_Array (1 .. 5);
   Index_Buffer         : GL.Objects.Buffers.Buffer;
   Position_Tex_Buffer  : Buffers.Buffer_Array (1 .. 2);

   Rendering_Program    : GL.Objects.Programs.Program;
   Update_Program       : GL.Objects.Programs.Program;
   Time_Step_Location   : GL.Uniforms.Uniform;

   Time_Step            : constant Single := 0.000009; --  0.0007;  --  orig 0.07
   Iteration_Index      : Integer := 1;
   Iterations_Per_Frame : UInt := 1;  -- 16
   Max_Iterations       : constant Integer := 100000;  --  20000
   Draw_Lines           : Boolean := False;
   Draw_Points          : Boolean := True;

   procedure Load_Shaders;
   procedure Update_Transform_Buffer;

   --  ------------------------------------------------------------------------

   procedure Handle_Keyboard (Window : in out Glfw.Windows.Window) is
      use Glfw.Input;
   begin
      if Window'Access.Key_State (Keys.R) = Pressed then
         Load_Shaders;
      elsif Window'Access.Key_State (Keys.L) = Pressed then
         Draw_Lines := not Draw_Lines;
      elsif Window'Access.Key_State (Keys.P) = Pressed then
         Draw_Points := not Draw_Points;
      elsif Window'Access.Key_State (Keys.Numpad_Add) = Pressed then
         Iterations_Per_Frame := Iterations_Per_Frame + 1;
      elsif Window'Access.Key_State (Keys.Numpad_Substract) = Pressed then
         Iterations_Per_Frame := Iterations_Per_Frame - 1;
      end if;
   end Handle_Keyboard;

   --  -------------------------------------------------------------------------

   procedure Initialize is
   begin
      for index in VAO'Range loop
         VAO (index).Initialize_Id;
         VAO (index).Bind;
      end loop;

      Load_Shaders;
      Buffers.Setup_Buffers (VAO, VBO, Index_Buffer, Position_Tex_Buffer);
   end Initialize;

   --  ----------------------------------------------------------------------------

    procedure Load_Shaders is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
      Update_Shader   : Shader_Sources (1 .. 1);
      Update_Varyings : constant String := "tf_position_mass,tf_velocity";
   begin
      Update_Shader (1) := (Src ("src/shaders/update_vertex_shader.glsl", Vertex_Shader));
      Update_Program := Program_From (Update_Shader);

      if not GL.Objects.Programs.Link_Status (Update_Program) then
         Put_Line ("Initialize_Shaders, Update_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Update_Program));
      else
         GL.Objects.Programs.Use_Program (Update_Program);
         Time_Step_Location :=
           GL.Objects.Programs.Uniform_Location (Update_Program, "time_step");
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

      if Iteration_Index > Max_Iterations then
            Initialize;
            Iteration_Index := 1;
      end if;
      Handle_Keyboard (Window);
      Update_Transform_Buffer;

      GL.Objects.Programs.Use_Program (Rendering_Program);
      if Draw_Points then
         GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
         --  Point_Size is set in the vertex shader
         GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, Buffers.Total_Points);
         GL.Toggles.Disable (GL.Toggles.Vertex_Program_Point_Size);
      end if;
      if Draw_Lines then
         GL.Objects.Buffers.Element_Array_Buffer.Bind (Index_Buffer);
         Put_Line ("Render Draw_Elements.");
         GL.Objects.Buffers.Draw_Elements
           (Lines, 2 * Buffers.Total_Connections, UInt_Type, 0);
         Put_Line ("Render Elements drawn.");
      end if;

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Main_Loop.Render.");
         raise;
   end Render;

   --  ----------------------------------------------------------------------------

   procedure Update_Transform_Buffer is
      use GL.Toggles;
      Buffer_Index  : Integer;
   begin
      GL.Objects.Programs.Use_Program (Update_Program);
      GL.Uniforms.Set_Single (Time_Step_Location, Time_Step);
      Enable (Rasterizer_Discard);
      for index in reverse 1 .. Iterations_Per_Frame loop
         Buffer_Index :=  Iteration_Index Mod 2 + 1;      -- BI 1 or 2
         GL.Objects.Vertex_Arrays.Bind (VAO (Buffer_Index));
         GL.Objects.Buffers.Texture_Buffer.Bind
           (Position_Tex_Buffer (Buffer_Index));

         Iteration_Index := Iteration_Index + 1;
         if Buffer_Index /= 1 then
            Buffer_Index := 0;
         end if;

         GL.Objects.Buffers.Transform_Feedback_Buffer.Bind_Buffer_Base
           (0, VBO (Buffers.Position_A + Buffer_Index));  -- VBO 1 or 2
         GL.Objects.Buffers.Transform_Feedback_Buffer.Bind_Buffer_Base
           (1, VBO (Buffers.Velocity_A + Buffer_Index));  -- VBO 3 or 4

         GL.Objects.Programs.Begin_Transform_Feedback (Points);
         GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, Buffers.Total_Points);
         GL.Objects.Programs.End_Transform_Feedback;
      end loop;
      Disable (Rasterizer_Discard);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Main_Loop.Update_Transform_Buffer.");
         raise;
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
--        delay (0.1);
   end loop;
exception
   when Program_Loader.Shader_Loading_Error =>
      -- message was already written to stdout
      null;
end Main_Loop;
