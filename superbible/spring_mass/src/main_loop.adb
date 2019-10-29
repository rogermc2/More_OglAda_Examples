
with Ada.Text_IO; use Ada.Text_IO;

with  GL.Attributes;
with GL.Buffers;
with GL.Toggles;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   use GL.Types;

   --  Buffer types
   Position_A           : constant Integer := 1;
   Position_B           : constant Integer := 2;
   Velocity_A           : constant Integer := 3;
--     Velocity_B           : constant Integer := 4;
   Connection           : constant Integer := 5;

   VAO                  : array (1 .. 2) of GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   VBO                  : array (1 .. 5) of GL.Objects.Buffers.Buffer;
   Index_Buffer         : GL.Objects.Buffers.Buffer;
   Position_Tex_Buffer  : array (1 .. 2) of GL.Objects.Buffers.Buffer;

   Rendering_Program    : GL.Objects.Programs.Program;
   Update_Program       : GL.Objects.Programs.Program;

   Draw_Lines           : Boolean;
   Draw_Points          : Boolean;
   Iterations_Per_Frame : UInt;

   Points_X             : constant Int := 50;
   Points_Y             : constant Int := 50;
   Total_Points         : constant Int :=  Points_X * Points_Y;
   Total_Connections    : constant Int
     := (Points_X - 1) * Points_Y + Points_X * (Points_Y - 1);

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
      use GL.Objects.Buffers;
      use Maths.Single_Math_Functions;

      procedure Load_Connections_Buffer is new
        GL.Objects.Buffers.Load_To_Buffer (Ints.Vector4_Pointers);

      Num_X               : constant Single := Single (Points_X);
      Num_Y               : constant Single := Single (Points_Y);
      Lines               : constant Int := Total_Connections;
      Initial_Positions   : Singles.Vector4_Array (1 .. Total_Points);
      Initial_Velocities  : constant Singles.Vector3_Array (1 .. Total_Points)
        := (others => (0.0, 0.0, 0.0));
      Initial_Connections : Ints.Vector4_Array (1 .. Total_Points)
        := (others => (-1, -1, -1, -1));

      X_Value             : Single;
      Y_Value             : Single;
      Value               : Singles.Vector4;
      Initial_Index       : Int := 0;
   begin
      Load_Shaders;
      for index_Y in 1 .. Points_Y loop
         Y_Value := Single (index_Y) / Single (Points_Y);
         for index_X in 1 .. Points_X loop
            X_Value := Single (index_X) / Single (Points_X);
            Value (GL.X) := (X_Value - 0.5) * Num_X;
            Value (GL.Y) := (Y_Value - 0.5) * Num_Y;
            Value (GL.Z) := 0.6 * Sin (X_Value) * Cos (Y_Value);
            Value (GL.W) := 1.0;
            Initial_Positions (Initial_Index) := Value;
            if index_Y /= Points_Y - 1 then
               if index_X /= 1 then
                  Initial_Connections (Initial_Index)(GL.X) := Initial_Index - 1;
               end if;
               if index_Y /= 1 then
                  Initial_Connections (Initial_Index)(GL.Y) := Initial_Index - Points_X;
               end if;
               if index_X /= Points_X - 1 then
                  Initial_Connections (Initial_Index)(GL.Z) := Initial_Index + 1;
               end if;
               Initial_Connections (Initial_Index)(GL.W) := Initial_Index + Points_X;
            end if;
            Initial_Index := Initial_Index + 1;
         end loop;
      end loop;

      for index in VAO'Range loop
         VAO (index).Initialize_Id;
         VAO (index).Bind;
      end loop;
      for index in VBO'Range loop
         VBO (index).Initialize_Id;
      end loop;
      for index in VAO'Range loop
         Array_Buffer.Bind (VBO (Position_A + 1));
         Utilities.Load_Vertex_Buffer
           (Array_Buffer, Initial_Positions, Dynamic_Copy);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Index  => 0, Count  => 4, Kind  => Single_Type,
            Normalized => True, Stride => 0, Offset => 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);

         Array_Buffer.Bind (VBO (Velocity_A + 1));
         Utilities.Load_Vertex_Buffer
           (Array_Buffer, Initial_Velocities, Dynamic_Copy);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (1, 3, Single_Type, True, 0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);

         Array_Buffer.Bind (VBO (Connection));
         Load_Connections_Buffer
           (Array_Buffer, Initial_Connections, Static_Draw);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (2, 4, Int_Type, True, 0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (2);
      end loop;

      for index in Position_Tex_Buffer'Range loop
         Position_Tex_Buffer (index).Initialize_Id;
         GL.Objects.Buffers.Texture_Buffer.Bind (Position_Tex_Buffer (index));
         if index = 1 then
            GL.Objects.Buffers.Texture_Buffer.Allocate (GL.Pixels.RGBA32F, VBO (Position_A));
         else
            GL.Objects.Buffers.Texture_Buffer.Allocate (GL.Pixels.RGBA32F, VBO (Position_B));
         end if;
      end loop;

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
