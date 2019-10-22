
with Interfaces.C.Pointers;

with Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Objects;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   type Particle_Buffer is record
      Position : Singles.Vector4;
      Velocity : Singles.Vector4;
   end record;

   type Particle_Buffer_Array is array (UInt range <>) of aliased Particle_Buffer;

   package Particle_Buffer_Package is new Interfaces.C.Pointers
     (UInt, Particle_Buffer, Particle_Buffer_Array,
      Particle_Buffer'(others => <>));
   procedure Map_Particle_Buffer_Range is new
     GL.Objects.Buffers.Map_Range (Particle_Buffer_Package);

   Particle_Group_Size         : constant UInt := 128;   --  1024;
   Particle_Group_Count        : constant UInt := 512;  --  8192
   Particle_Count              : constant UInt :=
                                   Particle_Group_Size * Particle_Group_Count;
   Maximum_Attractors          : constant Int := 64;

   Vec4_Size                   : constant UInt := GL.Types.Singles.Vector4'Size / 8;
   Buffer_Size                 : constant Size := Size (Particle_Count * 2 * Vec4_Size);

   Background                  : constant GL.Types.Colors.Color := (0.0, 1.0, 0.0, 0.0);

   Render_VAO                  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Position_Buffer             : GL.Objects.Buffers.Buffer;
   Velocity_Buffer             : GL.Objects.Buffers.Buffer;
   Position_Texture_Buffer     : GL.Objects.Buffers.Buffer;
   Velocity_Texture_Buffer     : GL.Objects.Buffers.Buffer;
--     Position_Texture            : GL.Objects.Textures.Texture;
--     Velocity_Texture            : GL.Objects.Textures.Texture;

   Attractor_Buffer            : GL.Objects.Buffers.Buffer;

   Render_Program              : GL.Objects.Programs.Program;

   MVP_Matrix_ID               : GL.Uniforms.Uniform;
   Attractor_Masses            : Single_Array (1 .. Maximum_Attractors);

   Start_Ticks                 : constant UInt := UInt (Glfw.Time) - 100000;  --  seconds
   Last_Ticks                  : UInt;
   Map_Access                  : GL.Objects.Buffers.Map_Bits;

   --  ------------------------------------------------------------------------

   procedure Load_Array_Buffer;
   function Random_Float return Single;
   function Random_Vector (Min_Magnitude, Max_Magnitude : Single) return Singles.Vector3;

   --  ------------------------------------------------------------------------

   procedure Initialize is

      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
   begin
      Render_VAO.Initialize_Id;
      Render_VAO.Bind;

      --  Map_Access.Write indicates that the returned pointer may be used to
      --  modify buffer object data.
      --  Map_Access.Invalidate_Buffer indicates that the previous contents
      --  of the entire buffer may be discarded.
      Map_Access.Write := True;
      Map_Access.Invalidate_Buffer := True;

      Position_Buffer.Initialize_Id;
      Array_Buffer.Bind (Position_Buffer);
      Velocity_Buffer.Initialize_Id;
      Array_Buffer.Bind (Velocity_Buffer);

      Load_Array_Buffer;

      Position_Texture_Buffer.Initialize_Id;
      Texture_Buffer.Bind (Position_Texture_Buffer);
      Texture_Buffer.Allocate (GL.Pixels.RGBA32F, Position_Buffer);

      Velocity_Texture_Buffer.Initialize_Id;
      Texture_Buffer.Bind (Velocity_Texture_Buffer);
      Texture_Buffer.Allocate (GL.Pixels.RGBA32F, Velocity_Buffer);

      Attractor_Buffer.Initialize_Id;
      Uniform_Buffer.Bind (Attractor_Buffer);
      Uniform_Buffer.Allocate (32, Static_Draw);

      for index in 1 .. Maximum_Attractors loop
         Attractor_Masses (index) := 0.5 * (Random_Float + 1.0);
      end loop;
      Uniform_Buffer.Bind_Buffer_Base (0, Attractor_Buffer);

      --  A simple program to visualize the result
      Render_Program := Program_From
        ((Src ("src/shaders/render_vertex_shader_12.glsl", Vertex_Shader),
          Src ("src/shaders/render_fragment_shader_12.glsl", Fragment_Shader)));
      MVP_Matrix_ID := Uniform_Location (Render_Program, "mvp");

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Initialize.");
         raise;
   end Initialize;

   --  ------------------------------------------------------------------------

   procedure Display (Window : in out Glfw.Windows.Window) is
      use GL.Blending;
      use GL.Toggles;
      use GL.Types.Singles;
      use Maths;
      use Single_Math_Functions;
      use GL.Objects.Buffers;
      Window_Width       : Glfw.Size;
      Window_Height      : Glfw.Size;
      Aspect             : Single;
      Attractors_Pointer : Particle_Buffer_Package.Pointer;
--        Buffer_Pointer     : Particle_Buffer_Package.Pointer;  --  to Particle_Buffer_Array
      MVP_Matrix         : Singles.Matrix4;
      Current_Ticks      : constant UInt := UInt (Glfw.Time);
      Time               : constant Single := Single (Start_Ticks - Current_Ticks);
      Delta_Time         : Single;
   begin
      Delta_Time := 0.075 * Single (Current_Ticks - Last_Ticks);
      Last_Ticks := Current_Ticks;

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Background_Colour_And_Depth (Background);

      if Delta_Time > 0.01 then
         Disable (Depth_Test);
         Enable (Blend);
         Set_Blend_Func (One, One);

         Map_Particle_Buffer_Range
           (Uniform_Buffer, Map_Access, 0,  32, Attractors_Pointer);
         for index in 1 .. 32 loop
            Attractors_Pointer.Position
              := (50.0 * Sin (20.0 * 7.5 * Single (index + 3) * Time),
                  50.0 * Sin (20.0 * 3.9 * Single (index + 6) * Time),
                  100.0 * Sin (20.0 * 5.3 * Single (index + 2) *
                      Cos (9.1 * Single (index + 4) * Time)),
                  Attractor_Masses (Int (index)));
            Particle_Buffer_Package.Increment (Attractors_Pointer);
         end loop;
         Unmap (Uniform_Buffer);

         --  If dt is too large, the system could explode,
         --  so cap it to some maximum value.
         if Delta_Time >= 2.0 then
            Delta_Time := 2.0;
         end if;

         Aspect := Single (Window_Height) / Single (Window_Width);
         MVP_Matrix :=
           Perspective_Matrix (Degree (45.0), Aspect, 0.1, 1000.0) *
             Translation_Matrix ((0.0, 0.0, -160.0)) *
               Rotation_Matrix (Degree (1000.0 * Time), (0.0, 1.0, 0.0));

         GL.Objects.Programs.Use_Program (Render_Program);
         GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);

         Render_VAO.Bind;
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   procedure Load_Array_Buffer is
      use GL.Objects.Buffers;
      use GL.Types.Singles;
      Positions_Pointer  : Particle_Buffer_Package.Pointer;  --  to Particle_Buffer_Array
      Velocities_Pointer : Particle_Buffer_Package.Pointer;  --  to Particle_Buffer_Array
      test               : Vector4;
   begin
      Array_Buffer.Bind (Position_Buffer);
      Array_Buffer.Allocate (Long (Buffer_Size), Dynamic_Copy);

      test := To_Vector4 (Random_Vector (-10.0, 10.0)) + (0.0, 0.0, 0.0, Random_Float);
      Utilities.Print_Vector ("test", test);
      --  Load positions
      --  Map_Particle_Buffer_Range maps all or part of the data store of a specified
      --  buffer object into the client's address space.
      --  offset and length indicate the range of data in the buffer object
      --  that is to be mapped in terms of basic machine units.
      Map_Particle_Buffer_Range
        (Array_Buffer, Map_Access, 0, Buffer_Size, Positions_Pointer);

      Put_Line ("Main_Loop.Load_Array_Buffer Buffer_Range mapped.");
      for B_Index in 1 .. Particle_Count loop
         Positions_Pointer.Position
           := To_Vector4 (Random_Vector (-10.0, 10.0)) + (0.0, 0.0, 0.0, Random_Float);
         Particle_Buffer_Package.Increment (Positions_Pointer);
      end loop;
      Unmap (Array_Buffer);
      Put_Line ("Main_Loop.Load_Array_Buffer Array_Buffer unmapped.");

      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 4, Single_Type, True, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      Array_Buffer.Bind (Velocity_Buffer);
      Array_Buffer.Allocate (Long (4 * Buffer_Size), Dynamic_Copy);

      --  Load velocities
      Map_Particle_Buffer_Range (Array_Buffer, Map_Access, 0,  Buffer_Size, Velocities_Pointer);
      for B_Index in 1 .. Particle_Count loop
         Velocities_Pointer.Velocity := To_Vector4 (Random_Vector (-0.1, 0.1));
         Particle_Buffer_Package.Increment (Velocities_Pointer);
      end loop;
      Unmap (Array_Buffer);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Load_Array_Buffer.");
         raise;
   end Load_Array_Buffer;

   --  ------------------------------------------------------------------------

   function Random_Float return Single is
      use Ada.Numerics.Float_Random;
      Gen      : Generator;
   begin
      return Single (Random (Gen));
   end Random_Float;

   --  ------------------------------------------------------------------------

   function Random_Vector (Min_Magnitude, Max_Magnitude : Single)
                           return Singles.Vector3 is
      use Ada.Numerics.Float_Random;
      use GL.Types.Singles;
      Gen      : Generator;
      R_Vector : Vector3 := (2.0 * Single (Random (Gen)) - 1.0,
                             2.0 * Single (Random (Gen)) - 1.0,
                             2.0 * Single (Random (Gen)) - 1.0);
   begin
      R_Vector := Maths.Normalized (R_Vector);
      R_Vector := R_Vector * (Random_Float *
                              (Max_Magnitude - Min_Magnitude) + Min_Magnitude);
      return R_Vector;
   end Random_Vector;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Initialize;
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
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
