
with Interfaces.C.Pointers;

with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Uniforms;
with GL.Window;

with Maths;
with Program_Loader;
--  with Utilities;

package body Particle_System is

   type Vertex_Buffer_Array is array (Int range <>) of
     GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   type Buffer_Array is array (Int range <>) of GL.Objects.Buffers.Buffer;

   type Vertex is record
        Position : Singles.Vector3 := (0.0, 0.0, 0.0);
        Velocity : Singles.Vector3 := (0.0, 0.0, 0.0);
   end record;
   type Vertex_Array is array (Int range <>) of aliased Vertex;

   package Vertex_Pointers is new Interfaces.C.Pointers
     (Int, Vertex, Vertex_Array, Vertex'(others => <>));
   procedure Load_Vertex_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (Vertex_Pointers);


--     Single_Bytes       : constant Int := Single'Size / 8;
   Num_Particles      : constant Int := 128 * 1024;
   Num_Buffers        : constant Int := 2;
   Num_Spheres        : constant Int := 3;
--     Buffer_Size        : constant Long := 4 * Long (Num_Particles * Single_Bytes);
   Dt                 : constant Single := 1.0 / 60.0;
   Bounce             : constant Single := 1.2;          --  inelastic: 1.0, elastic: 2.0
   G                  : constant Singles.Vector3 := (0.0, -9.81, 0.0);
   Vertex_Data_Array  : Vertex_Array (1 .. Num_Particles);
   Centre             : constant Singles.Vector3_Array  (1 .. Num_Spheres) :=
                           ((0.0, 12.0, 1.0),
                            (-3.0, 0.0, 0.0),
                            (5.0, -10.0, 0.0));
   Radius             : constant Single_Array  (1 .. Num_Spheres) := (3.0, 7.0, 12.0);
   Vertices_Array     : Vertex_Buffer_Array (1 .. Num_Buffers);
   Vertex_Buffers     : Buffer_Array (1 .. Num_Buffers);
   Particle_Program   : GL.Objects.Programs.Program;
   Transform_Program  : GL.Objects.Programs.Program;
   Buffer_Index       : Int := 1;
   View_ID            : GL.Uniforms.Uniform;
   Projection_ID      : GL.Uniforms.Uniform;
   Centre_ID          : GL.Uniforms.Uniform;
   Radius_ID          : GL.Uniforms.Uniform;
   G_ID               : GL.Uniforms.Uniform;
   Dt_ID              : GL.Uniforms.Uniform;
   Bounce_ID          : GL.Uniforms.Uniform;
   Seed_ID            : GL.Uniforms.Uniform;

   procedure Load_Buffers;
   procedure Load_Shaders;

   --  -------------------------------------------------------------------------

     procedure Init is
   begin
      Load_Shaders;
      Load_Buffers;

   exception
      when others =>
         Put_Line ("An exception occurred in Particle_System.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Load_Buffers is
      use GL.Objects.Buffers;
      use GL.Types.Singles;
   begin
     for index in 1 .. Num_Particles loop
        Vertex_Data_Array (index).Position := Maths.Random_Vector (-0.5, 0.5);
        Vertex_Data_Array (index).Position :=
              (0.0, 20.0, 0.0) + 5.0 * Vertex_Data_Array (index).Position;
      end loop;

     for index in 1 .. Num_Buffers loop
         Vertices_Array (index).Initialize_Id;
         Vertices_Array (index).Bind;
         Vertex_Buffers (index).Initialize_Id;
         Array_Buffer.Bind (Vertex_Buffers (index));
      end loop;

     for index in 1 .. Num_Buffers loop
         Array_Buffer.Bind (Vertex_Buffers (index));
         Load_Vertex_Buffer
              (Array_Buffer, Vertex_Data_Array, Static_Draw);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, 6, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, False, 6, 3);
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_Buffers.");
         raise;
   end Load_Buffers;

   --  ------------------------------------------------------------------------

   procedure Load_Shaders is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      Transform_Varyings    : constant String := "outposition,outvelocity";
      Transform_Shader_List : Program_Loader.Shader_Sources (1 .. 1);
      OK                    : Boolean;
   begin
      Particle_Program := Program_Loader.Program_From
        ((Program_Loader.Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
         Program_Loader.Src ("src/shaders/geometry_shader.glsl", Geometry_Shader),
         Program_Loader.Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));

      View_ID := GL.Objects.Programs.Uniform_Location
        (Particle_Program, "View");
      Projection_ID := GL.Objects.Programs.Uniform_Location
        (Particle_Program, "Projection");

      Transform_Shader_List (1) :=
          Program_Loader.Src ("src/shaders/transform_vertex_shader.glsl", Vertex_Shader);
      Transform_Program := Program_Loader.Program_From (Transform_Shader_List);

      Transform_Program.Use_Program;
      Transform_Feedback_Varyings (Particle_Program, Transform_Varyings, Interleaved_Attribs);
      Transform_Program.Link;
      OK := Link_Status (Transform_Program);
      if not OK then
         Put_Line ("Main_Loop.Setup, Transform_Program Link for Transform_Varyings failed");
         Put_Line (Info_Log (Transform_Program));
      end if;

      Transform_Program.Use_Program;
      Transform_Feedback_Varyings (Transform_Program, Transform_Varyings, Interleaved_Attribs);
      Transform_Program.Link;
      OK := Link_Status (Transform_Program);
      if not OK then
         Put_Line ("Main_Loop.Setup, Transform_Program Link for Out_Varying failed");
         Put_Line (Info_Log (Transform_Program));
      end if;

      Centre_ID := GL.Objects.Programs.Uniform_Location
        (Transform_Program, "center");
      Radius_ID := GL.Objects.Programs.Uniform_Location
        (Transform_Program, "radius");
      G_ID := GL.Objects.Programs.Uniform_Location
        (Transform_Program, "g");
      Dt_ID := GL.Objects.Programs.Uniform_Location
        (Transform_Program, "dt");
      Bounce_ID := GL.Objects.Programs.Uniform_Location
        (Transform_Program, "bounce");
      Seed_ID := GL.Objects.Programs.Uniform_Location
        (Transform_Program, "seed");

   exception
      when others =>
         Put_Line ("An exception occurred in Particle_System.Load_Shaders.");
         raise;
   end Load_Shaders;

   --  ------------------------------------------------------------------------

    procedure Render_Particles (Window : in out Glfw.Windows.Window) is
    use Singles;
    use Maths;
      Current_Time      : constant Single := Single (Glfw.Time);
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
      Projection_Matrix : constant Matrix4 := Perspective_Matrix
          (Degrees (90.0), 4.0 / 3.0, 0.1, 100.0);
      View_Matrix       : Matrix4 := Translation_Matrix ((0.0, 0.0, -30.0));
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));

      Transform_Program.Use_Program;

      GL.Uniforms.Set_Single (Centre_ID, Centre);
      GL.Uniforms.Set_Single (Radius_ID, Radius);
      GL.Uniforms.Set_Single (G_ID, G);
      GL.Uniforms.Set_Single (Dt_ID, Dt);
      GL.Uniforms.Set_Single (Bounce_ID, Bounce);
      GL.Uniforms.Set_Int (Seed_ID, Int (10000.0 * Abs (Maths.Random_Float)));

      if Buffer_Index = 2 then
        Buffer_Index := 1;
      else
        Buffer_Index := 2;
      end if;

      Vertices_Array (Buffer_Index).Bind;
      GL.Objects.Buffers.Transform_Feedback_Buffer.Bind_Buffer_Base
           (0, Vertex_Buffers (Buffer_Index));

      GL.Toggles.Enable (GL.Toggles.Rasterizer_Discard);
         GL.Objects.Programs.Begin_Transform_Feedback (Points);
         GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, Num_Buffers);
         GL.Objects.Programs.End_Transform_Feedback;
      GL.Toggles.Disable (GL.Toggles.Rasterizer_Discard);

      Particle_Program.Use_Program;
      View_Matrix := Rotation_Matrix (Degrees (30.0), (1.0, 0.0, 0.0)) * View_Matrix;
      View_Matrix := Rotation_Matrix (Degrees (-22.5 * Radian (Current_Time)),
                                      (0.0, 1.0, 0.0)) * View_Matrix;

      GL.Uniforms.Set_Single (View_ID, View_Matrix);
      GL.Uniforms.Set_Single  (Projection_ID, Projection_Matrix);

      Vertices_Array (Buffer_Index).Bind;
      GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, Num_Particles);

   exception
      when others =>
         Put_Line ("An exception occurred in Particle_System.Render_Particles.");
         raise;
   end Render_Particles;

   --  ------------------------------------------------------------------------

end Particle_System;
