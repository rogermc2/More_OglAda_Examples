
with Interfaces.C.Pointers;

with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;
with GL.Uniforms;
with GL.Window;

with Maths;
with Program_Loader;
--  with Utilities;

package body Particle_System is

    type Vertex is record
        Position : Singles.Vector3 := (0.0, 0.0, 0.0);
        Velocity : Singles.Vector3 := (0.0, 0.0, 0.0);
    end record;
    type Vertex_Array is array (Int range <>) of aliased Vertex;

    package Vertex_Pointers is new Interfaces.C.Pointers
      (Int, Vertex, Vertex_Array, Vertex'(others => <>));
    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (Vertex_Pointers);

    type Random_Int_Range is new Int range 0 .. 3;
    package Int_Random_Package is new Ada.Numerics.Discrete_Random (Random_Int_Range);

    Gen_Int            : Int_Random_Package.Generator;

    Num_Particles      : constant Int := 128 * 1024;
    Vertex_Data_Array  : Vertex_Array (1 .. Num_Particles);  --  (3.0, 7.0, 12.0);
    Vertices           : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer      : GL.Objects.Buffers.Buffer;
    Particle_Program   : GL.Objects.Programs.Program;
    View_ID            : GL.Uniforms.Uniform;
    Projection_ID      : GL.Uniforms.Uniform;

    procedure Load_Buffers;
    procedure Load_Shaders;

    --  -------------------------------------------------------------------------

    function Random_4 return Single is
        use Maths;
    begin
        return 2.0 * Abs (Random_Float) + Abs (Random_Float) +
        Abs (Random_Float) + Abs (Random_Float);
    end Random_4;

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
        use Int_Random_Package;
        use Maths;
        use Maths.Single_Math_Functions;  -- Needed for single powers
        Arm     : Random_Int_Range;
        Alpha   : Single;
        R       : Single;
    begin
        Vertices.Initialize_Id;
        Vertices.Bind;

        --  Create a galaxylike distribution of points
        for index in 1 .. Num_Particles loop
            Arm := Random (Gen_Int);
            Alpha := 1.0 / (0.1 + (Abs (Random_Float)) ** 0.7) - 1.0 / 1.1;
            R := 4.0 * Alpha;
            Alpha := Alpha + 2.0 * Ada.Numerics.Pi * Single (Arm) / 3.0;
            Vertex_Data_Array (index).Position := (R * Sin (Alpha), 0.0, R * Cos (Alpha));
            Vertex_Data_Array (index).Position := Vertex_Data_Array (index).Position +
              (Random_4 * (4.0 - 0.2 * Alpha),
               Random_4 * (2.0 - 0.1 * Alpha),
               Random_4 * (4.0 - 0.2 * Alpha));
        end loop;

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);

        Load_Vertex_Buffer
          (Array_Buffer, Vertex_Data_Array, Static_Draw);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, 0, 0);

    exception
        when others =>
            Put_Line ("An exception occurred in Load_Buffers.");
            raise;
    end Load_Buffers;

    --  ------------------------------------------------------------------------

    procedure Load_Shaders is
        use GL.Objects.Shaders;
    begin
        --  The vertex shader passes through position data to the pipeline.
        --  The geometry shader creates the billboard quads.
        --  The fragment shader creates a bell like radial color distribution.
        Particle_Program := Program_Loader.Program_From
          ((Program_Loader.Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
            Program_Loader.Src ("src/shaders/geometry_shader.glsl", Geometry_Shader),
            Program_Loader.Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));

        View_ID := GL.Objects.Programs.Uniform_Location
          (Particle_Program, "View");
        Projection_ID := GL.Objects.Programs.Uniform_Location
          (Particle_Program, "Projection");

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Load_Shaders.");
            raise;
    end Load_Shaders;

    --  ------------------------------------------------------------------------

    procedure Render_Particles (Window : in out Glfw.Windows.Window) is
        use Singles;
        use Maths;
        use Maths.Single_Math_Functions;
        Current_Time      : constant Single := Single (Glfw.Time);  --  Seconds
        Window_Width      : Glfw.Size;
        Window_Height     : Glfw.Size;
        Projection_Matrix : constant Matrix4 := Perspective_Matrix
          (Degrees (90.0), 4.0 / 3.0, 0.1, 100.0);
        Scale_Matrix      : constant Matrix4 := Scaling_Matrix (2.5);
        View_Matrix       : Matrix4 :=  Scale_Matrix;
    begin
        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                                GL.Types.Int (Window_Height));

        --  Rotate the camera around the origin
        View_Matrix := Rotation_Matrix (Degrees (30.0 * Radian (Sin (0.01 * Current_Time))),
                                        (1.0, 0.0, 0.0)) * View_Matrix;
        View_Matrix := Rotation_Matrix (Degrees (-22.5 * Radian (0.01 * Current_Time)),
                                        (0.0, 1.0, 0.0)) * View_Matrix;

        View_Matrix := Translation_Matrix ((0.0, 0.0, -50.0)) * View_Matrix;

        Particle_Program.Use_Program;
        GL.Uniforms.Set_Single (View_ID, View_Matrix);
        GL.Uniforms.Set_Single  (Projection_ID, Projection_Matrix);

        Vertices.Bind;
        GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, Num_Particles);

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Render_Particles.");
            raise;
    end Render_Particles;

    --  ------------------------------------------------------------------------

end Particle_System;
