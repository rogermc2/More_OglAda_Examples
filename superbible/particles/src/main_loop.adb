
with Interfaces.C.Pointers;

with Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Window;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
    use GL.Types;

    type Particle is record
        Position : Singles.Vector3;
        Velocity : Singles.Vector3;
    end record;
    type Particles_Array is array (UInt range <>) of aliased Particle;

    package Particle_Buffer_Package is new Interfaces.C.Pointers
      (UInt, Particle, Particles_Array, Particle'(others => <>));
    procedure Map_Particle_Buffer_Range is new
      GL.Objects.Buffers.Map_Range (Particle_Buffer_Package);

    Draw_Program        : GL.Objects.Programs.Program;
    Vertex_Array        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Particle_Buffer     : GL.Objects.Buffers.Buffer;
    Mapped_Buffer_Ptr   : Particle_Buffer_Package.Pointer;
    Particles_Ptr_Array : array (1 .. 2) of Particle_Buffer_Package.Pointer;
    Frame_Index         : UInt := 0;
    Previous_Time       : Single := 0.0;

    Num_Particles       : constant UInt := 2048;
    Particles           : Particles_Array (1 .. Num_Particles);
    Buffer_Size         : constant Size := Size (Num_Particles * Particle'Size / 8);
    Map_Access          : GL.Objects.Buffers.Map_Bits;

    function Random_Float return Single;
    procedure Update_Particles (Delta_Time : Single);

    --  ----------------------------------------------------------------------------

    procedure Initialize_Particles is
        use Singles;
        Buffer_Pointer : Particle_Buffer_Package.Pointer := Mapped_Buffer_Ptr;
    begin
        for count in 1 .. Num_Particles loop
            Particles (count).Position := (0.0, 0.0, 0.0);
            Particles (count).Velocity := (0.0, 0.0, 0.0);

            Buffer_Pointer.all := Particles (count);
            Particle_Buffer_Package.Increment (Buffer_Pointer);
        end loop;
        Particles_Ptr_Array (2) := Mapped_Buffer_Ptr;

        Buffer_Pointer := Mapped_Buffer_Ptr;
        for count in 1 .. Num_Particles loop
            Particles (count).Position (GL.X) := Random_Float * 6.0 - 3.0;
            Particles (count).Position (GL.Y) := Random_Float * 6.0 - 3.0;
            Particles (count).Position (GL.Z) := Random_Float * 6.0 - 3.0;
            Particles (count).Velocity := 0.001 * Particles (count).Position;

            Buffer_Pointer.all := Particles (count);
            Particle_Buffer_Package.Increment (Buffer_Pointer);
        end loop;
        Particles_Ptr_Array (1) := Mapped_Buffer_Ptr;

    exception
        when others =>
          Put_Line ("An exceptiom occurred in Initialize_Particles.");
        raise;
    end Initialize_Particles;

    --  ----------------------------------------------------------------------------

    function Random_Float return Single is
        use Ada.Numerics.Float_Random;
        Gen      : Generator;
    begin
        return Single (Random (Gen));
    end Random_Float;

    --  ------------------------------------------------------------------------

    procedure Render_Particles (Window : in out Glfw.Windows.Window;
                                Current_Time : Glfw.Seconds) is
        Black         : constant GL.Types.Colors.Color := (0.8, 0.8, 0.8, 1.0);
        Window_Width  : Glfw.Size;
        Window_Height : Glfw.Size;
        Delta_Time    : constant Single := Single (Current_Time) - Previous_Time;
    begin
        Previous_Time := Single (Current_Time);

        Utilities.Clear_Background_Colour_And_Depth (Black);
        Glfw.Windows.Get_Size (Window'Access, Window_Width, Window_Height);
        GL.Window.Set_Viewport (0, 0, Int (Window_Width), Int (Window_Height));

        Update_Particles (0.001 * Delta_Time);

        Vertex_Array.Bind;
        GL.Objects.Buffers.Array_Buffer.Flush_Mapped_Buffer_Range (0, Buffer_Size);

        GL.Objects.Programs.Use_Program (Draw_Program);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, Int (Num_Particles));

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Render_Particles.");
            raise;
    end Render_Particles;

    --  ----------------------------------------------------------------------------

    procedure Setup is
        use Program_Loader;
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);

        -- Point size is set in the vertex shader
        GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);

        Particle_Buffer.Initialize_Id;
        Array_Buffer.Bind (Particle_Buffer);

        Map_Access.Write := True;
        Map_Access.Persistent := True;
        Array_Buffer.Allocate (Long (Buffer_Size), Dynamic_Draw);

        Map_Access.Persistent := False;  --  Map_Particle_Buffer_Range fails otherwise
        Map_Access.Flush_Explicit := True;
        Map_Particle_Buffer_Range
          (Array_Buffer, Map_Access, 0, Buffer_Size, Mapped_Buffer_Ptr);

        Initialize_Particles;
        GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 1,
                                                 Kind   => Single_Type,
                                                 Normalized => True,
                                                 Stride => 2, Offset => 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);

        Draw_Program :=
          Program_From ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
                         Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));

    exception
        when others =>
          Put_Line ("An exceptiom occurred in Setup.");
        raise;
    end Setup;

    --  ----------------------------------------------------------------------------

    procedure Update_Particles (Delta_Time : Single) is
        use Singles;
        Buffer_Pointer : Particle_Buffer_Package.Pointer := Mapped_Buffer_Ptr;
        Source_1_Ptr   : Particle_Buffer_Package.Pointer;
        Source_2_Ptr   : Particle_Buffer_Package.Pointer;
        Dest_Ptr       : Particle_Buffer_Package.Pointer;
        Source_1       : Particle;
        Source_Index   : constant Integer := Integer (Frame_Index) Rem 2 + 1;
        Dest_Index     : constant Integer := Integer (Frame_Index + 1) Rem 2 + 1;
        Delta_Pos      : Vector3;
        Delta_Vel      : Vector3 := (0.0, 0.0, 0.0);
        Delta_Dir      : Vector3;
        Distance       : Single;
    begin
        Source_1_Ptr := Particles_Ptr_Array (Source_Index);
        Dest_Ptr := Particles_Ptr_Array (Dest_Index);
          for count in 1 .. Num_Particles loop
            Source_1 := Source_1_Ptr.all;
            Delta_Vel := Delta_Time * (0.0, 0.0, 0.0);
            Source_2_Ptr := Particles_Ptr_Array (Source_Index);
--              Put_Line ("Update_Particles count: " & UInt'Image (count));
            for count_2 in 1 .. Num_Particles loop
                if count_2 /= count then
                    Delta_Pos := Source_2_Ptr.Position - Source_1.Position;
                    if Maths.Length (Delta_Pos) /= 0.0 then
                        Delta_Dir := Maths.Normalized (Delta_Pos);
                    else
                        Delta_Dir := (0.0, 0.0, 1.0);
                    end if;
                    Distance := Maths.Length (Delta_Pos);
                    if Distance < 0.005 then
                        Distance := 0.005;
                    end if;
                    Delta_Vel := Delta_Vel + Delta_Dir / (Distance * Distance);
                end if;
--                  Put_Line ("Update_Particles counts: " & UInt'Image (count) &
--                              "  " & UInt'Image (count_2));
--                  Dest_Ptr.Position := Delta_Time * (0.0, 0.0, 1.0);
                Dest_Ptr.Position := Source_1_Ptr.Position + Source_1_Ptr.Velocity;
                Dest_Ptr.Velocity := Source_1_Ptr.Velocity + 0.01 * Delta_Vel * Delta_Time;
                 Particle_Buffer_Package.Increment (Source_2_Ptr);
            end loop;
            Mapped_Buffer_Ptr.Position := Dest_Ptr.Position;
            Particle_Buffer_Package.Increment (Source_1_Ptr);
            Particle_Buffer_Package.Increment (Dest_Ptr);
            Particle_Buffer_Package.Increment (Buffer_Pointer);
        end loop;
        Frame_Index := Frame_Index + 1;
    exception
        when others =>
          Put_Line ("An exceptiom occurred in Update_Particles.");
        raise;
    end Update_Particles;

    --  ----------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup;
    while Running loop
        Render_Particles (Main_Window, Glfw.Time);
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
