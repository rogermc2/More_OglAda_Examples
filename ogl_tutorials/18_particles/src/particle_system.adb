
with Ada.Containers.Generic_Array_Sort;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
--  with GL.Objects.Shaders.Lists;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;

with Load_DDS;
with Maths;
with Program_Loader;
with Utilities;

package body Particle_System is
    use Ada.Containers;

    type Particle is record
        Position        : Singles.Vector3;
        Speed           : Singles.Vector3;
        Diameter        : Single;
        Angle           : Single;
        Weight          : Single;
        Colour          : GL.Types.Colors.Color;
        Life            : Single := -1.0;
        Camera_Distance : Single := -1.0;
    end record;

    type Particle_Array is array (Int range <>) of Particle;

    function "<" (Left, Right : Particle) return Boolean is
    begin
        return Maths.Length (Left.Position) < Maths.Length (Right.Position);
    end "<";

    procedure Sort_Particles is new Generic_Array_Sort (Index_Type   => GL.Types.Int,
                                                        Element_Type => Particle,
                                                        Array_Type   => Particle_Array,
                                                        "<"          => "<");

    Single_Bytes           : constant Int := Single'Size / 8;

    Max_Particles          : constant Int := 100000;
    Particle_Count         : Int;
    Particle_Program       : GL.Objects.Programs.Program;
    Colour_Buffer          : GL.Objects.Buffers.Buffer;
    Positions_Buffer       : GL.Objects.Buffers.Buffer;
    Particle_Texture       : GL.Objects.Textures.Texture;
    Buffer_Size            : constant Long := 4 * Long (Max_Particles * Single_Bytes);
    Camera_Right_ID        : GL.Uniforms.Uniform;
    Camera_Up_ID           : GL.Uniforms.Uniform;
    View_Point_ID          : GL.Uniforms.Uniform;
    Texture_ID             : GL.Uniforms.Uniform;
    Last_Used_Particle     : Int := 1;
    Particle_Container     : Particle_Array (1 .. Max_Particles);
    Position_Size_Data     : Singles.Vector4_Array (1 .. Max_Particles);
    Colour_Data            : Singles.Vector4_Array (1 .. Max_Particles);

    procedure Load_Buffers;
    procedure Load_Shaders;

    --  -------------------------------------------------------------------------

    function Get_Colour_Data return Singles.Vector4_Array is
    begin
        return Colour_Data;
    end Get_Colour_Data;

    --  -------------------------------------------------------------------------

    function Get_Position_Size_Data return Singles.Vector4_Array  is
    begin
        return Position_Size_Data;
    end Get_Position_Size_Data;

    --  -------------------------------------------------------------------------

    procedure Init is
        use GL.Objects.Buffers;
    begin
        Load_Shaders;

        Positions_Buffer.Initialize_Id;
        Array_Buffer.Bind (Positions_Buffer);
        Load_Buffers;
        GL.Objects.Buffers.Allocate (Array_Buffer, Long (4 * Max_Particles * Single_Bytes), Stream_Draw);

        Colour_Buffer.Initialize_Id;
        Array_Buffer.Bind (Colour_Buffer);
        GL.Objects.Buffers.Allocate (Array_Buffer, Long (4 * Max_Particles * Single_Bytes), Stream_Draw);

        Load_DDS ("src/textures/particle.DDS", Particle_Texture);

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Init.");
            raise;
    end Init;

    --  -------------------------------------------------------------------------

    function Find_Unused_Particle return Int is
        Unused : Int := 0; -- If all particles are taken override the first one
    begin
        for index in Last_Used_Particle .. Max_Particles loop
            if Particle_Container (index).Life < 0.0 then
                Last_Used_Particle := index;
                Unused  := index;
            end if;
        end loop;

        for index in 1 .. Last_Used_Particle loop
            if Particle_Container (index).Life < 0.0 then
                Last_Used_Particle := index;
                Unused  := index;
            end if;
        end loop;

        return Unused;

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Find_Unused_Particle.");
            raise;
    end Find_Unused_Particle;

    --  -------------------------------------------------------------------------

    procedure Load_Buffers is
        use GL.Objects.Buffers;
    begin

        Positions_Buffer.Initialize_Id;
        Array_Buffer.Bind (Positions_Buffer);
        Allocate (Array_Buffer, Buffer_Size, Static_Draw);

        Colour_Buffer.Initialize_Id;
        Array_Buffer.Bind (Colour_Buffer);
        Allocate (Array_Buffer, Buffer_Size, Static_Draw);

    exception
        when others =>
            Put_Line ("An exception occurred in Load_Buffers.");
            raise;
    end Load_Buffers;

    --  ------------------------------------------------------------------------

    procedure Load_Shaders is
        use GL.Objects.Shaders;
    begin
        Particle_Program := Program_Loader.Program_From
          ((Program_Loader.Src ("src/shaders/particle_vertex_shader.glsl",
           Vertex_Shader),
           Program_Loader.Src ("src/shaders/particle_fragment_shader.glsl",
             Fragment_Shader)));

        Camera_Right_ID := GL.Objects.Programs.Uniform_Location
          (Particle_Program, "CameraRight_worldspace");
        Camera_Up_ID := GL.Objects.Programs.Uniform_Location
          (Particle_Program, "CameraUp_worldspace");
        View_Point_ID := GL.Objects.Programs.Uniform_Location
          (Particle_Program, "VP");

        Texture_ID := GL.Objects.Programs.Uniform_Location
          (Particle_Program, "myTextureSampler");

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Load_Shaders.");
            raise;
    end Load_Shaders;

    --  ------------------------------------------------------------------------

    procedure Render_Particles is
    use GL.Blending;
    use GL.Objects.Buffers;
    use GL.Objects.Textures;
    begin
        Array_Buffer.Bind (Positions_Buffer);

        --  Buffer orphaning, a common way to improve streaming performance.
        --  See http://www.opengl.org/wiki/Buffer_Object_Streaming
        Allocate (Array_Buffer, Buffer_Size, Stream_Draw);
        Utilities.Load_Vertex_Sub_Buffer (Array_Buffer, 0, Position_Size_Data);

        Array_Buffer.Bind (Colour_Buffer);
        Allocate (Array_Buffer, Buffer_Size, Stream_Draw);
        Utilities.Load_Vertex_Sub_Buffer (Array_Buffer, 0, Colour_Data);

        GL.Toggles.Enable (GL.Toggles.Blend);
        Set_Blend_Func (Src_Alpha, One_Minus_Src_Alpha);

        GL.Objects.Programs.Use_Program (Particle_Program);
        Set_Active_Unit (0);
        GL.Objects.Textures.Targets.Texture_2D.Bind (Particle_Texture);
        GL.Uniforms.Set_Int (Texture_ID, 0);

        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        Array_Buffer.Bind (Positions_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 4, Single_Type, False, 0, 0);

        GL.Attributes.Enable_Vertex_Attrib_Array (2);
        Array_Buffer.Bind (Colour_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (2, 4, UByte_Type, True, 0, 0);

        --  These functions are specific to glDrawArrays*Instanced*.
	--  The first parameter is the attribute buffer we're talking about.
	--  The second parameter is the "rate at which generic vertex attributes
        --  advance when rendering multiple instances"
        GL.Attributes.Vertex_Attrib_Divisor (1, 1);  --  positions : one per quad (its center)
        GL.Attributes.Vertex_Attrib_Divisor (2, 1);  --  colour : one per quad

        GL.Objects.Vertex_Arrays.Draw_Arrays_Instanced (Triangle_Strip, 0, 4, Particle_Count);
        GL.Attributes.Disable_Vertex_Attrib_Array (1);
        GL.Attributes.Disable_Vertex_Attrib_Array (2);

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Render_Particles.");
            raise;
    end Render_Particles;

    --  ------------------------------------------------------------------------

    procedure Set_Texture_ID (Tex : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Texture_ID, Tex);
    end Set_Texture_ID;

    --  ------------------------------------------------------------------------

    procedure Set_IDs (VP : Singles.Matrix4) is
        Right  : constant Singles.Vector3 :=
                   (VP (GL.X, GL.X), VP (GL.Y, GL.X), VP (GL.X, GL.Z));
        Up     : constant Singles.Vector3 :=
                   (VP (GL.X, GL.Y), VP (GL.Y, GL.Y), VP (GL.Z, GL.Y));
    begin
        GL.Objects.Programs.Use_Program (Particle_Program);
        GL.Uniforms.Set_Single (View_Point_ID, VP);
        GL.Uniforms.Set_Single  (Camera_Right_ID, Right);
        GL.Uniforms.Set_Single (Camera_Up_ID, Up);

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Set_IDs.");
            raise;
    end Set_IDs;

    --  ------------------------------------------------------------------------

    procedure Update_Particles (Delta_Time : Single) is
        use GL.Types.Colors;
        use GL.Types.Singles;
        New_Particles    : Int := 10000 * Int (Delta_Time);
        Particle_Index   : Int;
        aParticle        : Particle;
        Spread           : constant Single := 1.5;
        Main_Direction   : constant Vector3 := (0.0, 10.0, 0.0);
        Random_Direction : Vector3;
    begin
        if New_Particles > 160 then
            New_Particles := 160;
        end if;

        Particle_Count := 0;
        for index in 1 .. New_Particles loop
            Particle_Index := Find_Unused_Particle;
            Random_Direction := (Maths.Random_Float,
                                 Maths.Random_Float,
                                 Maths.Random_Float);
            Particle_Container (Particle_Index).Life := 5.0;
            Particle_Container (Particle_Index).Position := (0.0, 0.0, -20.0);
            Particle_Container (Particle_Index).Speed :=
              Main_Direction + Spread * Random_Direction;
            Particle_Container (Particle_Index).Colour :=
              (Maths.Random_Float / 3.0,
               Maths.Random_Float / 3.0,
               Maths.Random_Float / 3.0,
               Maths.Random_Float / 3.0);
            Particle_Container (Particle_Index).Diameter :=
              Maths.Random_Float / 2.0 + 0.1;

            for index in 1 .. Max_Particles loop
                aParticle := Particle_Container (index);
                if aParticle.Life > 0.0 then
                    Particle_Count := Particle_Count + 1;
                    aParticle.Life := aParticle.Life - Delta_Time;
                    if aParticle.Life > 0.0 then
                        aParticle.Speed :=
                          aParticle.Speed + 0.5 * Delta_Time * (0.0, -9.81, 0.0);
                        aParticle.Position :=
                          aParticle.Position + Delta_Time * aParticle.Speed;
                        aParticle.Camera_Distance :=
                          Maths.Length (aParticle.Position) - aParticle.Camera_Distance;

                        Position_Size_Data (Particle_Count) :=
                          (aParticle.Position (GL.X), aParticle.Position (GL.Y),
                           aParticle.Position (GL.Z), aParticle.Diameter);

                        Colour_Data (Particle_Count) :=
                          (aParticle.Colour (R), aParticle.Colour (G),
                          aParticle.Colour (B), aParticle.Colour (A));
                    else
                        --  Particles that just died will be put
                        --  at the end of the buffer in Sort_Particles
                        aParticle.Camera_Distance := -1.0;
                    end if;
                end if;
            end loop;
        end loop;

        Sort_Particles (Particle_Container);

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Update_Particles.");
            raise;
    end Update_Particles;

    --  -------------------------------------------------------------------------

--      procedure Use_Program is
--          use GL.Objects.Shaders.Lists;
--      begin
--          if GL.Objects.Programs.Link_Status (Particle_Program) then
--              declare
--                  Shaders_List : constant GL.Objects.Shaders.Lists.List :=
--                                   GL.Objects.Programs.Attached_Shaders  (Particle_Program);
--                  Curs         : constant GL.Objects.Shaders.Lists.Cursor :=
--                                   Shaders_List.First;
--              begin
--                  if Curs = GL.Objects.Shaders.Lists.No_Element then
--                      Put_Line ("Particle_System.Use_Program, Shaders list is empty");
--                  else
--                      GL.Objects.Programs.Use_Program (Particle_Program);
--                  end if;
--              end;  -- declare block
--          else
--              Put_Line ("Particle_System.Use_Program, program link check failed");
--          end if;
--
--      exception
--          when  others =>
--              Put_Line ("An exception occurred in Particle_System.Use_Program.");
--              raise;
--      end Use_Program;

    --  -------------------------------------------------------------------------

end Particle_System;
