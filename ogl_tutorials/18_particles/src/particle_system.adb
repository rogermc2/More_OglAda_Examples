
with Maths;
--  with Utilities;

package body Particle_System is

    type Particle is record
        Position        : Singles.Vector3;
        Speed           : Singles.Vector3;
        Diameter        : Single;
        Angle           : Single;
        Weight          : Single;
        Life            : Single := -1.0;
        Camera_Distance : Single := -1.0;
    end record;

    Single_Bytes       : constant Int := Single'Size / 8;

    Last_Used_Particle : Int := 1;
    Particle_Container : array (1 .. Max_Particles) of Particle;
    Position_Size_Data : Single_Array (1 .. 4 * Max_Particles);
    Colour_Data        : Single_Array (1 .. 4 * Max_Particles);

    --  -------------------------------------------------------------------------

    procedure Init (Position_Buffer, Colour_Buffer : in out GL.Objects.Buffers.Buffer) is
        use GL.Objects.Buffers;
    begin
        Position_Buffer.Initialize_Id;
        Array_Buffer.Bind (Position_Buffer);
        GL.Objects.Buffers.Allocate (Array_Buffer, Long (4 * Max_Particles * Single_Bytes), Stream_Draw);

        Colour_Buffer.Initialize_Id;
        Array_Buffer.Bind (Colour_Buffer);
        GL.Objects.Buffers.Allocate (Array_Buffer, Long (4 * Max_Particles * Single_Bytes), Stream_Draw);
    end Init;

    --  -------------------------------------------------------------------------

    procedure Sort_Particles is
    begin
        null;
    end Sort_Particles;

    --  -------------------------------------------------------------------------

    function Find_Unused_Particle return Int is
        Unused : Int := 0; -- If all particles are taken override the first one
    begin
        for index in Last_Used_Particle .. Max_Particles loop
            if Particle_Container (index).Life < 1.0 then
                Last_Used_Particle := index;
                Unused  := index;
            end if;
        end loop;

        return Unused;
    end Find_Unused_Particle;

    --  -------------------------------------------------------------------------

    procedure Update_Particles (Delta_Time : Single) is
        use GL.Types.Singles;
        New_Particles    : Int := 10000 * Int (Delta_Time);
        Particle_Index   : Int;
        Spread           : constant Single := 1.5;
        Main_Direction   : constant Vector3 := (0.0, 10.0, 0.0);
        Random_Direction : Vector3;
    begin
        if New_Particles > 160 then
            New_Particles := 160;
        end if;
        for index in 1 .. New_Particles loop
            Random_Direction := (Maths.Random_Float,
                                 Maths.Random_Float,
                                 Maths.Random_Float);
            Particle_Index := Find_Unused_Particle;
            Particle_Container (Particle_Index).Life := 5.0;
            Particle_Container (Particle_Index).Position := (0.0, 0.0, -20.0);
            Particle_Container (Particle_Index).Speed := Main_Direction + Spread * Random_Direction;
        end loop;
    end Update_Particles;

    --  -------------------------------------------------------------------------
end Particle_System;
