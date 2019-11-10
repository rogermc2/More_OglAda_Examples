
with GL.Types; use GL.Types;

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

    Single_Bytes       : Int := Single'Size / 8;
    Max_Particles      : constant Int := 100000;
    Last_Used_Particle : Int := 1;
    Particle_Container : array (1 .. Max_Particles) of Particle;

   --  -------------------------------------------------------------------------

    procedure Init (Position_Buffer : in out GL.Objects.Buffers.Buffer) is
    use GL.Objects.Buffers;
    begin
        Position_Buffer.Initialize_Id;
        Array_Buffer.Bind (Position_Buffer);
        GL.Objects.Buffers.Allocate (Array_Buffer, Long (4 * Max_Particles * Single_Bytes), Stream_Draw);
    end Init;

   --  -------------------------------------------------------------------------

    procedure Sort_Particles is
    begin
        null;
    end Sort_Particles;

   --  -------------------------------------------------------------------------

    function Unused_Particle return Int is
        Unused : Int := 0; -- If all particles are taken override the first one
    begin
        for index in Last_Used_Particle .. Max_Particles loop
            if Particle_Container (index).Life < 1.0 then
                Last_Used_Particle := index;
                Unused  := index;
            end if;
        end loop;

        return Unused;
    end Unused_Particle;

end Particle_System;
