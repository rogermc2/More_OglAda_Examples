
with GL.Objects.Buffers;
with GL.Objects.Textures;
with GL.Objects.Programs;

with Maths;

with Settings;
with Texture_Manager;

package body Shadows is

    type Shadow_Data is record
        --  Camera settings
	Near             : Single := 0.0;
        Far              : Single := 0.0;
        Fovy             : Single := 0.0;
        Aspect           : Maths.Degree := 0.0;
	Caster_Vs        : Singles.Matrix4_Array (1 .. 6) :=
                             (others => (others => (others => 0.0)));
        Caster_P         : Singles.Vector4 := Maths.Vec4_0;
	Caster_Pos_Wor   : Singles.Vector3 := Maths.Vec3_0;
	--  Shaders
	Depth_Sp         : GL.Objects.Programs.Program;
        Depth_Skinned_Sp : GL.Objects.Programs.Program;
	--  Debug
--          Debug_Quad_Sp : GL.Objects.Programs.Program;
--  	Debug_Quad_Vao;
	Cube_Fb          : GL.Objects.Buffers.Buffer;
	Cube_Colour_Tex  : GL.Objects.Textures.Texture;
	Render_Buffer    : GL.Objects.Buffers.Buffer;
end record;

    G_Shadows : Shadow_Data;

--  ----------------------------------------------------------------------------

    procedure Bind_Cube_Shadow_Texture (Slot : Integer) is
    begin
        if Settings.Shadows_Enabled then
            Texture_Manager.Bind_Cube_Texture (Slot, G_Shadows.Cube_Colour_Tex);
        end if;
    end Bind_Cube_Shadow_Texture;

--  ----------------------------------------------------------------------------

    function Init_Shadows return Boolean is
    begin
        return False;
    end Init_Shadows;

--  ----------------------------------------------------------------------------

    function Caster_Position return Singles.Vector3 is
    begin
        return G_Shadows.Caster_Pos_Wor;
    end Caster_Position;

--  ---------------------------------------------------------------------

end Shadows;
