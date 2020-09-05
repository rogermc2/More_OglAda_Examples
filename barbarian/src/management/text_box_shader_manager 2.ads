
with GL.Types; use GL.Types;
with GL.Objects.Programs;

package Text_Box_Shader_Manager is

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Colour_ID (Colour : Singles.Vector4);
    procedure Set_Position_ID (Position : Singles.Vector2);
    procedure Set_Scale (Scale : Singles.Vector2);
    procedure Set_Viewport_Dimensions (Dimensions : Singles.Vector2);

end Text_Box_Shader_Manager;
