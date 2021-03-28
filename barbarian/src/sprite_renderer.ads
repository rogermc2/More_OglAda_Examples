
with Maths;

with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Camera;

package Sprite_Renderer is

    Sprite_Exception : Exception;

    function Add_Sprite (Diffuse, Specular : GL.Objects.Textures.Texture;
                         Rows, Columns : Integer) return Natural;
    procedure Clear_Sprites;
    function Sprite_World_Pos (Sprite_Index : Positive) return Singles.Vector3;
    procedure Init;
    procedure Render_Sprite (Sprite_Index : Positive);
    procedure Set_Ambient_Light_Level (Rgb : Singles.Vector3);
    procedure Set_Sprite_Current_Sprite (Sprite_Index, Current_Sprite : Natural);
    procedure Set_Sprite_Heading (Sprite_Index : Positive;
                                  Heading_Deg : Maths.Degree);
    procedure Set_Sprite_Pitch (Sprite_Index : Positive; Pitch_Deg : Maths.Degree);
    procedure Set_Sprite_Position (Sprite_Index : Positive;
                                   World_Pos : Singles.Vector3);
    procedure Set_Sprite_Scale (Sprite_Index : Positive; Scale : Singles.Vector3);
    procedure Set_Sprite_Visible (Sprite_Index : Positive; Visible : Boolean);
    procedure Start_Sprite_Rendering;
    procedure Update_Dynamic_Light (Pos_Wor, Diff, Spec : Singles.Vector3;
                                    Light_Range : Single);
    procedure Update_Static_Lights_Uniforms;

end Sprite_Renderer;
