
with Maths;

with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Camera;

package Sprite_Renderer is

    Sprite_Exception : Exception;

    function Add_Sprite (Diffuse, Specular : GL.Objects.Textures.Texture;
                         Columns, Rows : Integer) return Int;
    procedure Clear_Sprites;
    function Get_Sprite_World_Pos (Sprite_Index : Int) return Singles.Vector3;
    procedure Init;
    procedure Render_Sprite (Sprite_Index : Int);
    procedure Set_Ambient_Light_Level (Rgb : Singles.Vector3);
    procedure Set_Sprite_Current_Sprite (Sprite_Index, Current_Sprite : Int);
    procedure Set_Sprite_Heading (Sprite_Index : Int;
                                  Heading_Deg : Maths.Degree);
    procedure Set_Sprite_Pitch (Sprite_Index : Int; Pitch_Deg : Maths.Degree);
    procedure Set_Sprite_Position (Sprite_Index : Int;
                                   World_Pos : Singles.Vector3);
    procedure Set_Sprite_Scale (Sprite_Index : Int; Scale : Singles.Vector3);
    procedure Set_Sprite_Visible (Sprite_Index : Int; Visible : Boolean);
    procedure Start_Sprite_Rendering;
    procedure Update_Dynamic_Light (Pos_Wor, Diff, Spec : Singles.Vector3;
                                            Light_Range : Float);
    procedure Update_Static_Lights_Uniforms;

end Sprite_Renderer;
