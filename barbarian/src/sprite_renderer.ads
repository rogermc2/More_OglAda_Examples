
with Maths;

with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Camera;

package Sprite_Renderer is

    Sprite_Exception : Exception;

    function Add_Sprite (Diffuse, Specular : GL.Objects.Textures.Texture;
                         Columns, Rows : Integer) return Integer;
    procedure Clear_Sprites;
    function Get_Sprite_World_Pos (Sprite_Index : Integer) return Singles.Vector3;
    procedure Init;
    procedure Render_Sprite (Sprite_Index : Integer);
    procedure Set_Ambient_Light_Level (Rgb : Singles.Vector3);
    procedure Set_Sprite_Current_Sprite (Sprite_Index, Current_Sprite : Integer);
    procedure Set_Sprite_Heading (Sprite_Index : Integer;
                                  Heading_Deg : Maths.Degree);
    procedure Set_Sprite_Pitch (Sprite_Index : Integer; Pitch_Deg : Maths.Degree);
    procedure Set_Sprite_Position (Sprite_Index : Integer;
                                   World_Pos : Singles.Vector3);
    procedure Set_Sprite_Scale (Sprite_Index : Integer; Scale : Singles.Vector3);
    procedure Set_Sprite_Visible (Sprite_Index : Integer; Visible : Boolean);
    procedure Start_Sprite_Rendering (G_Camera : Camera.Camera_Data);
    procedure Update_Sprites_Dynamic_Light (Pos_Wor, Diff, Spec : Singles.Vector3;
                                            Light_Range : Float);
    procedure Update_Sprites_Static_Lights_Uniforms;

end Sprite_Renderer;
