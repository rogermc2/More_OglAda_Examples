
with GL.Objects.Programs;
with GL.Types; use GL.Types;

package Properties_Shader_Manager is
   use GL.Objects.Programs;

    type Light_Array is new Singles.Vector3_Array (1 .. 32);
    type Light_Range_Array is new Single_Array (1 .. 32);

    Prop_Shader            : Program;  --  Basic shader
    Prop_Skinned_Shader    : Program;  --  Skinned shader
    Coins_Shader           : Program;  --  Shiny treasure shader
    Jav_Stand_Shader       : Program;  --  Pulsing 'look at me' lighting shader
    Portal_Shader          : Program;  --  Wobbly portal shader
    Bounding_Sphere_Shader : Program;  --  Bounding sphere shader

    procedure Load_Prop_Shaders;
    procedure Set_Camera_Position (Position : Singles.Vector3);
    procedure Set_Caster_Position (Position : Singles.Vector3);
    procedure Set_Cube_Texture (Texture : Int);
    procedure Set_Diff_Map (Diff_Map : Int);
    procedure Set_Dyn_Light_Pos (Position : Singles.Vector3);
    procedure Set_Dyn_Light_Diff (Diff : Singles.Vector3);
    procedure Set_Dyn_Light_Spec (Spec : Singles.Vector3);
    procedure Set_Dyn_Light_Range (Light_Range : Single);
    procedure Set_Inverse_Matrix (Inverse_Matrix : Singles.Matrix4);
    procedure Set_L_A (L_A : Singles.Vector3);
    procedure Set_Light_Pos (Position : Light_Array);
    procedure Set_Light_Diff (Diff : Light_Array);
    procedure Set_Light_Spec (Spec : Light_Array);
    procedure Set_Light_Range (Light_Range : Light_Range_Array);
    procedure Set_Model (Model_Matrix : Singles.Matrix4);
    procedure Set_Norm_Map (Norm_Map : Int);
    procedure Set_Outline_Pass (Ol_Pass : Single);
    procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4);
    procedure Set_Shadows_Enabled (Enable : Single);
    procedure Set_Spec_Map (Spec_Map : Int);
    procedure Set_Static_Light_Indices (Indices : Ints.Vector2);
    procedure Set_View (View_Matrix : Singles.Matrix4);

end Properties_Shader_Manager;
