
with GL.Types; use GL.Types;

package Properties_Shader_Manager is

   type Light_Array is private;
   type Light_Range_Array is private;

    procedure Load_Prop_Shaders;
    procedure Set_Camera_Position (Position : Singles.Vector3);
    procedure Set_Caster_Position (Position : Singles.Vector3);
    procedure Set_Cube_Texture (Texture : UInt);
    procedure Set_Diff_Map (Diff_Map : UInt);
    procedure Set_Dyn_Light_Pos (Position : Singles.Vector3);
    procedure Set_Dyn_Light_Diff (Diff : Singles.Vector3);
    procedure Set_Dyn_Light_Spec (Spec : Singles.Vector3);
    procedure Set_Dyn_Light_Range (Light_Range : Single);
    procedure Set_Inverse_Map (Inverse_Map : Singles.Matrix4);
    procedure Set_L_A (L_A : Singles.Vector3);
    procedure Set_Light_Pos (Position : Light_Array);
    procedure Set_Light_Diff (Diff : Light_Array);
    procedure Set_Light_Spec (Spec : Light_Array);
    procedure Set_Light_Range (Light_Range : Light_Range_Array);
    procedure Set_Model (Model_Matrix : Singles.Matrix4);
    procedure Set_Norm_Map (Norm_Map : UInt);
    procedure Set_Ol_Pass (Ol_Pass : Single);
    procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4);
    procedure Set_Shadow_Enabled (Enable : Single);
    procedure Set_Spec_Map (Spec_Map : UInt);
    procedure Set_Static_Light_Indices (Indices : Singles.Vector2);
    procedure Set_View (View_Matrix : Singles.Matrix4);

    private
       type Light_Array is new Singles.Vector3_Array (1 .. 32);
       type Light_Range_Array is new Single_Array (1 .. 32);

end Properties_Shader_Manager;
