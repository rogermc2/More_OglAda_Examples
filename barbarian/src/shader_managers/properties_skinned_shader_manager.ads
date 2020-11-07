
with GL.Objects.Programs;
with GL.Types; use GL.Types;
with Properties_Shader_Manager;

package Properties_Skinned_Shader_Manager is
   use Properties_Shader_Manager;

   procedure Init_Prop_Skinned_Shader (Prop_Shader : out GL.Objects.Programs.Program);
   procedure Set_Bone_Matrices (Bone_Matrices : Singles.Matrix4_Array);
   procedure Set_Caster_Position (Position : Singles.Vector3);
   procedure Set_Cube_Texture (Texture : Int);
   procedure Set_Diff_Map (Diff_Map : Int);
   procedure Set_Dyn_Light_Pos (Position : Singles.Vector3);
   procedure Set_Dyn_Light_Diff (Diff : Singles.Vector3);
   procedure Set_Dyn_Light_Spec (Spec : Singles.Vector3);
   procedure Set_Dyn_Light_Range (Light_Range : Single);
   procedure Set_L_A (L_A : Singles.Vector3);
   procedure Set_Light_Pos (Position : Light_Array);
   procedure Set_Light_Diff (Diff : Light_Array);
   procedure Set_Light_Spec (Spec : Light_Array);
   procedure Set_Light_Range (Light_Range : Light_Range_Array);
   procedure Set_Model (Model_Matrix : Singles.Matrix4);
   procedure Set_Ol_Pass (Ol_Pass : Single);
   procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4);
   procedure Set_Shadow_Enabled (Enable : Single);
   procedure Set_Spec_Map (Spec_Map : Int);
   procedure Set_Static_Light_Indices (Indices : Ints.Vector2);
   procedure Set_View (View_Matrix : Singles.Matrix4);

end Properties_Skinned_Shader_Manager;
