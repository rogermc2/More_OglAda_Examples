
with GL.Types; use GL.Types;

package Shadows is

   type Shadow_Direction is (Shadow_Dir_Right, Shadow_Dir_Left,
                             Shadow_Dir_Up, Shadow_Dir_Down,
                             Shadow_Dir_Back, Shadow_Dir_Forward);
   pragma Ordered (Shadow_Direction);

   procedure Bind_Shadow_FB (Dir : Shadow_Direction);
   procedure Bind_Cube_Shadow_Texture (Slot : Integer);
   function Caster_Position return Singles.Vector3;
   procedure Init;
   procedure Set_Depth_Model_Matrix (Mat : Singles.Matrix4);
   procedure Set_Depth_Skinned_Bone_Matrices (Mats : Singles.Matrix4_Array);
   procedure Set_Depth_Skinned_Model_Matrix (Mat : Singles.Matrix4);

end Shadows;
