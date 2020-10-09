
with GL.Types; use GL.Types;

package Shader_Manager is

   procedure Init;
   procedure Set_Model_Matrix (Program_Index : Positive;
                               Model_Matrix  : Singles.Matrix4);
   procedure Set_Projection_Matrix (Program_Index : Positive;
                                    Projection_Matrix: Singles.Matrix4);
   procedure Set_View_Matrix (Program_Index : Positive;
                              View_Matrix  : Singles.Matrix4);

end Shader_Manager;
