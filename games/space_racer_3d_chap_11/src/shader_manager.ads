
with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;

package Shader_Manager is

    procedure Init_Shaders (theProgram : in out GL.Objects.Programs.Program);
    procedure Set_Colour (Program : GL.Objects.Programs.Program;
                         Colour       : GL.Types.Colors.Basic_Color);
    procedure Set_Model_Matrix (Model_Matrix : GL.Types.Singles.Matrix4);
    procedure Set_Projection_Matrix (Projection_Matrix : GL.Types.Singles.Matrix4);
    procedure Set_View_Matrix (View_Matrix : GL.Types.Singles.Matrix4);

end Shader_Manager;
