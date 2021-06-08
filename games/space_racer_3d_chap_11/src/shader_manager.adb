
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Uniforms;

with Program_Loader;

package body Shader_Manager is

    Colour_Uniform      : GL.Uniforms.Uniform;
    Model_Uniform       : GL.Uniforms.Uniform;
    View_Uniform        : GL.Uniforms.Uniform;
    Projection_Uniform  : GL.Uniforms.Uniform;

    procedure Init_Shaders (theProgram : in out GL.Objects.Programs.Program) is
        use GL.Objects.Shaders;
        use Program_Loader;
    begin
        theProgram := Program_From
          ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));
        GL.Objects.Programs.Use_Program (theProgram);

        Colour_Uniform :=
          GL.Objects.Programs.Uniform_Location (theProgram, "colour_in");
        Model_Uniform :=
          GL.Objects.Programs.Uniform_Location (theProgram, "model_matrix");
        Projection_Uniform :=
          GL.Objects.Programs.Uniform_Location (theProgram, "projection_matrix");
        View_Uniform :=
          GL.Objects.Programs.Uniform_Location (theProgram, "view_matrix");

        GL.Objects.Programs.Use_Program (theProgram);
        GL.Uniforms.Set_Single (Model_Uniform, GL.Types.Singles.Identity4);
        GL.Uniforms.Set_Single (Projection_Uniform, GL.Types.Singles.Identity4);
        GL.Uniforms.Set_Single (View_Uniform, GL.Types.Singles.Identity4);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Shader_Manager.Init_Shaders.");
            Put_Line (Exception_Information (anError));
            raise;
    end Init_Shaders;

    --  ------------------------------------------------------------------------

    procedure Set_Colour (Program : GL.Objects.Programs.Program;
                          Colour : GL.Types.Colors.Basic_Color) is
        use GL.Types.Colors;
        Colour_Vec : constant GL.Types.Singles.Vector3 :=
                       (Colour (R), Colour (G), Colour (B));
   begin
      GL.Objects.Programs.Use_Program (Program);
        GL.Uniforms.Set_Single (Colour_Uniform, Colour_Vec);
    end Set_Colour;

    --   -----------------------------------------------------------------------

    procedure Set_Model_Matrix (Model_Matrix : GL.Types.Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Model_Uniform, Model_Matrix);
    end Set_Model_Matrix;

    --   ---------------------------------------------------------------------------------

    procedure Set_Projection_Matrix (Projection_Matrix : GL.Types.Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Projection_Uniform, Projection_Matrix);
    end Set_Projection_Matrix;

    --   ---------------------------------------------------------------------------------

    procedure Set_View_Matrix (View_Matrix : GL.Types.Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (View_Uniform, View_Matrix);
    end Set_View_Matrix;

    --   ---------------------------------------------------------------------------------

end Shader_Manager;
