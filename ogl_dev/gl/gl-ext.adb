
with Interfaces.C; use Interfaces.C;
with GL.API.Ext;

package body GL.Ext is

    procedure Set_Geometry_Input_Type (theProgram : GL.Objects.Programs.Program;
                                       Input_Type : Connection_Mode) is
    begin
      GL.API.Ext.Program_Parameter (theProgram.Raw_Id, GL_GEOMETRY_INPUT_TYPE_EXT,
                                    GL.Types.Int (Input_Type'Enum_Rep));
      Raise_Exception_On_OpenGL_Error;
    end Set_Geometry_Input_Type;

    --  ----------------------------------------------------------------------------

    procedure Set_Geometry_Output_Type (theProgram : GL.Objects.Programs.Program;
                                       Output_Type : Connection_Mode) is
    begin
      GL.API.Ext.Program_Parameter (theProgram.Raw_Id, GL_GEOMETRY_OUTPUT_TYPE_EXT,
                                    GL.Types.Int (Output_Type'Enum_Rep));
      Raise_Exception_On_OpenGL_Error;
    end Set_Geometry_Output_Type;

    --  ----------------------------------------------------------------------------

   procedure Set_Geometry_Vertices_Out_Type (theProgram : GL.Objects.Programs.Program;
                                             Output_Type : UInt) is
    begin
      GL.API.Ext.Program_Parameter (theProgram.Raw_Id, GL_GEOMETRY_VERTICES_OUT_EXT,
                                    GL.Types.Int (Output_Type));
      Raise_Exception_On_OpenGL_Error;
    end Set_Geometry_Vertices_Out_Type;

    --  ----------------------------------------------------------------------------

end GL.Ext;
