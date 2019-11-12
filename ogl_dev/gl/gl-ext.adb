
with Interfaces.C; use Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with GL.API.Ext;

package body GL.Ext is

    procedure Set_Geometry_Input_Type (theProgram : GL.Objects.Programs.Program;
                                       Input_Type : Connection_Mode) is
    begin
      Put_Line ("GL.Ext.Set_Geometry_Input_Type theProgram.Raw_Id, GL_GEOMETRY_INPUT_TYPE_EXT: " &
                  GL.Types.UInt'Image (theProgram.Raw_Id) & "  " &
                  GL.API.Ext.Geometry_Type'Image (GL.API.Ext.GL_GEOMETRY_INPUT_TYPE_EXT));
      GL.API.Ext.Program_Parameter
        (theProgram.Raw_Id,
         GL.API.Ext.GL_GEOMETRY_INPUT_TYPE_EXT,
         Input_Type);
      Raise_Exception_On_OpenGL_Error;

    end Set_Geometry_Input_Type;

    --  ----------------------------------------------------------------------------

    procedure Set_Geometry_Output_Type (theProgram : GL.Objects.Programs.Program;
                                       Output_Type : Connection_Mode) is
    begin
      GL.API.Ext.Program_Parameter
        (theProgram.Raw_Id,
         GL.API.Ext.GL_GEOMETRY_OUTPUT_TYPE_EXT, Output_Type);
      Raise_Exception_On_OpenGL_Error;

    end Set_Geometry_Output_Type;

    --  ----------------------------------------------------------------------------

   procedure Set_Geometry_Vertices_Out_Type (theProgram : GL.Objects.Programs.Program;
                                             Output_Type : GL.Types.Int) is
    begin
      GL.API.Ext.Program_Parameter
        (theProgram.Raw_Id,
         GL.API.Ext.GL_GEOMETRY_VERTICES_OUT_EXT, Connection_Mode'Enum_Val (Output_Type));
      Raise_Exception_On_OpenGL_Error;

    end Set_Geometry_Vertices_Out_Type;

    --  ----------------------------------------------------------------------------

end GL.Ext;
