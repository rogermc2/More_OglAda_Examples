
with GL.Ext;
with GL.Types;

package body OglDev_Technique is

    function Finalize (theProgram : GL.Objects.Programs.Program) return boolean is
    begin
        GL.Ext.Set_Geometry_Input_Type (theProgram, GL.Types.Points);
        GL.Ext.Set_Geometry_Output_Type (theProgram, GL.Types.Triangle_Strip);
        GL.Ext.Set_Geometry_Vertices_Out_Type (theProgram, 4);
        return False;
    end Finalize;

end OglDev_Technique;
