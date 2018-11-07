
with Ada.Text_IO; use Ada.Text_IO;

with GL.Ext;
with GL.Types;

package body OglDev_Technique is

    function Finalize (theProgram : GL.Objects.Programs.Program) return boolean is
        use GL.Objects.Programs;
        Success : Boolean := False;
    begin
        Put_Line ("OglDev_Technique.Finalize entered");
        GL.Ext.Set_Geometry_Input_Type (theProgram, GL.Types.Points);
        Put_Line ("OglDev_Technique.Finalize, Geometry Input_Type set");
        GL.Ext.Set_Geometry_Output_Type (theProgram, GL.Types.Triangle_Strip);
        GL.Ext.Set_Geometry_Vertices_Out_Type (theProgram, 4);
        Put_Line ("OglDev_Technique.Finalize, Geometry Types set");

        Bind_Attrib_Location (theProgram, 0, "Position");
        Bind_Attrib_Location (theProgram, 1, "TexCoord");
        Bind_Attrib_Location (theProgram, 2, "Normal");

        Success := Link_Status (theProgram);
        if not Success then
            Put_Line ("OglDev_Technique.Finalize, Program Link failed");
            Put_Line (Info_Log (theProgram));
        else
            Put_Line ("OglDev_Technique.Finalize, Program Link ok");
            Validate (theProgram);
            Success := Link_Status (theProgram);
            if not Success then
                Put_Line ("OglDev_Technique.Finalize, Program validation failed");
                Put_Line (Info_Log (theProgram));
            else
                Put_Line ("OglDev_Technique.Finalize, Program validation ok");
            end if;
        end if;
        return Success;

   exception
      when  others =>
         Put_Line ("An exception occurred in OglDev_Technique.Finalize.");
         raise;

    end Finalize;

end OglDev_Technique;
