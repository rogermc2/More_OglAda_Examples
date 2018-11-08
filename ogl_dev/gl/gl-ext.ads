
with GL.Objects.Programs;
with GL.Types; use  GL.Types;

package GL.Ext is

   procedure Set_Geometry_Input_Type (theProgram : GL.Objects.Programs.Program;
                                      Input_Type : Connection_Mode);
   procedure Set_Geometry_Output_Type (theProgram  : GL.Objects.Programs.Program;
                                       Output_Type : Connection_Mode);
   procedure Set_Geometry_Vertices_Out_Type (theProgram  : GL.Objects.Programs.Program;
                                             Output_Type : GL.Types.Int);
end GL.Ext;
