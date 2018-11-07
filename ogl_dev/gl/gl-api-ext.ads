
package GL.API.Ext is

    procedure Program_Parameter (Program, Name : UInt; Value : Int);
      pragma Import (C, Program_Parameter, "glProgramParameteriEXT");

end GL.API.Ext;
