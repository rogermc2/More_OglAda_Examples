
package GL.API.Ext is

    procedure Program_Parameter (Program: UInt; Name : Int; Value : Int);
      pragma Import (C, Program_Parameter, "glProgramParameteriEXT");

end GL.API.Ext;
