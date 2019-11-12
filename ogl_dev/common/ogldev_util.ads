
with Interfaces; use Interfaces;
with Interfaces.C;

with GL.Objects.Buffers;
with GL.Types;

with Post_Process; use Post_Process;

with Maths;

with Ogldev_Math;

package Ogldev_Util is
    use Interfaces.C;

    Assimp_Basic_Load_Flags : constant unsigned :=
                                AI_Process_Triangulate'Enum_Rep + AI_Process_Gen_Smooth_Normals'Enum_Rep +
                                  AI_Process_Flip_UVs'Enum_Rep + AI_Process_Join_Identical_Vertices'Enum_Rep;
    pragma Convention (C, Assimp_Basic_Load_Flags);

    Invalid_Uniform_Location : constant C.unsigned := 16#FFFFFFFF#;
    Invalid_OGL_Value        : constant C.unsigned := 16#FFFFFFFF#;

    procedure Load_Vector11_Buffer is new GL.Objects.Buffers.Load_To_Buffer
      (Ogldev_Math.Vector11_Pointers);

    procedure Print_GL_Array8 (Name : String; anArray : Maths.Vector8_Array;
                               Start, Finish : GL.Types.Int);
    procedure Print_GL_Array11 (Name : String; anArray : Ogldev_Math.Vector11_Array);

end Ogldev_Util;
