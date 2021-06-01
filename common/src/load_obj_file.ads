
with Ada.Containers.Vectors;
with GL.Types;

package Load_Obj_File is

    use GL.Types.Ints;
    use GL.Types.Singles;
    package Obj_Index_Package is new Ada.Containers.Vectors
      (Positive, GL.Types.Ints.Vector3);
    subtype Obj_Int3_Array is Obj_Index_Package.Vector;

    package Obj_Array2_Package is new Ada.Containers.Vectors
      (Positive, GL.Types.Singles.Vector2);
    subtype Obj_Array2 is Obj_Array2_Package.Vector;

    package Obj_Array3_Package is new Ada.Containers.Vectors
      (Positive, GL.Types.Singles.Vector3);
    subtype Obj_Array3 is Obj_Array3_Package.Vector;

    procedure Load_Object
      (File_Name : String; Vertices, Normals : out Obj_Array3;
       UVs : out Obj_Array2;
       Vertex_Indices, Normal_Indices, UV_Indices : out Obj_Int3_Array);

end Load_Obj_File;
