
with Load_Obj_File;

with Model;

package Load_Buffers is

    procedure Load_Buffers
      (aModel : in out Model.Model_Data; Vertices : Load_Obj_File.Obj_Array3;
       Vertex_Indices : Load_Obj_File.Obj_Int3_Array);

end Load_Buffers;
