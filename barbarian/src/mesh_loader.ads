
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with GL_Maths;

package Mesh_Loader is

   Max_Bones : constant  Int := 32;
   Mesh_Loader_Exception : Exception;

    procedure Init;
    function Load_Managed_Mesh (Mesh_Name : String; Has_Vp, Has_Vn, Has_Vt,
                                Has_tangents, Has_bones : Boolean := False)
                                return Integer;
    function Load_Mesh_Data_Only (File_Name : String;
                                  Points : in out GL_Maths.Vector3_List;
                                  Tex_Coords : in out GL_Maths.Vector2_List;
                                  Normals : in out GL_Maths.Vector3_List;
                                  Point_Count : in out Integer)
                                  return Boolean;
    function Loaded_Mesh_VAO (Index : Integer;
                              VAO : in out  GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                              return Boolean;
    function Point_Count (Index : Integer) return Integer;

end Mesh_Loader;
