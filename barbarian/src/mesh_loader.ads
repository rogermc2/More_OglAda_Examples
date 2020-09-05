
with GL.Objects.Vertex_Arrays;
with GL.Types;

package Mesh_Loader is

    Max_Bones : constant  GL.Types.Int := 32;

    procedure Init;
    function Load_Managed_Mesh (Mesh : String; Has_Vp, Has_Vn, Has_Vt,
                                Has_Vtangents, Has_bones : Boolean := False)
                                return Integer;
    function Loaded_Mesh_VAO (Index : Integer;
                              VAO : out  GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                              return Boolean;
    function Point_Count (Index : Integer) return Integer;

end Mesh_Loader;
