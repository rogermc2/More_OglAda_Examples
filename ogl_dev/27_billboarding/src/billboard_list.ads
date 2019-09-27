
with GL.Types;

Package Billboard_List is

    function Init (Tex_File_Name : String) return Boolean;
    procedure Render (View_Point_Matrix : GL.Types.Singles.Matrix4;
                      Camera_Position : GL.Types.Singles.Vector3);

end Billboard_List;
