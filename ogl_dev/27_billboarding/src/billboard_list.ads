
with GL.Types;

Package Billboard_List is

   function Init (Tex_File_Name : String) return Boolean;
   procedure Render (VP : GL.Types.Singles.Matrix4);

end Billboard_List;
