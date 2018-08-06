
with Ogldev_Util;

with Assimp.API;
with Importer;
with Scene;

package body Assimp_Mesh is

    function Init_From_Scene (theMesh : in out Mesh; theScene : Scene.AI_Scene;
                              File_Name : String) return Boolean;
    function Init_Materials (theMesh : in out AI_Mesh; theScene : Scene.AI_Scene;
                             File_Name : String) return Boolean;
    procedure Init_Mesh (theMesh : in out AI_Mesh; Mesh_Index : UInt);

    --  ------------------------------------------------------------------------

    function Init_From_Scene (theMesh : in out Mesh; theScene  : Scene.AI_Scene;
                              File_Name : String)return Boolean is
    begin
        return False;
    end Init_From_Scene;

    --  ------------------------------------------------------------------------

    function Init_Materials (theMesh : in out AI_Mesh; theScene : Scene.AI_Scene;
                             File_Name : String) return Boolean is
    begin
        return False;
    end Init_Materials;

    --  ------------------------------------------------------------------------

    procedure Init_Mesh (theMesh : in out AI_Mesh; Mesh_Index : UInt) is
    begin
        null;
--          Assimp.API.Init_Mesh (theMesh : in out API_Mesh; Mesh_Index : UInt)
--            (Material, Tex_Type, unsigned (Tex_Index), C_Path'Access);
    end Init_Mesh;

    --  ------------------------------------------------------------------------

    procedure Load_Mesh (File_Name : String; theMesh : in out Mesh) is
        theScene : Scene.AI_Scene;
        Ok       : Boolean := False;
    begin
         theScene := Importer.Read_File (File_Name, UInt (Ogldev_Util.Assimp_Load_Flags));
         Ok := Init_From_Scene (theMesh, theScene, File_Name);
    end Load_Mesh;

    --  ------------------------------------------------------------------------

    procedure Render_Mesh (theMesh : Mesh) is
    begin
        null;
    end Render_Mesh;

    --  ------------------------------------------------------------------------

end Assimp_Mesh;
