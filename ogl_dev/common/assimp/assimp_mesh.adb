
with Ogldev_Util;

with Assimp.API;
with Importer;
with Scene;

package body Assimp_Mesh is

    function Init_From_Scene (theMesh : in out Mesh; theScene : Scene.AI_Scene;
                              File_Name : String) return Boolean;
    function Init_Materials (theMesh : in out Mesh; theScene : Scene.AI_Scene;
                             File_Name : String) return Boolean;
    procedure Init_Mesh (theMesh : in out Mesh; Mesh_Index : UInt; anAI_Mesh : AI_Mesh);
    function To_API_Mesh (anAI_Mesh : AI_Mesh) return API_Mesh;

    --  ------------------------------------------------------------------------

    function Init_From_Scene (theMesh : in out Mesh; theScene  : Scene.AI_Scene;
                              File_Name : String) return Boolean is
        use AI_Mesh_Package;
        anAI_Mesh : AI_Mesh;
        Index     : UInt := 0;
    begin
        for iterator  in theScene.Meshes.Iterate loop
            anAI_Mesh := Element (iterator);
            Index := Index + 1;
            Init_Mesh (theMesh, Index, anAI_Mesh);
        end loop;

        return Init_Materials (theMesh, theScene, File_Name);
    end Init_From_Scene;

    --  ------------------------------------------------------------------------

    function Init_Materials (theMesh : in out Mesh; theScene : Scene.AI_Scene;
                             File_Name : String) return Boolean is
    begin
        return False;
    end Init_Materials;

    --  ------------------------------------------------------------------------

    procedure Init_Mesh (theMesh : in out Mesh; Mesh_Index : UInt; anAI_Mesh : AI_Mesh) is
        pai_Mesh : API_Mesh := To_API_Mesh (anAI_Mesh);
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

    function To_API_Mesh (anAI_Mesh : AI_Mesh) return API_Mesh is
        use Interfaces;
        C_Mesh : API_Mesh;
    begin
        C_Mesh.Num_Vertices := C.unsigned (Length (anAI_Mesh.Vertices));
        return C_Mesh;
    end To_API_Mesh;

    --  ------------------------------------------------------------------------

end Assimp_Mesh;
