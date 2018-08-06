
with Assimp.API;
with Scene;

package body Assimp_Mesh is

    function Init_From_Scene (theMesh : in out AI_Mesh; theScene : Scene.AI_Scene;
                              File_Name : Ada.Strings.Unbounded.Unbounded_String)
                             return Boolean;
    function Init_Materials (theMesh : in out AI_Mesh; theScene : Scene.AI_Scene;
                             File_Name : Ada.Strings.Unbounded.Unbounded_String)
                            return Boolean;
    procedure Init_Mesh (theMesh : in out AI_Mesh; Mesh_Index : UInt);

    --  ------------------------------------------------------------------------

    function Init_From_Scene (theMesh : in out AI_Mesh; theScene  : Scene.AI_Scene;
                              File_Name : Ada.Strings.Unbounded.Unbounded_String)
                             return Boolean is
    begin
        return False;
    end Init_From_Scene;

    --  ------------------------------------------------------------------------

    function Init_Materials (theMesh : in out AI_Mesh; theScene : Scene.AI_Scene;
                             File_Name : Ada.Strings.Unbounded.Unbounded_String)
                             return Boolean is
    begin
        return False;
    end Init_Materials;

    --  ------------------------------------------------------------------------

    procedure Init_Mesh (theMesh : in out AI_Mesh; Mesh_Index : UInt) is
    begin
        Assimp.API.Init_Mesh (theMesh : in out API_Mesh; Mesh_Index : UInt)
          (Material, Tex_Type, unsigned (Tex_Index), C_Path'Access);
    end Init_Mesh;

    --  ------------------------------------------------------------------------

    procedure Load_Mesh (File_Name : Ada.Strings.Unbounded.Unbounded_String;
                         theMesh   : in out AI_Mesh) is
    begin
        null;
    end Load_Mesh;

    --  ------------------------------------------------------------------------

    procedure Render_Mesh (theMesh : AI_Mesh) is
    begin
        null;
    end Render_Mesh;

    --  ------------------------------------------------------------------------

end Assimp_Mesh;
