
--  with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with Load_BMP_File;
with Utilities;

with Cube_Data;
with Shader_Manager_Texture;

package body Textures_Manager is

    Texture_VAO    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Cube_Buffer    : GL.Objects.Buffers.Buffer;
    Texture_Buffer : GL.Objects.Buffers.Buffer;

    procedure Load_Texture (File_Name : String;
                            aTexture : in out GL.Objects.Textures.Texture);

    --  -------------------------------------------------------------------------

    procedure Bind (aTexture : in out GL.Objects.Textures.Texture) is
        use GL.Objects.Textures.Targets;
        use GL.Objects.Textures;
    begin
        Texture_VAO.Bind;
        Texture_2D.Bind (aTexture);
        Set_Active_Unit (0);
    end Bind;

    --  ------------------------------------------------------------------------

    procedure Init (aTexture : in out GL.Objects.Textures.Texture) is
        use GL.Attributes;
        use GL.Objects.Buffers;
        use GL.Types;
    begin
        Texture_VAO.Initialize_Id;
        Texture_VAO.Bind;

        Shader_Manager_Texture.Use_Texture_Program;
        Shader_Manager_Texture.Set_Texture_Unit (0);

        Cube_Buffer.Initialize_Id;
        Array_Buffer.Bind (Cube_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Cube_Data.Vertex_Data,
                                      Static_Draw);
        Enable_Vertex_Attrib_Array (0);
        Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, 0, 0);

        Texture_Buffer.Initialize_Id;
        Array_Buffer.Bind (Texture_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Cube_Data.UV_Data,
                                      Static_Draw);
        Enable_Vertex_Attrib_Array (1);
        Set_Vertex_Attrib_Pointer (1, 2, Single_Type, False, 0, 0);

        Load_Texture ("src/resources/marble.bmp", aTexture);

    end Init;

    --  ------------------------------------------------------------------------

    procedure Load_Texture (File_Name : String;
                            aTexture : in out GL.Objects.Textures.Texture) is
        use GL.Objects.Textures;
        use GL.Objects.Textures.Targets;
    begin
        Set_Active_Unit (0);
        Load_BMP_File.Load_BMP_To_Texture (File_Name, False, aTexture);
        Texture_2D.Set_Minifying_Filter (Linear);
        Texture_2D.Set_Magnifying_Filter (Linear);

    end Load_Texture;

    --  ------------------------------------------------------------------------

end Textures_Manager;
