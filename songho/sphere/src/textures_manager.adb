
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
with Load_BMP_File;

package body Textures_Manager is

    procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                            Image_File_Name : String; Wrap : Boolean) is
        use GL.Objects.Textures.Targets;
    begin
        Load_BMP_File.Load_BMP_To_Texture (Image_File_Name, Wrap, aTexture);
        GL.Objects.Textures.Set_Active_Unit (0);

        Texture_2D.Generate_Mipmap;

    exception
        when others =>
            Put_Line ("An exception occurred in TexturesManger.Load_Texture");
            raise;
    end Load_Texture;

end Textures_Manager;
