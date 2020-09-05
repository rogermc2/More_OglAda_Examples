
with GL.Objects.Textures; use GL.Objects.Textures;

package Texture_Manager is

    procedure Init_Texture_Manager;
    procedure Load_Image_To_Texture (File_Name : String;
                                     aTexture : out Texture;
                                     Gen_Mips, Use_SRGB : Boolean);

end Texture_Manager;
