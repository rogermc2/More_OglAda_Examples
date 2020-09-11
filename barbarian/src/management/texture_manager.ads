
with GL.Objects.Textures; use GL.Objects.Textures;

package Texture_Manager is

    Texture_Exception : Exception;

    procedure Bind_Texture (Slot : Natural;
                            Tex : GL.Objects.Textures.Texture);
    procedure Bind_Cube_Texture (Slot : Natural;
                            Tex : GL.Objects.Textures.Texture);
    procedure Init_Texture_Manager;
    procedure Load_Image_To_Texture (File_Name : String;
                                     aTexture : out Texture;
                                     Gen_Mips, Use_SRGB : Boolean);
end Texture_Manager;
