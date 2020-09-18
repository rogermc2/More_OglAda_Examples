
with GL.Objects.Textures; use GL.Objects.Textures;

package Texture_Manager is

    Texture_Exception : Exception;

    function Bind_Texture (Slot : Natural; Tex : GL.Objects.Textures.Texture)
                           return Boolean;
    function Bind_Cube_Texture (Slot : Natural;
                                 Tex : GL.Objects.Textures.Texture)
                                 return Boolean;
    function Init_Texture_Manager return Boolean;
    function Load_Image_To_Texture (File_Name : String;
                                    aTexture : in out Texture;
                                    Gen_Mips, Use_SRGB : Boolean) return Boolean;
end Texture_Manager;
