
with GL.Objects.Textures;

package Textures_Manager is

    procedure Load_Texture (File_Name : String;
                            aTexture : in out GL.Objects.Textures.Texture);

    Image_Error : Exception;

end Textures_Manager;
