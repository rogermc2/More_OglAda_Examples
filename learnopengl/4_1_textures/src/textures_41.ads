
with GL.Objects.Textures;

package Textures_41 is

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                           Image_File_Name : String);

   Image_Error : Exception;

end Textures_41;
