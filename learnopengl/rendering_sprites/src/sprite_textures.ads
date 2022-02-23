
with GL.Objects.Textures;

package Sprite_Textures is

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                           Image_File_Name : String);

   Image_Error : Exception;

end Sprite_Textures;
