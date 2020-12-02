
with GL.Objects.Textures;
with GL.Types;

package Textures is

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                          Width : out GL.Types.Int);

   Image_Error : Exception;

end Textures;
