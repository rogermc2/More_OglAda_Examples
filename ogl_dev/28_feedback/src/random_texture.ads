
with GL.Objects.Textures;
with GL.Types;

package Random_Texture is

   procedure Init_Random_Texture (aTexture : in out GL.Objects.Textures.Texture;
                                  Texture_Length : GL.Types.UInt);
   procedure  Bind (aTexture : in out GL.Objects.Textures.Texture;
                    Texture_Unit : GL.Objects.Textures.Texture_Unit);

end Random_Texture;
