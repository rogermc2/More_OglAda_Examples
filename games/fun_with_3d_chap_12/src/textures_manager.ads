
with GL.Objects.Textures;

package Textures_Manager is

    procedure Init (aTexture : in out GL.Objects.Textures.Texture);

    Image_Error : exception;

end Textures_Manager;
