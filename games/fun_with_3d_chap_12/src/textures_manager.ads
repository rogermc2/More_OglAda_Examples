
with GL.Objects.Textures;

package Textures_Manager is

    procedure Bind (aTexture : in out GL.Objects.Textures.Texture);
    procedure Init (aTexture : in out GL.Objects.Textures.Texture);

    Image_Error : exception;

end Textures_Manager;
