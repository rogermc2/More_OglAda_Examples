
with Ada.Strings.Unbounded;
with GL.Objects.Textures;

package Textures_Manager is

    type Texture_List is array (Natural range <>) of GL.Objects.Textures.Texture;
    type Image_Sources is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   procedure Load_Textures (Textures : in out Texture_List;
                            Images   : Image_Sources);

   Image_Error : Exception;

end Textures_Manager;
