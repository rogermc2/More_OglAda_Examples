
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with GL.Objects.Textures;

package Textures_Manager is

   type Texture_List is private;
   type Image_Sources is array (Positive range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   procedure Add_Texture_To_List (theTextures : in out Texture_List;
                                  aTexture    : GL.Objects.Textures.Texture);
   function Get_Last_Index (theTextures : Texture_List) return Positive;
   function Get_Texture (theTextures : Texture_List; Index : Positive) return
     GL.Objects.Textures.Texture;

   Image_Error : Exception;

private
   use GL.Objects.Textures;
   package Textures_Package is new Ada.Containers.Vectors
     (Positive, GL.Objects.Textures.Texture);
   type Texture_List is new Textures_Package.Vector with null Record;

end Textures_Manager;
