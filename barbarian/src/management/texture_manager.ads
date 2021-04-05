
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Textures; use GL.Objects.Textures;

package Texture_Manager is

   type Loaded_Texture is private;

   Texture_Exception : Exception;

   procedure Bind_Texture (Slot : Natural; Tex : GL.Objects.Textures.Texture);
   procedure Bind_Cube_Texture (Slot : Natural;
                                Tex  : GL.Objects.Textures.Texture);
   function Get_Default_Texture return Texture;
   function Get_Loaded_Texture (Index : Positive) return Loaded_Texture;
   procedure Init;
   procedure Load_Image_To_Texture
     (File_Name : String; aTexture : in out Texture; Gen_Mips, Use_SRGB : Boolean);
   procedure Refilter_Textures;

private

    type Loaded_Texture is record
        File_Name   : Unbounded_String;
        --          Texture_ID  : GL.Types.UInt;
        theTexture  : GL.Objects.Textures.Texture;
        Has_Mipmaps : Boolean := False;
    end record;

end Texture_Manager;
