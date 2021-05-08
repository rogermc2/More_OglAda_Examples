
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Pixels;

package body Textures_Manager is

   --  -------------------------------------------------------------------------

   procedure Add_Texture_To_List (theTextures : in out Texture_List;
                                  aTexture    : Texture) is
   begin
      theTextures.Append (aTexture);
   end Add_Texture_To_List;

   --  -------------------------------------------------------------------------

   function Get_Last_Index (theTextures : Texture_List) return Integer is
   begin
      return theTextures.Last_Index;
   end Get_Last_Index;

   --  -------------------------------------------------------------------------

   function Get_Texture (theTextures : Texture_List; Index : Natural) return
     GL.Objects.Textures.Texture is
   begin
      return theTextures.Element (Index);
   end Get_Texture;

   --  -------------------------------------------------------------------------

   procedure Load_Textures (Textures : in out Texture_List;
                            Images   : Image_Sources) is
      use Ada.Strings.Unbounded;
      aTexture : Texture;
   begin
      for Index in Images'Range loop
         GL.Images.Load_File_To_Texture (
                                         Path           => To_String (Images (Index)),
                                         Texture        => aTexture,
                                         Texture_Format => GL.Pixels.RGBA,
                                         Try_TGA        => True);
         Put_Line ("Setup_Textures; image " & To_String (Images (Index)) &
                     " loaded.");
      end loop;
      Textures.Append (aTexture);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Setup_Textures.");
         Put_Line (Exception_Information (anError));
         raise;
   end Load_Textures;

   --  -------------------------------------------------------------------------

end Textures_Manager;
