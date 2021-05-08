
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Pixels;

package body Textures_Manager is

   --  -------------------------------------------------------------------------

   procedure Load_Textures (Textures : in out Texture_List;
                             Images : Image_Sources) is
      use Ada.Strings.Unbounded;
      Image_Index : Positive := 1;
   begin
      for Index in Textures'Range loop
         GL.Images.Load_File_To_Texture (
           Path           => To_String (Images (Image_Index)),
           Texture        => Textures (Index),
           Texture_Format => GL.Pixels.RGBA,
           Try_TGA        => True);
         Put_Line ("Setup_Textures; image " & To_String (Images (Image_Index)) &
           " loaded.");
         Image_Index := Image_Index + 1;
      end loop;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Setup_Textures.");
         Put_Line (Exception_Information (anError));
         raise;
   end Load_Textures;

end Textures_Manager;
