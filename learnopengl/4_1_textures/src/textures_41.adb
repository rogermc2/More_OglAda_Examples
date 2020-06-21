
with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Pixels;

package body Textures_41 is

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                           Image_File_Name : String) is
   begin
      GL.Images.Load_File_To_Texture (
           Path           => Image_File_Name,
           Texture        => aTexture,
           Texture_Format => GL.Pixels.RGBA,
           Try_TGA        => True);

   exception
      when others =>
         Put_Line ("An exception occurred in Textures_41.Load_Texture");
         raise;
   end Load_Texture;

end Textures_41;
