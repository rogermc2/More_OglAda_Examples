
with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types;

package body Textures is

   procedure Load_Texture (aTexture        : in out GL.Objects.Textures.Texture;
                           Image_File_Name : String) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
   begin
      GL.Objects.Textures.Set_Active_Unit (0);
      --  Load_File_To_Texture initializes and binds the texture
      GL.Images.Load_File_To_Texture (Path           => Image_File_Name,
                                      Texture        => aTexture,
                                      Texture_Format => GL.Pixels.RGB,
                                      Try_TGA        => False);
      Texture_2D.Set_Minifying_Filter (Linear);

   exception
      when others =>
         Put_Line ("An exception occurred in Textures.Load_Texture");
         raise;
   end Load_Texture;

end Textures;
