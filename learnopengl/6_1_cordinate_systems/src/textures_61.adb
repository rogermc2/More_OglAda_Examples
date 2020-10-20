
with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Pixels;
with GL.Objects.Textures.Targets;

package body Textures_61 is

   procedure Load_Texture
     (aTexture : in out GL.Objects.Textures.Texture;
      Image_File_Name : String) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
   begin
      GL.Images.Load_File_To_Texture (
           Path           => Image_File_Name,
           Texture        => aTexture,
           Texture_Format => GL.Pixels.RGBA,
           Try_TGA        => True);

      Texture_2D.Bind (aTexture);

      Texture_2D.Set_X_Wrapping (Repeat);
      Texture_2D.Set_Y_Wrapping (Repeat);
      Texture_2D.Set_Minifying_Filter (Linear);
      Texture_2D.Set_Magnifying_Filter (Linear);
      Texture_2D.Generate_Mipmap;

   exception
      when others =>
         Put_Line ("An exception occurred in Textures_61.Load_Texture");
         raise;
   end Load_Texture;

end Textures_61;
