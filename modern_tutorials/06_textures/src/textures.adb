
with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Objects.Textures.Targets;
with GL.Pixels;

package body Textures is

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                           Image_File_Name : String) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
   begin
      GL.Objects.Textures.Set_Active_Unit (0);
      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      Texture_2D.Set_Minifying_Filter (Nearest);
      Texture_2D.Set_Magnifying_Filter (Nearest);

      GL.Images.Load_File_To_Texture (
           Path           => Image_File_Name,
           Texture        => aTexture,
           Texture_Format => GL.Pixels.RGBA,
           Try_TGA        => True);

      Texture_2D.Generate_Mipmap;

   exception
      when others =>
         Put_Line ("An exception occurred in Textures.Load_Texture");
         raise;
   end Load_Texture;

end Textures;
