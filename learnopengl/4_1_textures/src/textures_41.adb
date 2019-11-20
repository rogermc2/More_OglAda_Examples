
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types; use GL.Types;

with SOIL;
with SOIL.Images;

package body Textures_41 is

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                           Image_File_Name : String) is
      use GL.Objects.Textures.Targets;
      Image    : SOIL.Images.Image;
      Width    :  Int;
      Height   :  Int;
   begin

      Image.Load (Image_File_Name);
      Width := SOIL.Images.Width (Image);
      Height := SOIL.Images.Height (Image);
      if not SOIL.Images.Loaded (Image) then
         raise Image_Error with
           "Load_Image; " & Image_File_Name & " failed to load.";
      end if;

      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      --  All upcoming GL_TEXTURE_2D operations now have effect on
      --  this texture object
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Repeat); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Repeat); --  Wrap_T

      Texture_2D.Load_From_Data  (0, GL.Pixels.RGB, Width, Height,
                                  GL.Pixels.RGB, GL.Pixels.Unsigned_Byte,
                                  Image.Data);
      Texture_2D.Generate_Mipmap;

   exception
      when others =>
         Put_Line ("An exception occurred in Textures_41.Load_Texture");
         raise;
   end Load_Texture;

end Textures_41;
