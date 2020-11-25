
with System;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types;

with Res_Texture;

package body Textures is
   type Bitmap_Record is record
      Width   : GL.Types.Int;
      Height  : GL.Types.Int;
      Pitch   : GL.Types.Int;
      Buffer  : System.Address;
   end record;

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                           Image_File_Name : String) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      Bitmap            : Bitmap_Record;
      Bitmap_Image_Ptr  : GL.Objects.Textures.Image_Source;
   begin
      Bitmap.Width := Res_Texture.Resource_Texture.Width;
      Bitmap.Height := Res_Texture.Resource_Texture.Height;
      Bitmap.Pitch := Res_Texture.Resource_Texture.Bytes_Per_Pixel;


      GL.Objects.Textures.Set_Active_Unit (0);
      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      Texture_2D.Set_Minifying_Filter (Linear);

      Bitmap_Image_Ptr := GL.Objects.Textures.Image_Source (Bitmap.Buffer);
      Texture_2D.Load_From_Data (Level           => 0,
                                 Internal_Format => GL.Pixels.RGB,
                                 Width           => Res_Texture.Resource_Texture.Width,
                                 Height          => Res_Texture.Resource_Texture.Height,
                                 Source_Format   => GL.Pixels.RGB,
                                 Source_Type     => GL.Pixels.Unsigned_Byte,
                                 Source          => Image_Source (Res_Texture.Resource_Texture.Pixels));

   exception
      when others =>
         Put_Line ("An exception occurred in Textures.Load_Texture");
         raise;
   end Load_Texture;

end Textures;
