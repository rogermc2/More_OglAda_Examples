
with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Objects.Textures.Targets;
with GL.Pixels;

with Res_Texture;

package body Textures is

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                           Image_File_Name : String) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
   begin
      GL.Objects.Textures.Set_Active_Unit (0);
      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      Texture_2D.Set_Minifying_Filter (Linear);

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
