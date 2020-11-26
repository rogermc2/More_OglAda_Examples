
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types;

with MGL_Common;

package body Textures is

   type Image_Data is array (GL.Types.Int range <>) of GL.Types.UByte;

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      Texture_Data : MGL_Common.SDL_Data;
   begin
      GL.Objects.Textures.Set_Active_Unit (0);
      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);

      MGL_Common.Read_SDL_File ("src/res_texture.tex", Texture_Data);
      Put_Line ("Textures.Load_Texture file read.");
      declare
         Image : Image_Data
           (1 .. Texture_Data.Width * Texture_Data.Height * Texture_Data.Pitch);
      begin
         for index in Texture_Data.Data.First_Index ..
           Texture_Data.Data.Last_Index loop
            declare
               aLine : String := To_String (Texture_Data.Data.Element (index));
               aChar : Character;
            begin
               for char_pos in aLine'Range loop
                  aChar := aLine (char_pos);
                  Put (aChar);
                  Image (Int (index + char_pos - 1)) :=
                    UByte (Character'Pos (aLine (char_pos)));
               end loop;
               New_Line;
            end;
         end loop;

         --        GL.Images.Load_File_To_Texture (
         --             Path           => "src/res_texture.tex",
         --             Texture        => aTexture,
         --             Texture_Format => GL.Pixels.RGB,
         --             Try_TGA        => False);
         Texture_2D.Load_From_Data (Level           => 0,
                                    Internal_Format => GL.Pixels.RGBA,
                                    Width           => Texture_Data.Width,
                                    Height          => Texture_Data.Height,
                                    Source_Format   => GL.Pixels.RGBA,
                                    Source_Type     => GL.Pixels.Unsigned_Byte,
                                    Source          => Image_Source (Image'Address));
      end;  --  declare block

      Texture_2D.Set_Minifying_Filter (Nearest);
      Texture_2D.Set_Magnifying_Filter (Nearest);

   exception
      when others =>
         Put_Line ("An exception occurred in Textures.Load_Texture");
         raise;
   end Load_Texture;

end Textures;
