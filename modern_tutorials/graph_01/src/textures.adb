
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Images;
with GL.Objects.Textures.Targets;
with GL.Pixels;

with Utilities;

with MGL_Common;

package body Textures is

   type Image_Data is array (GL.Types.Int range <>) of GL.Types.UByte;

   --  -------------------------------------------------------------------------

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                           Width    : out GL.Types.Int) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      Texture_Data : MGL_Common.SDL_Data;
   begin
      GL.Objects.Textures.Set_Active_Unit (0);
      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);

      MGL_Common.Read_SDL_File ("src/res_texture.tex", Texture_Data);

      Width := Texture_Data.Width;
      declare
         use Ada.Strings;
         Char_Index  : Int := 0;
         Image       : Image_Data
           (1 .. Texture_Data.Width * Texture_Data.Height * Texture_Data.Pitch)
           := (others => 0);
         Image_Index : Int := 0;
      begin
         for index in Texture_Data.Data.First_Index ..
           Texture_Data.Data.Last_Index loop
            declare
               aLine : constant String := To_String (Texture_Data.Data.Element (index));
               Last  : constant Integer := aLine'Length;
               Pos1  : Integer := Fixed.Index (aLine, "\");
               Pos2  : Integer := Pos1;
               Pos3  : Integer := Pos1;
               DQ    : Boolean := False;
            begin
               while Pos1 > 0 and Pos2 > 0 loop
                  if Pos2 = 0 then
                     DQ := False;
                     Pos1 := Fixed.Index (aLine, "\");
                  end if;
                  Pos2 := Fixed.Index (aLine (Pos1 + 1 .. Last), "\");
                  Pos3 := Fixed.Index (aLine (Pos1 + 1 .. Last - 1), """");
                  DQ := Pos3 /= 0 and Pos3 < Pos2;
                  if DQ then
                     Pos2 := Pos3;
                  end if;
                  if Pos2 /= 0 then
                     Image_Index := Image_Index + 1;
                     Image (Image_Index) :=
                       MGL_Common.To_UByte (aLine (Pos1 + 1 .. Pos2 - 1));
                     Pos1 := Pos2;
                  else   --  Pos2 = 0
                     Image_Index := Image_Index + 1;
                     Image (Image_Index) :=
                       MGL_Common. To_UByte (aLine (Pos1 + 1 .. Last));
                  end if;
                  if DQ then
                     Pos1 := Pos3 + 1;
                  end if;
               end loop;
            end;
         end loop;

         Texture_2D.Load_From_Data (Level           => 0,
                                    Internal_Format => GL.Pixels.RGBA,
                                    Width           => Texture_Data.Width,
                                    Height          => Texture_Data.Height,
                                    Source_Format   => GL.Pixels.RGBA,
                                    Source_Type     => GL.Pixels.Unsigned_Byte,
                                    Source          => Image_Source (Texture_Data.Data'Address));
         Texture_2D.Set_Minifying_Filter (Nearest);
         Texture_2D.Set_Magnifying_Filter (Nearest);
      end;  --  declare block

   exception
      when others =>
         Put_Line ("An exception occurred in Textures.Load_Texture");
         raise;
   end Load_Texture;

   --  -------------------------------------------------------------------------

end Textures;
