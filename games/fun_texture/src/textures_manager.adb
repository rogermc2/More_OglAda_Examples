
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
--  with GL.Pixels;
--  with GL.Types; use GL.Types;

--  with SOIL;
--  with SOIL.Images;
with Load_BMP_File;

package body Textures_Manager is

    procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                            Image_File_Name : String; Wrap : Boolean) is
        use GL.Objects.Textures.Targets;
--          use SOIL;
--          Image          : SOIL.Images.Image;
--          Soil_Flages    : constant Texture_Flags := (others => False);
--          Width          : Int;
--          Height         : Int;
--          Image_Format   : Explicit_Image_Format;
--          Pixel_Format   : GL.Pixels.Internal_Format;
--          Data_Format    : GL.Pixels.Data_Format := GL.Pixels.RGB;
    begin
        Load_BMP_File.Load_BMP_To_Texture (Image_File_Name, Wrap, aTexture);
--          Image.Load (Image_File_Name);
--          aTexture.Initialize_Id;
--          Texture_2D.Bind (aTexture);
--          Put_Line ("TexturesManger.Load_Texture loading aTexture.");
--  --          Load_File_To_Texture  (Image_File_Name, aTexture);
--          Put_Line ("TexturesManger.Load_Texture aTexture loaded ");
--          Width := SOIL.Images.Width (Image);
--          Height := SOIL.Images.Height (Image);
--          if not SOIL.Images.Loaded (Image) then
--              raise Image_Error with
--                "Load_Image; " & Image_File_Name & " failed to load.";
--          end if;
--          Put_Line ("TexturesManger.Load_Texture Width, Height" &
--                     Int'Image (Width) & "  " & Int'Image (Height));
--
--          Image_Format := SOIL.Images.Channels (Image);
--          Put_Line ("TexturesManger.Load_Texture Image_Format  " &
--                     Explicit_Image_Format'Image (Image_Format));
--          if Image_Format = L then
--              Pixel_Format := GL.Pixels.Luminance;
--          elsif Image_Format = RGB then
--              Pixel_Format := GL.Pixels.RGB;
--          elsif Image_Format = RGBA then
--              Pixel_Format := GL.Pixels.RGBA;
--              Data_Format := GL.Pixels.RGBA;
--          else
--              raise Image_Error with
--                "TexturesManger Load_Image; " & Image_File_Name & "format not supported.";
--          end if;
--
--          aTexture.Initialize_Id;
--          Texture_2D.Bind (aTexture);
        --  All upcoming GL_TEXTURE_2D operations affect this texture object
--          Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear_Mipmap_Linear);
--          Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
        --  If wrap is true the texture wraps over at the edges (repeat).
        --  If false the texture ends at the edges (clamp).
--          if Wrap then
--              Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Repeat); --  Wrap_S
--              Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Repeat); --  Wrap_T
--          else
--              Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp);
--              Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp);
--          end if;
--
--          GL.Objects.Textures.Targets.Texture_2D.Generate_Mipmap;
        GL.Objects.Textures.Set_Active_Unit (0);

--          Texture_2D.Load_From_Data  (0, Pixel_Format, Width, Height,
--                                      Data_Format, GL.Pixels.Unsigned_Byte,
--                                      Image.Data);

--          Texture_2D.Set_Lowest_Mipmap_Level (0);
--          if Image_Format = L then
--              Texture_2D.Set_Highest_Mipmap_Level (1);
--          elsif Image_Format = RGB then
--              Texture_2D.Set_Highest_Mipmap_Level (3);
--          elsif Image_Format = RGBA then
--              Texture_2D.Set_Highest_Mipmap_Level (4);
--          end if;
        Texture_2D.Generate_Mipmap;

    exception
        when others =>
            Put_Line ("An exception occurred in TexturesManger.Load_Texture");
            raise;
    end Load_Texture;

end Textures_Manager;
