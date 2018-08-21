
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
with GL.Pixels;

with Magick_Image;

package body Ogldev_Texture is

   procedure Bind (theTexture : Ogl_Texture;
                   Texture_Unit : Ogldev_Engine_Common.Texture_Unit_Index) is
   use GL.Low_Level;
   use GL.Objects.Textures.Targets;
   begin
      GL.Objects.Textures.Set_Active_Unit (Texture_Unit'Enum_Rep);
      case theTexture.Texture_Target is
         when Enums.Texture_1D => Texture_1D.Bind (theTexture.Texture_Object);
         when Enums.Texture_2D => Texture_2D.Bind (theTexture.Texture_Object);
         when Enums.Texture_3D => Texture_3D.Bind (theTexture.Texture_Object);
         when others =>
            raise Texture_Exception with
              "Ogldev_Texture.Bind, unhandles texture type.";
      end case;
   end Bind;

   --  -------------------------------------------------------------------------

   procedure Init_Texture
     (theTexture : in out Ogl_Texture;
      Target_Type : GL.Low_Level.Enums.Texture_Kind;
      File_Name  :  String) is
      use Ada.Strings.Unbounded;
   begin
      theTexture.File_Name := To_Unbounded_String (File_Name);
      theTexture.Texture_Target := Target_Type;
   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Texture.Init_Texture.");
         raise;
   end Init_Texture;

   --  -------------------------------------------------------------------------

   procedure Load (theTexture : in out Ogl_Texture) is
      use Ada.Strings.Unbounded;
      use GL.Low_Level;
      use GL.Objects.Textures.Targets;
   begin
      Magick_Image.Load_Blob (To_String (theTexture.File_Name), "RGBA");
      theTexture.Blob_Data := Magick_Image.Get_Blob_Data;  --  Blob_Package.List

      theTexture.Image := Magick_Image.Get_Image;

      theTexture.Texture_Object.Initialize_Id;
      case theTexture.Texture_Target is
         when Enums.Texture_1D => Texture_1D.Bind (theTexture.Texture_Object);
         when Enums.Texture_2D => Texture_2D.Bind (theTexture.Texture_Object);
         when Enums.Texture_3D => Texture_3D.Bind (theTexture.Texture_Object);
         when others =>
            raise Texture_Exception with
              "Ogldev_Texture.Load, unhandled texture type.";
      end case;

--        Put_Line ("Ogldev_Texture.Load, Columns, Rows, Depth, Colours: " &
--                    UInt'Image (theTexture.Image.Columns) & "  " &
--                    UInt'Image (theTexture.Image.Rows) & "  " &
--                    UInt'Image (theTexture.Image.Depth) & "  " &
--                    UInt'Image (theTexture.Image.Colours));
      declare
         use Magick_Blob.Blob_Package;
         Data_Blob   : constant Magick_Blob.Blob_Data := theTexture.Blob_Data;
         Blob_Length : constant UInt := UInt (Data_Blob.Length);
         Data        : array (1 .. Blob_Length) of UByte;
         Index       : UInt := 0;
         Curs        : Cursor := Data_Blob.First;
         Level       : constant GL.Objects.Textures.Mipmap_Level := 0;
      begin
         while Has_Element (Curs) loop
            Index := Index + 1;
            Data (Index) := Element (Curs);
            Next (Curs);
         end loop;

            -- load Texture_2D buffer with data from Data array.
            Texture_2D.Load_From_Data (Level, GL.Pixels.RGBA,
                                       Int (theTexture.Image.Columns),
                                       Int (theTexture.Image.Rows),
                                       GL.Pixels.RGBA, GL.Pixels.Unsigned_Byte,
                                       GL.Objects.Textures.Image_Source (Data'Address));
            Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
            Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      end;  --  declare
   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Texture.Load.");
         raise;
   end Load;

   --  -------------------------------------------------------------------------

end Ogldev_Texture;
