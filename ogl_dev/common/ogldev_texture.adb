
with Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
with GL.Pixels;

with Magick_Blob.API;
with Magick_Image;
with Magick_Image.API;

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
   end Init_Texture;

   --  -------------------------------------------------------------------------

   procedure Load (theTexture : in out Ogl_Texture) is
      use Ada.Strings.Unbounded;
      use GL.Low_Level;
      use GL.Objects.Textures.Targets;
   begin
      --  Type of Image_Ref is Core_Image.AI_Image;
      --  Type of Blob_Data  is Magick_Blob.Blob_Data;Index
      Magick_Image.Load_Blob (To_String (theTexture.File_Name), "RGBA");

--        Put_Line ("Ogldev_Texture.Load, Read_File. ");
--        Magick_Image.Read_File (theTexture.Image_Ref, To_String (theTexture.File_Name));
--        Put_Line ("Ogldev_Texture.Load, Write_Blob. ");
--        Magick_Image.Write_Blob (theTexture.Image_Ref, theTexture.Blob_Data, "RGBA");
--        Put_Line ("Ogldev_Texture.Load, Blob written. ");

      theTexture.Texture_Object.Initialize_Id;
      case theTexture.Texture_Target is
         when Enums.Texture_1D => Texture_1D.Bind (theTexture.Texture_Object);
         when Enums.Texture_2D => Texture_2D.Bind (theTexture.Texture_Object);
         when Enums.Texture_3D => Texture_3D.Bind (theTexture.Texture_Object);
         when others =>
            raise Texture_Exception with
              "Ogldev_Texture.Load, unhandled texture type.";
      end case;

      Put_Line ("Ogldev_Texture.Load, Columns, Rows, Depth, Colours: " &
                  Interfaces.C.size_t'Image (theTexture.Image_Ref.Columns) & "  " &
                  Interfaces.C.size_t'Image (theTexture.Image_Ref.Rows) & "  " &
                  Interfaces.C.size_t'Image (theTexture.Image_Ref.Depth) & "  " &
                  Interfaces.C.size_t'Image (theTexture.Image_Ref.Colours));
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
                Put_Line ("Ogldev_Texture.Load Index:" & UInt'Image (Index));
            Index := Index + 1;
            Data (Index) := Element (Curs);
            Next (Curs);
         end loop;

            -- load Texture_2D buffer with data from Data array.
         Put_Line ("Ogldev_Texture.Load Load_From_Data.");
            Texture_2D.Load_From_Data (Level, GL.Pixels.RGBA,
                                       Int (theTexture.Image_Ref.Columns),
                                       Int (theTexture.Image_Ref.Rows),
                                       GL.Pixels.RGBA, GL.Pixels.Unsigned_Byte,
                                       GL.Objects.Textures.Image_Source (Data'Address));
            Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
            Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
         Put_Line ("Ogldev_Texture.Load Data loaded.");
      end;  --  declare
   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Texture.Load.");
         raise;
   end Load;

   --  -------------------------------------------------------------------------

end Ogldev_Texture;
