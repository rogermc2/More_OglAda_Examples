
with Interfaces.C;

with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
with GL.Pixels;

with Magick_Image;

package body Ogldev_Texture is

   procedure Bind (theTexture      : Ogl_Texture;
                   theTexture_Unit : GL.Objects.Textures.Texture_Unit) is
      use GL.Low_Level;
      use GL.Objects.Textures.Targets;
   begin
      if theTexture.Texture_Object.Initialized then
         GL.Objects.Textures.Set_Active_Unit (theTexture_Unit);
         case theTexture.Texture_Target is
         when Enums.Texture_1D =>
            Texture_1D.Bind (theTexture.Texture_Object);
         when Enums.Texture_2D =>
            Texture_2D.Bind (theTexture.Texture_Object);
         when Enums.Texture_3D =>
            Texture_3D.Bind (theTexture.Texture_Object);
         when others =>
            raise Texture_Exception with
              "Ogldev_Texture.Bind, unhandled texture type.";
         end case;
      else
         raise Texture_Exception with
           "Ogldev_Texture.Bind, the Texture_Object is not initialized.";
      end if;
   end Bind;

   --  -------------------------------------------------------------------------

   function Init_Texture
     (theTexture   : in out Ogl_Texture;
      Target_Type  : GL.Low_Level.Enums.Texture_Kind;
      Texture_File :  String) return Boolean is
      use Ada.Strings.Unbounded;
      Result : Boolean;
   begin
--        Put_Line ("Ogldev_Texture.Init_Texture file " & "*" & Texture_File & "*");
--        Put_Line ("Ogldev_Texture.Init_Texture file size " &
--                    Ada.Directories.File_Size'Image (Ada.Directories.Size
--                    ("/Ada_Source/OglAda_Examples/ogl_dev/content/phoenix.pcx")));
      Result := Ada.Directories.Exists (Texture_File);
      if Result then
         theTexture.File_Name := To_Unbounded_String (Texture_File);
         theTexture.Texture_Target := Target_Type;
      else
         Put_Line ("Ogldev_Texture.Init_Texture file *" & Texture_File &
                     "* not found");
      end if;
      return Result;

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
      --  The following tested with project 19_specular_lighting
      --  Load_Blob is implemented by image_io.loadBlob
      --  which implements
      --  theImage.read(fileName);                m_image.read(m_fileName);
      --  theImage.write(&theBlob, cppString);    m_image.write(&m_blob, "RGBA");
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

--        Put_Line ("Ogldev_Texture.Load, image signature: " &
--                    Interfaces.C.size_t'Image (theTexture.Image.Signature));
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
         Byte_Index  : UInt := 0;
         Curs        : Cursor := Data_Blob.First;
         Level       : constant GL.Objects.Textures.Mipmap_Level := 0;
      begin
--           Put_Line ("Ogldev_Texture.Load, Blob_Length: " &
--                       UInt'Image (Blob_Length));
         while Has_Element (Curs) loop
            Byte_Index := Byte_Index + 1;
            Data (Byte_Index) := Element (Curs);
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

   function Texture_Map_Size (theMap : Ogldev_Texture.Mesh_Texture_Map)
                              return GL.Types.UInt is
   begin
      return GL.Types.UInt (theMap.Length);
   end Texture_Map_Size;

   --  -------------------------------------------------------------------------

end Ogldev_Texture;
