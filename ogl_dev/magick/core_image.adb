
with Ada.Text_IO; use Ada.Text_IO;

package body Core_Image is

   function To_Image (C_Image : API_Image) return Image is
      use Ada.Strings.Unbounded;
      use GL.Types;
      use Magick_Pixel;
        theImage : Image;
   begin
      theImage.Storage_Class      := C_Image.Storage_Class;
      theImage.Colourspace        := C_Image.Colourspace;
      theImage.Compression_Method := C_Image.Compression_Method;
      theImage.Quality            := UInt (C_Image.Quality);
      theImage.Orientation        := C_Image.Orientation;
      theImage.Taint              := To_Boolean (C_Image.Taint);
      theImage.Columns            := UInt (C_Image.Columns);
      theImage.Rows               := UInt (C_Image.Rows);
      theImage.Depth              := UInt (C_Image.Depth);
      theImage.Alpha_Colour       := To_Pixel_Info (C_Image.Alpha_Colour);
      theImage.Background_Colour  := To_Pixel_Info (C_Image.Background_Colour);
      theImage.Border_Colour      := To_Pixel_Info (C_Image.Border_Colour);
      theImage.Transparent_Colour := To_Pixel_Info (C_Image.Transparent_Colour);
      theImage.Gamma              := Single (C_Image.Gamma);
      theImage.Chromaticity       := C_Image.Chromaticity;
      theImage.Render_Intent      := C_Image.Render_Intent;
      theImage.Profiles           := C_Image.Profiles;
      theImage.Units              := C_Image.Units;
      if C_Image.Montage /= Null_Ptr then
            theImage.Montage      := Value (C_Image.Montage); --  chars_ptr;
      end if;
      if C_Image.Directory /= Null_Ptr then
            theImage.Directory          := Value (C_Image.Directory); --  chars_ptr;
      end if;
      if C_Image.Geometry_Desc /= Null_Ptr then
            theImage.Geometry_Desc      := Value (C_Image.Geometry_Desc); --  chars_ptr;
      end if;
      theImage.Offset             := Long_Integer (C_Image.Offset);
      theImage.Resolution         := C_Image.Resolution;
      theImage.Page               := C_Image.Page;
      theImage.Extract_Info       := C_Image.Extract_Info;
      theImage.Fuzz               := Single (C_Image.Fuzz);
      theImage.Filter             := C_Image.Filter;
      theImage.Intensity          := C_Image.Intensity;
      theImage.Image_Interlace    := C_Image.Image_Interlace;
      theImage.Endian             := C_Image.Endian;
      theImage.Gravity            := C_Image.Gravity;
      theImage.Compose            := C_Image.Compose;
      theImage.Dispose            := C_Image.Dispose;
      theImage.Scene              := UInt (C_Image.Delay_Time);
      theImage.Delay_Time         := UInt (C_Image.Delay_Time);
      theImage.Duration           := UInt (C_Image.Delay_Time);
      theImage.Ticks_Per_Sec      := Long_Integer (C_Image.Delay_Time);
      theImage.Iterations         := UInt (C_Image.Delay_Time);
      theImage.Total_Colours      := UInt  (C_Image.Delay_Time);
      theImage.Start_Loop         := Long_Integer  (C_Image.Delay_Time);
      theImage.Interpolate        := C_Image.Interpolate;
      theImage.Black_Point_Compensation := To_Boolean (C_Image.Black_Point_Compensation);
      theImage.Tile_Offset        := C_Image.Tile_Offset;
      theImage.Kind               := C_Image.Kind;
      theImage.Dither             := To_Boolean (C_Image.Dither);
      theImage.Extent             := UInt (C_Image.Extent);
      theImage.Ping               := To_Boolean (C_Image.Ping);
      theImage.Read_Mask          := To_Boolean (C_Image.Read_Mask);
      theImage.Write_Mask         := To_Boolean (C_Image.Write_Mask);
      theImage.Alpha_Trait        := C_Image.Alpha_Trait;
      theImage.Num_Channels       := UInt (C_Image.Num_Channels);
      theImage.Num_Meta_Channels  := UInt (C_Image.Num_Meta_Channels);
      theImage.Metacontent_Extent := UInt (C_Image.Metacontent_Extent);
      theImage.Channel_Mask       := C_Image.Channel_Mask;
      theImage.Image_Cache        := C_Image.Image_Cache;
      theImage.Error_Data         := C_Image.Error_Data;
      theImage.Timer_Data         := C_Image.Timer_Data;
      theImage.Progress_Monitor   := C_Image.Progress_Monitor;
      theImage.Client_Data        := C_Image.Client_Data;
      theImage.Properties         := C_Image.Properties;
      theImage.Artifacts          := C_Image.Artifacts;
      theImage.File_Name := To_Unbounded_String (To_Ada (C_Image.File_Name));
      theImage.Magick_File_Name := To_Unbounded_String (To_Ada  (C_Image.Magick_File_Name));
      theImage.Magick := To_Unbounded_String (To_Ada  (C_Image.Magick));
      theImage.Magick_Columns     := UInt (C_Image.Columns);
      theImage.Magick_Rows        := UInt (C_Image.Rows);
      theImage.Time_Stamp         := Long_Integer (C_Image.Time_Stamp);
      theImage.Debug              := To_Boolean (C_Image.Debug);
      theImage.Reference_Count    := Long_Integer (C_Image.Reference_Count);
      --  theImage.Texture        access type set in File_To_Image
      --  theImage.Semaphore_Data access type set in File_To_Image
      theImage.List               := C_Image.List;
      theImage.Previous           := C_Image.Previous;
      theImage.Next               := C_Image.Next;
      theImage.Signature          := C_Image.Signature;
      theImage.Matte_Colour       := C_Image.Matte_Colour;
      return theImage;

   exception
      when others =>
         Put_Line ("An exception occurred in Magick_Image.To_Image.");
         raise;
   end To_Image;

   --  -------------------------------------------------------------------------

end Core_Image;
