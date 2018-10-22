
--  with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

--  with Ogldev_Math;

package body Core_Image is

--     function To_AI_Image (theImage : Image) return AI_Image;
--     function To_AI_Image_Info (Info : Image_Info) return AI_Image_Info;

   --  -------------------------------------------------------------------------
   --  File_To_Image and Image_To_Blob moved from Magick_Blob
   --  to avoid circularities

--     procedure File_To_Image (theImage : in out Image; File_Name : String) is
--        use Magick_Pixel;
--
--        C_Image          : aliased AI_Image;
--  --        Access types declared at this level to avoid pointer irregualrities
--        C_Col_Map        : aliased Pix_Info;
--        C_Col_Map_Ptr    : constant Pix_Info_Ptr := C_Col_Map'Unchecked_Access;
--        C_Ch_Map         : aliased Chan_Map;
--        C_Ch_Map_Ptr     : constant Chan_Map_Ptr := C_Ch_Map'Unchecked_Access;
--        C_Asc_85         : aliased Asc_85;
--        C_Asc_85_Ptr     : constant Asc_85_Ptr := C_Asc_85'Unchecked_Access;
--        C_Blob_Info      : aliased Magick_Blob.Blob_Info;  --  Blob_Info is private
--        C_Blob_Info_Ptr  : constant Magick_Blob.Blob_Info_Ptr := C_Blob_Info'Unchecked_Access;
--        C_Profile        : aliased Profile;
--        C_Profile_Ptr    : constant Profile_Ptr := C_Profile'Unchecked_Access;
--        C_Semaphore      : aliased Semaphore.Semaphore_Info;
--        C_Semaphore_Ptr  : constant Semaphore.Sem_Ptr := C_Semaphore'Unchecked_Access;
--        C_Image_Info     : aliased AI_Image_Info;
--        C_Image_Info_Ptr : constant Info_Ptr := C_Image_Info'Unchecked_Access;
--        Except           : aliased Magick_Exception.AI_Exception_Info;
--     begin
--        Except.Semaphore := C_Semaphore_Ptr;
--        C_Blob.Semaphore := C_Semaphore_Ptr;
--
--        C_Image := To_AI_Image (theImage);
--        C_Col_Map := To_AI_Pixel_Info (theImage.Colour_Map);
--        C_Image.Colour_Map := C_Col_Map_Ptr;
--        C_Image.Channel_Map := C_Ch_Map_Ptr;
--        C_Image.Ascii_85 := C_Asc_85_Ptr;
--        C_Image.Blob := C_Blob_Info_Ptr;
--        C_Image.Generic_Profile := C_Profile_Ptr;
--        C_Image.Semaphore_Data := C_Semaphore_Ptr;
--        C_Image.Image_Info := C_Image_Info_Ptr;
--        Magick_Blob.Set_Blob_Type (C_Image.Blob.all, Magick_Blob.File_Stream);
--        if Magick_Image.API.File_To_Image (C_Image'Access, To_C (File_Name),
--                                           Except'Access) =
--          Magic_True then
--              theImage := To_Image (C_Image);
--          else
--              raise Core_Image_Exception with "Magick_Image.File_To_Image error";
--        end if;
--
--     exception
--        when others =>
--           Put_Line ("An exception occurred in Magick_Image.File_To_Image.");
--           raise;
--     end File_To_Image;

   --  -------------------------------------------------------------------------

--     procedure Image_To_Blob (Info : Image_Info; theImage : in out Image;
--                              Size : in out GL.Types.UInt) is
--        C_Image  : aliased AI_Image;
--        C_Info   : aliased AI_Image_Info := To_AI_Image_Info (Info);
--        C_Size   : aliased size_t := size_t (Size);
--        Except   : aliased Magick_Exception.AI_Exception_Info;
--     begin
--        Put_Line ("Entered Magick_Image.Image_To_Blob.");
--        C_Image := To_AI_Image (theImage);
--        if Magick_Image.API.Image_To_Blob
--            (C_Info'Access, C_Image'Access, C_Size'Access,
--             Except'Access) = Magic_True then
--           Size := GL.Types.UInt (C_Size);
--        else
--           raise Core_Image_Exception with "Magick_Image.Image_To_Blob error";
--             & Interfaces.C.To_Ada (Interfaces.C.Strings.Value
--                                  (Except.Error_Description));
--        end if;
--
--     exception
--        when others =>
--           Put_Line ("An exception occurred in Magick_Image.Image_To_Blob.");
--           raise;
--     end Image_To_Blob;

   --  -------------------------------------------------------------------------

--     function To_AI_Image (theImage : Image) return AI_Image is
--        use Ada.Strings.Unbounded;
--        use Magick_Pixel;
--        C_Image   : AI_Image;
--        Num_Chars : size_t;
--     begin
--        C_Image.Storage_Class      := theImage.Storage_Class;
--        C_Image.Colourspace        := theImage.Colourspace;
--        C_Image.Compression_Method := theImage.Compression_Method;
--        C_Image.Quality            := size_t (theImage.Quality);
--        C_Image.Orientation        := theImage.Orientation;
--        C_Image.Taint              := To_Magick_Boolean (theImage.Taint);
--        C_Image.Columns            := size_t (theImage.Columns);
--        C_Image.Rows               := size_t (theImage.Rows);
--        C_Image.Depth              := size_t (theImage.Depth);
--        C_Image.Alpha_Colour       := To_AI_Pixel_Info (theImage.Alpha_Colour);
--        C_Image.Background_Colour  := To_AI_Pixel_Info (theImage.Background_Colour);
--        C_Image.Border_Colour      := To_AI_Pixel_Info (theImage.Border_Colour);
--        C_Image.Transparent_Colour := To_AI_Pixel_Info (theImage.Transparent_Colour);
--        C_Image.Gamma              := double (theImage.Gamma);
--        C_Image.Chromaticity       := theImage.Chromaticity;
--        C_Image.Render_Intent      := theImage.Render_Intent;
--        C_Image.Profiles           := theImage.Profiles;
--        C_Image.Units              := theImage.Units;
--        C_Image.Montage            := New_String (theImage.Montage); --  chars_ptr;
--        C_Image.Directory          := New_String (theImage.Directory); --  chars_ptr;
--        C_Image.Geometry_Desc      := New_String (theImage.Geometry_Desc); --  chars_ptr;
--        C_Image.Offset             := ssize_t (theImage.Offset);
--        C_Image.Resolution         := theImage.Resolution;
--        C_Image.Page               := theImage.Page;
--        C_Image.Extract_Info       := theImage.Extract_Info;
--        C_Image.Fuzz               := double (theImage.Fuzz);
--        C_Image.Filter             := theImage.Filter;
--        C_Image.Intensity          := theImage.Intensity;
--        C_Image.Image_Interlace    := theImage.Image_Interlace;
--        C_Image.Endian             := theImage.Endian;
--        C_Image.Gravity            := theImage.Gravity;
--        C_Image.Compose            := theImage.Compose;
--        C_Image.Dispose            := theImage.Dispose;
--        C_Image.Scene              := size_t (theImage.Delay_Time);
--        C_Image.Delay_Time         := size_t (theImage.Delay_Time);
--        C_Image.Duration           := size_t (theImage.Delay_Time);
--        C_Image.Ticks_Per_Sec      := ssize_t (theImage.Delay_Time);
--        C_Image.Iterations         := size_t  (theImage.Delay_Time);
--        C_Image.Total_Colours      := size_t  (theImage.Delay_Time);
--        C_Image.Start_Loop         := ssize_t  (theImage.Delay_Time);
--        C_Image.Interpolate        := theImage.Interpolate;
--        C_Image.Black_Point_Compensation := To_Magick_Boolean (theImage.Black_Point_Compensation);
--        C_Image.Tile_Offset        := theImage.Tile_Offset;
--        C_Image.Kind               := theImage.Kind;
--        C_Image.Dither             := To_Magick_Boolean (theImage.Dither);
--        C_Image.Extent             := size_t (theImage.Extent);
--        C_Image.Ping               := To_Magick_Boolean (theImage.Ping);
--        C_Image.Read_Mask          := To_Magick_Boolean (theImage.Read_Mask);
--        C_Image.Write_Mask         := To_Magick_Boolean (theImage.Write_Mask);
--        C_Image.Alpha_Trait        := theImage.Alpha_Trait;
--        C_Image.Num_Channels       := size_t (theImage.Num_Channels);
--        C_Image.Num_Meta_Channels  := size_t (theImage.Num_Meta_Channels);
--        C_Image.Metacontent_Extent := size_t (theImage.Metacontent_Extent);
--        C_Image.Channel_Mask       := theImage.Channel_Mask;
--        C_Image.Image_Cache        := theImage.Image_Cache;
--        C_Image.Error_Data         := theImage.Error_Data;
--        C_Image.Timer_Data         := theImage.Timer_Data;
--        C_Image.Progress_Monitor   := theImage.Progress_Monitor;
--        C_Image.Client_Data        := theImage.Client_Data;
--        C_Image.Properties         := theImage.Properties;
--        C_Image.Artifacts          := theImage.Artifacts;
--        To_C (To_String (theImage.File_Name), C_Image.File_Name, Num_Chars);
--        To_C (To_String (theImage.Magick_File_Name), C_Image.Magick_File_Name, Num_Chars);
--        To_C (To_String (theImage.Magick), C_Image.Magick, Num_Chars);
--        C_Image.Magick_Columns     := size_t (theImage.Columns);
--        C_Image.Magick_Rows        := size_t (theImage.Rows);
--        C_Image.Time_Stamp         := ssize_t (theImage.Time_Stamp);
--        C_Image.Debug              := To_Magick_Boolean (theImage.Debug);
--        C_Image.Reference_Count    := ssize_t (theImage.Reference_Count);
--        --  C_Image.Texture        access type set in File_To_Image
--        --  C_Image.Semaphore_Data access type set in File_To_Image
--        C_Image.List               := theImage.List;
--        C_Image.Previous           := theImage.Previous;
--        C_Image.Next               := theImage.Next;
--        C_Image.Signature          := 0;
--          size_t (Method_Attribute.Magick_Core_Signature);
--        C_Image.Matte_Colour       := theImage.Matte_Colour;
--
--        return C_Image;
--
--     end To_AI_Image;

   --  -------------------------------------------------------------------------

--     function To_AI_Image_Info (Info : Image_Info) return AI_Image_Info is
--        use Ada.Strings.Unbounded;
--        C_Info : AI_Image_Info;
--        Num_Chars : size_t;
--     begin
--        C_Info.Compression := Info.Compression;
--        C_Info.Orientation        := Info.Orientation;
--        C_Info.Temporary          := To_Magick_Boolean (Info.Temporary);
--        C_Info.Adjoin             := To_Magick_Boolean (Info.Adjoin);
--        C_Info.Affirm             := To_Magick_Boolean (Info.Affirm);
--        C_Info.Antialias          := To_Magick_Boolean (Info.Antialias);
--        C_Info.Size               := New_String (Info.Size);
--        C_Info.Extract            := New_String (Info.Extract);
--        C_Info.Page               := New_String (Info.Page);
--        C_Info.Scenes             := New_String (Info.Scenes);
--        C_Info.Scene              := size_t (Info.Scene);
--        C_Info.Num_Scenes         := size_t (Info.Num_Scenes);
--        C_Info.Depth              := size_t (Info.Depth);
--        C_Info.Interlace          := Info.Interlace;
--        C_Info.Endian             := Info.Endian;
--        C_Info.Units              := Info.Units;
--        C_Info.Quality            := size_t (Info.Quality);
--        C_Info.Sampling_Factor    := New_String (Info.Sampling_Factor);
--        C_Info.Server_Name        := New_String (Info.Server_Name);
--        C_Info.Font               := New_String (Info.Font);
--        C_Info.Texture            := New_String (Info.Texture);
--        C_Info.Density            := New_String (Info.Density);
--        C_Info.Point_Size         := double (Info.Point_Size);
--        C_Info.Fuzz               := double (Info.Fuzz);
--        C_Info.Background_Colour  := Info.Background_Colour;
--        C_Info.Border_Colour      := Info.Border_Colour;
--        C_Info.Transparent_Colour := Info.Transparent_Colour;
--        C_Info.Dither             := To_Magick_Boolean (Info.Dither);
--        C_Info.Monochrome         := To_Magick_Boolean (Info.Monochrome);
--        C_Info.Col_Space          := Info.Col_Space;
--        C_Info.Compose            := Info.Compose;
--        C_Info.Image_Kind         := Info.Image_Kind;
--        C_Info.Ping               := To_Magick_Boolean (Info.Ping);
--        C_Info.Verbose            := To_Magick_Boolean (Info.Verbose);
--        C_Info.Channel            := Info.Channel;
--        C_Info.Options            := Info.Options;
--        C_Info.Profile            := Info.Profile;
--        C_Info.Synchronize        := To_Magick_Boolean (Info.Synchronize);
--        C_Info.Progress_Monitor   := Info.Progress_Monitor;
--                             --  Magick_Progress_Monitor;
--                             --  Specifies a pointer to a method to
--                             --  monitor progress of an image operation.
--        C_Info.Client_Data        := Info.Client_Data;
--        C_Info.Image_Cache        := Info.Image_Cache;
--        C_Info.Stream             := Info.Stream;  --  Stream.Stream_Handler;
--        C_Info.File_ID            := New_String (Info.File_ID);
--        C_Info.Blob               := Info.Blob;
--        C_Info.Length             := size_t (Info.Length);
--        To_C (To_String (Info.Magick), C_Info.Magick, Num_Chars);
--        To_C (To_String (Info.Unique), C_Info.Unique, Num_Chars);
--        To_C (To_String (Info.File_Name), C_Info.File_Name, Num_Chars);
--        C_Info.Debug              := To_Magick_Boolean (Info.Debug);
--  --        C_Info.Custom_Stream_Info := Info.Custom_Stream;
--        C_Info.Signature          := 0;
--     size_t (Info.Signature);
--        C_Info.Matte_Colour       := Info.Matte_Colour;
--        return C_Info;
--     end To_AI_Image_Info;

   --  -------------------------------------------------------------------------

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
      theImage.Signature          := 0;
--          size_t (Method_Attribute.Magick_Core_Signature);
      theImage.Matte_Colour       := C_Image.Matte_Colour;
      return theImage;

   exception
      when others =>
         Put_Line ("An exception occurred in Magick_Image.To_Image.");
         raise;
   end To_Image;

   --  -------------------------------------------------------------------------

end Core_Image;
