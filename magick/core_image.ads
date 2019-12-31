
with System;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with  Ada.Strings.Unbounded;

with GL.Objects.Textures;
with GL.Types;

with Cache;
with Colour;
with Colour_Space;
with Composite;
with Compress;
with Core_Blob;
with Geometry;
with Layer;
with Core_Blob.API;
with Magick_Exception;
with Magick_Pixel;
with Magick_Type; use Magick_Type;
with Magick_Profile;
with Method;
with Monitor;
with Quantum;
with Resample;
with Semaphore;
--  with Stream;
with Timer;

package Core_Image is
--  Derived from
--  /usr/local/Cellar/imagemagick/7.0.7-31/include/ImageMagick-7/MagickCore/image.h
   Undefined_Ticks_Per_Second    : constant long := 100;
   Undefined_Compression_Quality : constant unsigned_long := 0;

    type Alpha_Channel_Option is (Undefined_Alpha_Channel,
                                  Activate_Alpha_Channel,
                                  Associate_Alpha_Channel,
                                  Background_Alpha_Channel,
                                  Copy_Alpha_Channel,
                                  Deactivate_Alpha_Channel,
                                  Discrete_Alpha_Channel,
                                  Disassociate_Alpha_Channel,
                                  Extract_Alpha_Channel,
                                  Off_Alpha_Channel,
                                  On_Alpha_Channel,
                                  Opaque_Alpha_Channel,
                                  Remove_Alpha_Channel,
                                  Set_Alpha_Channel,
                                  Shape_Alpha_Channel,
                                  Transparent_Alpha_Channel);
    pragma Convention (C, Alpha_Channel_Option);

    type Image_Type is (Undefined_Type,
                        Bilevel_Type,
                        Grayscale_Type,
                        Grayscale_Alpha_Type,
                        Palette_Type,
                        Palette_Alpha_Type,
                        True_Colour_Type,
                        True_Colour_Alpha_Type,
                        Colour_Separation_Type,
                        Colour_Separation_Alpha_Type,
                        Optimize_Type,
                        Palette_Bilevel_Alpha_Type);
    pragma Convention (C, Image_Type);

    type Interlace_Type is (Undefined_Interlace,
                            No_Interlace,
                            Line_Interlace,
                            Plane_Interlace,
                            Partition_Interlace,
                            GIF_Interlace,
                            JPEG_Interlace,
                            PNG_Interlace);
    pragma Convention (C, Interlace_Type);

    type Orientation_Type is (Undefined_Orientation,
                              Top_Left_Orientation,
                              Top_Right_Orientation,
                              Bottom_Right_Orientation,
                              Bottom_Left_Orientation,
                              Left_Top_Orientation,
                              Right_Top_Orientation,
                              Right_Bottom_Orientation,
                              Left_Bottom_Orientation);
    pragma Convention (C, Orientation_Type);

    type Resolution_Type is (Undefined_Resolution,
                             Pixels_Per_Inch_Resolution,
                             Pixels_Per_Centimeter_Resolution);
    pragma Convention (C, Resolution_Type);

    --     type VP_Method_Access is access function (I: C.int)
    --                    return Cache.View.Virtual_Pixel_Method;
    --        pragma Convention (C, VP_Method_Access);
    --     Virtual_Pixel_Method_Access : VP_Method_Access;
    --     pragma Import (C, Virtual_Pixel_Method_Access, "virtualpixelmethod_ptr");

    type Primary_Info is record
        X, Y, Z : double;
    end record;
    pragma Convention (C_Pass_By_Copy, Primary_Info);

    type Segment_Info is record
        X1, Y1 : double;
        X2, Y2 : double;
    end record;
    pragma Convention (C_Pass_By_Copy, Segment_Info);

    type Transmit_Type is (Undefined_Transmit_Type,
                           File_Transmit_Type,
                           Blob_Transmit_Type,
                           Stream_Transmit_Type,
                           Image_Transmit_Type);
    pragma Convention (C, Transmit_Type);

   --  Default values from imag.c AcquireImage
    type Chromatic_Info is record
        Red_Primary   : Primary_Info := (0.64, 0.33, 0.03);
        Green_Primary : Primary_Info := (0.3, 0.6, 1.0);
        Blue_Primary  : Primary_Info := (0.15, 0.06, 0.79);
        White_Point   : Primary_Info := (0.3127, 0.329, 0.3583);
    end record;
    pragma Convention (C_Pass_By_Copy, Chromatic_Info);

    subtype Image_Filename_Array is Interfaces.C.char_array (0 .. 4095);
    subtype Image_Magick_Filename_Array is Interfaces.C.char_array (0 .. 4095);
    subtype Image_Magick_Array is Interfaces.C.char_array (0 .. 4095);

    type Pix_Info is new Magick_Pixel.AI_Pixel_Info;
    type Pix_Info_Ptr is access all Pix_Info;
    type Chan_Map is new Magick_Pixel.AI_Pixel_Channel_Map;
    type Asc_85 is new Compress.Ascii_85_Info;
    type Asc_85_Ptr is access all Asc_85;
    type Profile is new Magick_Profile.Profile_Info;
    type Profile_Ptr is access all Profile;
    type Source is new GL.Objects.Textures.Image_Source;
    type Source_Ptr is access all Source;

    subtype Image_Info_Magick_Array is Interfaces.C.char_array (0 .. 4095);
    subtype Image_Info_Unique_Array is Interfaces.C.char_array (0 .. 4095);
    subtype Image_Info_Zero_Array is Interfaces.C.char_array (0 .. 4095);
    subtype Image_Info_Filename_Array is Interfaces.C.char_array (0 .. 4095);
    --  AI_Image_Info stores an image list, with all global settings used by all images
    --  held, unless overridden for that specific image.  See Sync_Image_Settings
    --  which maps any global setting that always overrides specific image settings.
    type Stream_Info;
    type API_Image_Info is record
        Compression        : Compress.Compression_Type
          := Compress.Undefined_Compression;
        Orientation        : Orientation_Type := Undefined_Orientation;
        Temporary          : Magick_Boolean_Type := Magic_False;
        Adjoin             : Magick_Boolean_Type := Magic_True;
        Affirm             : Magick_Boolean_Type := Magic_False;
        Antialias          : Magick_Boolean_Type := Magic_True;
        Size               : chars_ptr;    --  image generation size
        Extract            : chars_ptr;    --  crop/resize string on image read
        Page               : chars_ptr;
        Scenes             : chars_ptr;    --  scene numbers that is to be read in
        Scene              : size_t := 99;  --  starting value for image save numbering
        Num_Scenes         : size_t := 98;  --  total number of images in list - for escapes
        Depth              : size_t := 97;  --  current read/save depth of images
        Interlace          : Interlace_Type := No_Interlace;
        Endian             : Quantum.Endian_Type;
        Units              : Resolution_Type := Undefined_Resolution;
        Quality            : size_t := size_t (Undefined_Compression_Quality);
        Sampling_Factor    : chars_ptr;  --  JPEG write sampling factor
        Server_Name        : chars_ptr;  --  X windows server name - display/animate
        Font               : chars_ptr;  --  DUP for draw_info
        Texture            : chars_ptr;  --  montage/display background tile
        Density            : chars_ptr;  --  DUP for image and draw_info
        Point_Size         : double := 0.0;
        Fuzz               : double := 0.0;
        Alpha_Colour       : Magick_Pixel.Pixel_Info;
        Background_Colour  : Magick_Pixel.Pixel_Info;
        Border_Colour      : Magick_Pixel.Pixel_Info;
        Transparent_Colour : Magick_Pixel.Pixel_Info;
        Dither             : Magick_Boolean_Type := Magic_True;
        Monochrome         : Magick_Boolean_Type := Magic_False;
        Col_Space          : Colour_Space.Colourspace_Type
          := Colour_Space.Undefined_Colourspace;
        Compose            : Composite.Composite_Operator;
        Image_Kind         : Image_Type := Undefined_Type;
        Ping               : Magick_Boolean_Type := Magic_False;
        Verbose            : Magick_Boolean_Type := Magic_False;
        Channel            : Channel_Type := Default_Channels;
        Options            : System.Address := System.Null_Address;  --  splay tree of global options
        Profile            : System.Address := System.Null_Address;
        Synchronize        : Magick_Boolean_Type := Magic_False;
        Progress_Monitor   : System.Address := System.Null_Address;
        --  Magick_Progress_Monitor;
        --  Specifies a pointer to a method to
        --  monitor progress of an image operation.
        Client_Data        : System.Address := System.Null_Address;
        Image_Cache        : System.Address := System.Null_Address;
        Stream             : System.Address := System.Null_Address;  --  Stream.Stream_Handler;
        File_ID            : chars_ptr;
        Blob               : System.Address := System.Null_Address;
        Length             : size_t := 0;
        Magick             : Image_Info_Magick_Array :=
                             (others => char (Character'Val (0)));    --  image file format
        Unique             : Image_Info_Unique_Array :=
                             (others => char (Character'Val (0)));    --  unique tempory filename - delegates
      File_Name          : Image_Info_Filename_Array :=
                             ('H', 'e', 'l', 'l', 'o', others => char (Character'Val (0)));  --  filename when reading/writing image
        Debug              : Magick_Boolean_Type := Magic_False;
        Signature          : size_t := 0;
--                                 size_t (Method_Attribute.Magick_Core_Signature);
        Custom_Stream_Info : access Core_Blob.Custom_Stream_Info := Null;
        Matte_Colour       : Magick_Pixel.Pixel_Info;
    end record;  --  API_Image_Info
    pragma Convention (C_Pass_By_Copy, API_Image_Info);

   type Info_Ptr is access all API_Image_Info;

    type API_Image is record
        Storage_Class      : Class_Type := Magick_Type.Direct_Class;
        Colourspace        : Colour_Space.Colourspace_Type :=
                               Colour_Space.RGB_Colourspace;
        Compression_Method : Compress.Compression_Type :=
                               Compress.Undefined_Compression;
        Quality            : size_t := 0;
        Orientation        : Orientation_Type := Undefined_Orientation;
        Taint              : Magick_Boolean_Type := Magic_False;
        Columns            : size_t := 0;  --  physical size_t of image
        Rows               : size_t := 0;
        Depth              : size_t := Magickcore_Quantum_Depth;
        Colours            : size_t := 0;  --  size_t of color table on read
        Colour_Map         : Pix_Info_Ptr := Null;
        Alpha_Colour       : Magick_Pixel.AI_Pixel_Info;
        Background_Colour  : Magick_Pixel.AI_Pixel_Info;
        Border_Colour      : Magick_Pixel.AI_Pixel_Info;
        Transparent_Colour : Magick_Pixel.AI_Pixel_Info;
        Gamma              : double := 1.0 / 2.2;
        Chromaticity       : Chromatic_Info;
        Render_Intent      : Magick_Profile.Rendering_Intent :=
                               Magick_Profile.Perceptual_Intent;
        Profiles           : System.Address := System.Null_Address;
        Units              : Resolution_Type := Undefined_Resolution;
        Montage            : chars_ptr := Null_Ptr;
        Directory          : chars_ptr := Null_Ptr;
        Geometry_Desc      : chars_ptr := Null_Ptr;
        Offset             : ssize_t := 0;      --  ssize_t
        Resolution         : Geometry.Point_Info;
        Page               : Geometry.Rectangle_Info;
        Extract_Info       : Geometry.Rectangle_Info;
        Fuzz               : double := 0.0;
        Filter             : Resample.Filter_Types := Resample.Undefined_Filter;
        Intensity          : Magick_Pixel.Pixel_Intensity_Method;
        Image_Interlace    : Interlace_Type := No_Interlace;
        Endian             : Quantum.Endian_Type;
        Gravity            : Geometry.Gravity_Type := Geometry.Undefined_Gravity;
        Compose            : Composite.Composite_Operator :=
                               Composite.Over_Composite_Op;
        Dispose            : Layer.Dispose_Type := Layer.Undefined_Dispose;
        Scene              : size_t := 0;
        Delay_Time         : size_t := 0;
        Duration           : size_t := 0;
        Ticks_Per_Sec      : ssize_t := ssize_t (Undefined_Ticks_Per_Second);
        Iterations         : size_t := 0;
        Total_Colours      : size_t := 0;
        Start_Loop         : ssize_t := 0;      --  ssize_t
        Interpolate        : Magick_Pixel.Pixel_Interpolate_Method
          := Magick_Pixel.Undefined_Interpolate_Pixel;
        Black_Point_Compensation : Magick_Boolean_Type := Magic_False;
        Tile_Offset        : Geometry.Rectangle_Info;
        Kind               : Image_Type := Undefined_Type;
        Dither             : Magick_Boolean_Type := Magic_False;
        Extent             : size_t := 0;  --  Magick_Size_Type;
        Ping               : Magick_Boolean_Type := Magic_False;
        Read_Mask          : Magick_Boolean_Type := Magic_False;
        Write_Mask         : Magick_Boolean_Type := Magic_False;
        Alpha_Trait        : Magick_Pixel.Pixel_Trait;
        Num_Channels       : size_t := 0;
        Num_Meta_Channels  : size_t := 0;
        Metacontent_Extent : size_t := 0;
        Channel_Mask       : Channel_Type := Default_Channels;
        Channel_Map        : Magick_Pixel.Chan_Map_Ptr :=
                             Magick_Pixel.Acquire_Pixel_Channel_Map;
        Image_Cache        : Cache.Cache;
--          Image_Cache        : System.Address := System.Null_Address;
        Error_Data         : Colour.Error_Info;
        Timer_Data         : Timer.Timer_Info;
        Progress_Monitor   : Monitor.Magick_Progress_Monitor;
        --                             Specifies a pointer to a method to monitor
        --                             progress of an image operation.
        Client_Data        : System.Address := System.Null_Address;
        Ascii_85           : Asc_85_Ptr := Null;
        Generic_Profile    : Profile_Ptr := Null;
        Properties         : System.Address := System.Null_Address;
        Artifacts          : System.Address := System.Null_Address;
        File_Name          : Image_Filename_Array;
        Magick_File_Name   : Image_Magick_Filename_Array;  --  Coder used to decode image
        Magick             : Image_Magick_Array;
        Magick_Columns     : size_t := 0;
        Magick_Rows        : size_t := 0;
        Blob               : Core_Blob.Blob_Info_Ptr :=
        Core_Blob.API.Clone_Blob_Info (Null);
        Time_Stamp         : ssize_t := 0; --  time_t
        Debug              : Magick_Boolean_Type := Magic_False;
        Reference_Count    : ssize_t := 1;
        Semaphore_Data     : Semaphore.Sem_Ptr := Semaphore.Acquire_Semaphore_Info;
        Image_Info         : Info_Ptr := Null;
        --  Image processing list
        --        List               : access API_Image := Null;
        --        Previous           : access API_Image := Null;
        --        Next               : access API_Image := Null;
        List               : System.Address := System.Null_Address;
        Previous           : System.Address := System.Null_Address;
        Next               : System.Address := System.Null_Address;
        Signature          : size_t := 0;
--                                 size_t (Method_Attribute.Magick_Core_Signature);
        Matte_Colour       : Magick_Pixel.Pixel_Info;
    end record;  --  API_Image
   pragma Convention (C_Pass_By_Copy, API_Image);

   type API_Image_Ptr is access all API_Image;

    type Stream_Info is record
        theImage_Data : API_Image_Info;
        theImage      : access API_Image := Null;
        Stream        : access API_Image := Null;
        Quantum_Data  : Quantum.Quantum_Info;
        Map           : chars_ptr := Null_Ptr;
        Storage_Kind  : Magick_Pixel.Storage_Type;
        Pixels        : access unsigned_char := Null;
        Extract_Info  : Geometry.Rectangle_Info;
        Y             : size_t := 0;  --  ssize_t
        Except        : access  Magick_Exception.Exception_Info := Null;
        Client_Data   : System.Address := System.Null_Address;
        Signature     : size_t := 0;
--        size_t (Method_Attribute.Magick_Core_Signature);
    end record;  --  Stream_Info

    type Image is record
        Storage_Class      : Class_Type := Magick_Type.Undefined_Class;
        Colourspace        : Colour_Space.Colourspace_Type :=
                               Colour_Space.Undefined_Colourspace;
        Compression_Method : Compress.Compression_Type :=
                               Compress.Undefined_Compression;
        Quality            : GL.Types.UInt := 0;
        Orientation        : Orientation_Type := Undefined_Orientation;
        Taint              : Boolean := False;
        Columns            : GL.Types.UInt := 0;  --  physical size_t of image
        Rows               : GL.Types.UInt := 0;
        Depth              : GL.Types.UInt := 0;
        Colours            : GL.Types.UInt := 0;  --  size_t of color table on read
        Colour_Map         : aliased Magick_Pixel.Pixel_Info;
        Alpha_Colour       : Magick_Pixel.Pixel_Info;
        Background_Colour  : Magick_Pixel.Pixel_Info;
        Border_Colour      : Magick_Pixel.Pixel_Info;
        Transparent_Colour : Magick_Pixel.Pixel_Info;
        Gamma              : GL.Types.Single := 0.0;
        Chromaticity       : Chromatic_Info;
        Render_Intent      : Magick_Profile.Rendering_Intent :=
                               Magick_Profile.Undefined_Intent;
        Profiles           : System.Address := System.Null_Address;
        Units              : Resolution_Type := Undefined_Resolution;
        Montage            : String (1 .. 8) := "        ";
        Directory          : String (1 .. 8) := "        ";
        Geometry_Desc      : String (1 .. 8) := "        ";
        Offset             : Long_Integer := 0;      --  ssize_t
        Resolution         : Geometry.Point_Info;
        Page               : Geometry.Rectangle_Info;
        Extract_Info       : Geometry.Rectangle_Info;
        Fuzz               : GL.Types.Single := 0.0;
        Filter             : Resample.Filter_Types := Resample.Undefined_Filter;
        Intensity          : Magick_Pixel.Pixel_Intensity_Method;
        Image_Interlace    : Interlace_Type := Undefined_Interlace;
        Endian             : Quantum.Endian_Type;
        Gravity            : Geometry.Gravity_Type := Geometry.Undefined_Gravity;
        Compose            : Composite.Composite_Operator :=
                               Composite.Undefined_Composite_Op;
        Dispose            : Layer.Dispose_Type := Layer.Undefined_Dispose;
        Scene              : GL.Types.UInt := 0;
        Delay_Time         : GL.Types.UInt := 0;
        Duration           : GL.Types.UInt := 0;
        Ticks_Per_Sec      : Long_Integer := 0;      --  ssize_t
        Iterations         : GL.Types.UInt := 0;
        Total_Colours      : GL.Types.UInt := 0;
        Start_Loop         : Long_Integer := 0;      --  ssize_t
        Interpolate        : Magick_Pixel.Pixel_Interpolate_Method
          := Magick_Pixel.Undefined_Interpolate_Pixel;
        Black_Point_Compensation : Boolean := False;
        Tile_Offset        : Geometry.Rectangle_Info;
        Kind               : Image_Type := Undefined_Type;
        Dither             : Boolean := False;
        Extent             : GL.Types.UInt := 0;  --  Magick_Size_Type;
        Ping               : Boolean := False;
        Read_Mask          : Boolean := False;
        Write_Mask         : Boolean := False;
        Alpha_Trait        : Magick_Pixel.Pixel_Trait;
        Num_Channels       : GL.Types.UInt := 0;
        Num_Meta_Channels  : GL.Types.UInt := 0;
        Metacontent_Extent : GL.Types.UInt := 0;
        Channel_Mask       : Channel_Type := Undefined_Channel;
        Channel_Map        : aliased Magick_Pixel.Pixel_Channel_Map;
        Image_Cache        : Cache.Cache;
        Error_Data         : Colour.Error_Info;
        Timer_Data         : Timer.Timer_Info;
        Progress_Monitor   : Monitor.Magick_Progress_Monitor;
        --                             Specifies a pointer to a method to monitor
        --                             progress of an image operation.
        Client_Data        : System.Address := System.Null_Address;
        Ascii_85           : Compress.Ascii_85_Info;
        Generic_Profile    : aliased Magick_Profile.Profile_Info;
        Properties         : System.Address := System.Null_Address;
        Artifacts          : System.Address := System.Null_Address;
        File_Name          : Ada.Strings.Unbounded.Unbounded_String;
        Magick_File_Name   : Ada.Strings.Unbounded.Unbounded_String;  --  Coder used to decode image
        Magick             : Ada.Strings.Unbounded.Unbounded_String;
        Magick_Columns     : GL.Types.UInt := 0;
        Magick_Rows        : GL.Types.UInt := 0;
        Blob               : aliased Core_Blob.Blob_Info;
        Time_Stamp         : Long_Integer := 0; --  time_t
        Debug              : Boolean := False;
        Reference_Count    : Long_Integer := 0;
        Semaphore_Data     : Semaphore.Sem_Ptr := Null;
        Image_Data         : aliased GL.Objects.Textures.Image_Source;
        --  Image processing list
        List               : System.Address;
        Previous           : System.Address;
        Next               : System.Address;
        Signature          : size_t := 0;
--                                 size_t (Method_Attribute.Magick_Core_Signature);
        Matte_Colour       : Magick_Pixel.Pixel_Info;
    end record;  --  Image

    type Image_Info is record
        Compression        : Compress.Compression_Type
          := Compress.Undefined_Compression;
        Orientation        : Orientation_Type := Undefined_Orientation;
        Temporary          : Boolean := False;
        Adjoin             : Boolean := False;
        Affirm             : Boolean := False;
        Antialias          : Boolean := False;
        Size               : String (1 .. 8);
        Extract            : String (1 .. 8);
        Page               : String (1 .. 8);
        Scenes             : String (1 .. 8);
        Scene              : GL.Types.UInt := 0;
        Num_Scenes         : GL.Types.UInt := 0;
        Depth              : GL.Types.UInt := 0;
        Interlace          : Interlace_Type := Undefined_Interlace;
        Endian             : Quantum.Endian_Type;
        Units              : Resolution_Type := Undefined_Resolution;
        Quality            : GL.Types.UInt := 0;
        Sampling_Factor    : String (1 .. 8);
        Server_Name        : String (1 .. 8);
        Font               : String (1 .. 8);
        Texture            : String (1 .. 8);
        Density            : String (1 .. 8);
        Point_Size         : GL.Types.Single := 0.0;
        Fuzz               : GL.Types.Single := 0.0;
        Background_Colour  : Magick_Pixel.Pixel_Info;
        Border_Colour      : Magick_Pixel.Pixel_Info;
        Transparent_Colour : Magick_Pixel.Pixel_Info;
        Dither             : Boolean := False;
        Monochrome         : Boolean := False;
        Col_Space          : Colour_Space.Colourspace_Type
          := Colour_Space.Undefined_Colourspace;
        Compose            : Composite.Composite_Operator;
        Image_Kind         : Image_Type := Undefined_Type;
        Ping               : Boolean := False;
        Verbose            : Boolean := False;
        Channel            : Channel_Type := Undefined_Channel;
        Options            : System.Address := System.Null_Address;
        Profile            : System.Address := System.Null_Address;
        Synchronize        : Boolean := False;
        Progress_Monitor   : System.Address := System.Null_Address;
        --  Magick_Progress_Monitor;
        --  Specifies a pointer to a method to
        --  monitor progress of an image operation.
        Client_Data        : System.Address := System.Null_Address;
        Image_Cache        : System.Address := System.Null_Address;
        Stream             : System.Address := System.Null_Address;  --  Stream.Stream_Handler;
        File_ID            : String (1 .. 8);
        Blob               : System.Address := System.Null_Address;
        Length             : GL.Types.UInt := 0;
        Magick             : Ada.Strings.Unbounded.Unbounded_String;
        Unique             : Ada.Strings.Unbounded.Unbounded_String;
        File_Name          : Ada.Strings.Unbounded.Unbounded_String;
        Debug              : Boolean := False;
        Signature          : size_t := 0;
--        := GL.Types.UInt (Method_Attribute.Magick_Core_Signature);
        Custom_Stream      : Core_Blob.Custom_Stream_Ptr := Null;
        Matte_Colour       : Magick_Pixel.Pixel_Info;
    end record;  --  Image_Info

    Core_Image_Exception : Exception;
    --  File_To_Image and Image_To_Blob moved from Magick_Image
    --  to avoid circularities
--      procedure File_To_Image (theImage : in out Image; File_Name : String);
--
--      procedure Image_To_Blob (Info : Image_Info; theImage : in out Image;
--                               Size : in out GL.Types.UInt);
   function To_Image (C_Image : API_Image) return Image;

end Core_Image;
