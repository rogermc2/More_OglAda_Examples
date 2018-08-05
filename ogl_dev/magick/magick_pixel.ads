
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;

with GL.Types;

with Colour_Space;
with Magick_Type; use Magick_Type;

package Magick_Pixel is

   Max_Pixel_Channels : constant unsigned := 64;

   type Pixel_Channel is (Undefined_Pixel_Channel,
                          A_Pixel_Channel,
                          B_Pixel_Channel,
                          Black_Pixel_Channel,
                          Alpha_Pixel_Channel,
                          Index_Pixel_Channel,
                          Read_Mask_Pixel_Channel,
                          Write_Mask_Pixel_Channel,
                          Meta_Pixel_Channel,
                          Intensity_Pixel_Channel,
                          Sync_Pixel_Channel
                         );
   pragma Convention (C, Pixel_Channel);

   type Pixel_Interpolate_Method is (Undefined_Interpolate_Pixel,
                                     Average_Interpolate_Pixel,          --  Average 4 nearest neighbours
                                     Average9_Interpolate_Pixel,          --  Average 9 nearest neighbours
                                     Average16_Interpolate_Pixel,         --  blend of nearest 1, 2 or 4 pixels
                                     Background_Interpolate_Pixel,
                                     Bilinear_Interpolate_Pixel,         --  Triangular filter interpolation
                                     Blend_Interpolate_Pixel,
                                     Catrom_Interpolate_Pixel,             --  Catmull-Rom interpolation
                                     Integer_Interpolate_Pixel,           --  Integer (floor) interpolation
                                     Mesh_Interpolate_Pixel,              --  Triangular mesh interpolation
                                     Nearest_Neighbor_Interpolate_Pixel,  --  Nearest neighbour only
                                     Spline_Interpolate_Pixel             --  Cubic Spline (blurred) interpolation
                                     --  Filter_Interpolate_Pixel,        --  Use resize filter - (very slow)
                                    );
   pragma Convention (C, Pixel_Interpolate_Method);

   type Pixel_Intensity_Method is (Undefined_Pixel_Intensity_Method,
                                   Average_Pixel_Intensity_Method,
                                   Brightness_Pixel_Intensity_Method,
                                   Lightness_Pixel_Intensity_Method,
                                   MS_Pixel_Intensity_Method,
                                   Rec_601_Luma_Pixel_Intensity_Method,
                                   Rec_601_Luminance_Pixel_Intensity_Method,
                                   Rec_709_Luma_Pixel_Intensity_Method,
                                   Rec_709_Luminance_Pixel_Intensity_Method,
                                   RMS_Pixel_Intensity_Method);
   pragma Convention (C, Pixel_Intensity_Method);

   type Pixel_Trait is (Undefined_Pixel_Trait, Copy_Pixel_Trait,
                        Update_Pixel_Trait, Blend_Pixel_Trait);
   pragma Convention (C, Pixel_Trait);

   for Pixel_Trait use (Undefined_Pixel_Trait => 16#000000#,
                        Copy_Pixel_Trait      => 16#000001#,
                        Update_Pixel_Trait    => 16#000002#,
                        Blend_Pixel_Trait     => 16#000004#);

   type Storage_Type is (Undefined_Pixel, Char_Pixel,
                         Double_Pixel,  Float_Pixel,
                         Long_Pixel, Long_Long_Pixel,
                         Quantum_Pixel, Short_Pixel);
   pragma Convention (C, Storage_Type);

   type AI_Pixel_Channel_Map is record
      Channel  : Pixel_Channel;
      Traits   : Pixel_Trait;
      Offset   : double := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, AI_Pixel_Channel_Map);
   type Chan_Map_Ptr is access all AI_Pixel_Channel_Map;

   type Pixel_Channel_Map is record
      Channel  : Pixel_Channel;
      Traits   : Pixel_Trait;
      Offset   : GL.Types.Single := 0.0;
   end record;

   type AI_Pixel_Info is record
      Storage_Class      : Magick_Type.Class_Type := Magick_Type.Undefined_Class;
      Colourspace        : Colour_Space.Colourspace_Type :=
        Colour_Space.Undefined_Colourspace;
      Alpha_Trait        : Pixel_Trait := Undefined_Pixel_Trait;
      Fuzz               : double := 0.0;
      Depth              : size_t := 0;
      Count              : Magick_Size_Type := 0;
      Red                : Magick_Real_Type := 0.0;
      Green              : Magick_Real_Type := 0.0;
      Blue               : Magick_Real_Type := 0.0;
      Black              : Magick_Real_Type := 0.0;
      Alpha              : Magick_Real_Type := 0.0;
      Index              : Magick_Real_Type := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, AI_Pixel_Info);

   type Pixel_Info is record
      Storage_Class      : Magick_Type.Class_Type := Magick_Type.Undefined_Class;
      Colourspace        : Colour_Space.Colourspace_Type :=
        Colour_Space.Undefined_Colourspace;
      Alpha_Trait        : Pixel_Trait := Undefined_Pixel_Trait;
      Fuzz               : GL.Types.Single := 0.0;
      Depth              : GL.Types.UInt := 0;
      Count              : GL.Types.UInt := 0;
      Red                : GL.Types.Single := 0.0;
      Green              : GL.Types.Single := 0.0;
      Blue               : GL.Types.Single := 0.0;
      Black              : GL.Types.Single := 0.0;
      Alpha              : GL.Types.Single := 0.0;
      Index              : GL.Types.Single := 0.0;
   end record;

   type Pixel_Array is array (unsigned range <>) of aliased Pixel_Info;
   package Pixel_Package is new Interfaces.C.Pointers
     (unsigned, Pixel_Info, Pixel_Array, Pixel_Info'(others => <>));
   type Pixel_Info_Ptr is new Pixel_Package.Pointer;

   type Pixel_Packet is record
      Red     : Magick_Type.Quantum;
      Green   : Magick_Type.Quantum;
      Blue    : Magick_Type.Quantum;
      Opacity : Magick_Type.Quantum;
   end record;
   pragma Convention (C_Pass_By_Copy, Pixel_Packet);

   type Quantum_Pixel_Packet is record
      Red     : Magick_Type.Quantum;
      Green   : Magick_Type.Quantum;
      Blue    : Magick_Type.Quantum;
      Opacity : Magick_Type.Quantum;
      Index   : Magick_Type.Quantum;
   end record;
   pragma Convention (C_Pass_By_Copy, Quantum_Pixel_Packet);

   function Red_Pixel_Channel return Pixel_Channel renames Undefined_Pixel_Channel;
   function Cyan_Pixel_Channel return Pixel_Channel renames Undefined_Pixel_Channel;
   function Gray_Pixel_Channel return Pixel_Channel renames Undefined_Pixel_Channel;
   function L_Pixel_Channel return Pixel_Channel renames Undefined_Pixel_Channel;
   function Label_Pixel_Channel return Pixel_Channel renames Undefined_Pixel_Channel;
   function Y_Pixel_Channel return Pixel_Channel renames Undefined_Pixel_Channel;
   function Green_Pixel_Channel return Pixel_Channel renames A_Pixel_Channel;
   function Magenta_Pixel_Channel return Pixel_Channel renames A_Pixel_Channel;
   function Cb_Pixel_Channel return Pixel_Channel renames A_Pixel_Channel;
   function Blue_Pixel_Channel return Pixel_Channel renames B_Pixel_Channel;
   function Yellow_Pixel_Channel return Pixel_Channel renames B_Pixel_Channel;
   function Cr_Pixel_Channel return Pixel_Channel renames B_Pixel_Channel;

   function To_AI_Pixel_Info (Pix : Pixel_Info) return AI_Pixel_Info;
   function To_AI_Pixel_Channel_Map (theMap : Pixel_Channel_Map)
                                     return AI_Pixel_Channel_Map;
   function To_Pixel_Info (Pix : AI_Pixel_Info) return Pixel_Info;

   function Acquire_Pixel_Channel_Map return Chan_Map_Ptr;
   pragma Import (C, Acquire_Pixel_Channel_Map, "AcquirePixelChannelMap");

private

   for Pixel_Channel use (Undefined_Pixel_Channel  => 0,
                          A_Pixel_Channel          => 1,
                          B_Pixel_Channel          => 2,
                          Black_Pixel_Channel      => 3,
                          Alpha_Pixel_Channel      => 4,
                          Index_Pixel_Channel      => 5,
                          Read_Mask_Pixel_Channel  => 6,
                          Write_Mask_Pixel_Channel => 7,
                          Meta_Pixel_Channel       => 8,
                          Intensity_Pixel_Channel  => Max_Pixel_Channels,
                          Sync_Pixel_Channel       => Max_Pixel_Channels + 1
                         );

end Magick_Pixel;
