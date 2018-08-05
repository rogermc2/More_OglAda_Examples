
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package Magick_Type is

   Quantum_Range : constant double := double'Last - 1.0;
   Magickcore_Quantum_Depth : size_t := 16;

   type Magick_Boolean_Type is (Magic_False, Magic_True);
   pragma Convention (C, Magick_Boolean_Type);

   type bool is (Magic_False, Magic_True);
   pragma Convention (C, bool);

   type ssize_t is new Long_Integer;
   subtype Magick_Offset_Type is Long_Long_Integer;
   subtype Magick_Real_Type is double;
   subtype Magick_Size_Type is size_t;
   subtype Quantum is double range 0.0 .. Quantum_Range;

   type Class_Type is (Undefined_Class,
                       Bilevel_Type,
                       Direct_Class,
                       Pseudo_Class);
   pragma Convention (C, Class_Type);

   type Channel_Type is (Undefined_Channel,
                         Red_Channel,
                         Green_Channel,
                         Blue_Channel,
                         Alpha_Channel,
                         Black_Channel,
                         Composite_Channels,

                         --  Special purpose _Channel types.
                         True_Alpha_Channel, --  extract actual alpha _Channel from opacity */
                         RGB_Channels,      --  set alpha from  grayscale mask in RGB */
                         Sync_Channels,     --  _Channels should be modified equally */
                         Default_Channels,
                         All_Channels);
   pragma Convention (C, Channel_Type);

   type Stretch_Type is (Undefined_Stretch, Normal_Stretch,
                         Ultra_Condensed_Stretch, Extra_Condensed_Stretch,
                         Condensed_Stretch, SemiCondensed_Stretch,
                         Semi_Expanded_Stretch, Expanded_Stretch,
                         Extra_Expanded_Stretch, Ultra_Expanded_Stretch,
                         Any_Stretch);
   pragma Convention (C, Stretch_Type);  -- ../type.h:38

   type Style_Type is (Undefined_Style, Normal_Style, Italic_Style,
                       Oblique_Style, Any_Style);
   pragma Convention (C, Style_Type);  -- ../type.h:47

   type Type_Info is record
      Path        : Interfaces.C.Strings.chars_ptr;  -- ../type.h:55
      Name        : Interfaces.C.Strings.chars_ptr;  -- ../type.h:56
      Description : Interfaces.C.Strings.chars_ptr;  -- ../type.h:57
      Family      : Interfaces.C.Strings.chars_ptr;  -- ../type.h:58
      Style       : Style_Type;                      -- ../type.h:61
      Stretch     : Stretch_Type;                    -- ../type.h:64
      Encoding    : Interfaces.C.Strings.chars_ptr;  -- ../type.h:70
      Foundry     : Interfaces.C.Strings.chars_ptr;  -- ../type.h:71
      Format      : Interfaces.C.Strings.chars_ptr;  -- ../type.h:72
      Metrics     : Interfaces.C.Strings.chars_ptr;  -- ../type.h:73
      Glyphs      : Interfaces.C.Strings.chars_ptr;  -- ../type.h:74
      Previous    : access Type_Info;                -- ../type.h:80
      Next        : access Type_Info;                -- ../type.h:81
   end record;
   pragma Convention (C_Pass_By_Copy, Type_Info);    -- ../type.h:49

   function To_Boolean (Bool : Magick_Boolean_Type) return Boolean;
   function To_Magick_Boolean (Bool : Boolean) return Magick_Boolean_Type;

private

   for Class_Type use (Undefined_Class => 16#0000#,
                       Bilevel_Type    => 16#0001#,
                       Direct_Class    => 16#0002#,
                       Pseudo_Class    => 16#0003#);

   for Magick_Boolean_Type use (Magic_False => 16#0000#,
                                Magic_True  => 16#0001#);

   for Channel_Type use (Undefined_Channel  => 16#0000#,
                         Red_Channel        => 16#0001#,
                         Green_Channel      => 16#0002#,
                         Blue_Channel       => 16#0004#,
                         Alpha_Channel      => 16#0008#,
                         Black_Channel      => 16#0020#,
                         Composite_Channels => 16#002F#,
                         True_Alpha_Channel => 16#0040#,
                         RGB_Channels       => 16#0080#,
                         Sync_Channels      => 16#0100#,

                         --  Default_Channels is (All_Channels or Sync_Channels)
                         --  and not Opacity_Channel
                         Default_Channels   => 16#7FFFFF7#,
                         All_Channels       => 16#7FFFFFF#);

   function Gray_Channel return Channel_Type renames Red_Channel;
   function Cyan_Channel return Channel_Type renames Red_Channel;
   function Magenta_Channel return Channel_Type renames Green_Channel;
   function Yellow_Channel return Channel_Type renames Blue_Channel;
   function Opacity_Channel return Channel_Type renames Alpha_Channel;
   function Matte_Channel return Channel_Type renames Alpha_Channel;
   function Index_Channel return Channel_Type renames Black_Channel;
   function Gray_Channels return Channel_Type renames RGB_Channels;

end Magick_Type;
