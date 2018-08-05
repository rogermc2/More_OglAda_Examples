
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Colour;
with Composite;
with Geometry;
with Core_Image;
with Magick_Pixel;
with Magick_Type;
with Method_Attribute;

package Draw is

   type Align_Type is (Undefined_Align, LeftAlign,
                       Center_Align, Right_Align);
   pragma Convention (C, Align_Type);

   type Clip_Path_Units is (Undefined_Path_Units, User_Space,
                            User_Space_On_Use, Object_Bounding_Box);
   pragma Convention (C, Clip_Path_Units);

   type Decoration_Type is (Undefined_Decoration, No_Decoration,
                            Underline_Decoration, Overline_Decoration,
                            Line_Through_Decoration);
   pragma Convention (C, Decoration_Type);

   type Direction_Type is (Undefined_Direction,
                           Right_To_Left_Direction, Left_To_Right_Direction);
   pragma Convention (C, Direction_Type);

   type Fill_Rule is (Undefined_Rule, Even_Odd_Rule, Nonzero_Rule);
   pragma Convention (C, Fill_Rule);

   type Gradient_Type is (Undefined_Gradient,
                          Linear_Gradient, Radial_Gradient);
   pragma Convention (C, Gradient_Type);

   type Line_Cap is (Undefined_Cap, ButtCap,
                     Round_Cap, Square_Cap);
   pragma Convention (C, Line_Cap);

   type Line_Join is (Undefined_Join, Mitre_Join,
                      Round_Join, Bevel_Join);
   pragma Convention (C, Line_Join);

   type Paint_Method is (Undefined_Method, Point_Method, Replace_Method,
                         Floodfill_Method, Fill_To_Border_Method, Reset_Method);
   pragma Convention (C, Paint_Method);

   type Reference_Type is (Undefined_Reference, Gradient_Reference);
   pragma Convention (C, Reference_Type);

   type Spread_Method is (Undefined_Spread, Pad_Spread,
                          Reflect_Spread, Repeat_Spread);
   pragma Convention (C, Spread_Method);

   type Element_Reference is record
      ID        : chars_ptr;
      Ref_Type  : Reference_Type;
      Gradient  : Gradient_Type;
      Previous  : access Element_Reference;
      Next      : access Element_Reference;
      Signature : size_t := size_t (Method_Attribute.Magick_Core_Signature);
   end record;
   pragma Convention (C_Pass_By_Copy, Element_Reference);

   type Stop_Info is record
      Colour    : Magick_Pixel.Pixel_Info;
      Offset    : double := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, Stop_Info);

   type Gradient_Info is record
      Grad_Type    : Gradient_Type;
      Bounding_Box : Geometry.Rectangle_Info;
      Grad_Vector  : Core_Image.Segment_Info;
      Stop         : Stop_Info;
      Num_Stops    : size_t := 0;
      Spread       : Spread_Method;
      Debug        : Magick_Type.Magick_Boolean_Type := Magick_Type.Magic_False;
      Centre       : Geometry.Point_Info;
      Radii        : Geometry.Point_Info;
      Radius       : double := 0.0;
      Angle        : double := 0.0;
      Signature    : size_t := size_t (Method_Attribute.Magick_Core_Signature);
   end record;
   pragma Convention (C_Pass_By_Copy, Gradient_Info);

   type Draw_Info is record
      Primitive         : chars_ptr := New_String ("P");
      Geom              : chars_ptr := New_String ("G");
      View_Box          : Geometry.Rectangle_Info;
      Affine            : Geometry.Affine_Matrix;
      Fill              : Magick_Pixel.Pixel_Info;
      Stroke            : Magick_Pixel.Pixel_Info;
      Under_Colour      : Magick_Pixel.Pixel_Info;
      Border_Colour     : Magick_Pixel.Pixel_Info;
      Fill_Pattern      : access Core_Image.Image := Null;
      Stroke_Pattern    : access Core_Image.Image := Null;
      Stroke_Width      : double := 999.0;
      Gradient          : Gradient_Info;
      Stroke_Antialias  : Magick_Type.Magick_Boolean_Type :=
        Magick_Type.Magic_False;
      Text_Antialias    : Magick_Type.Magick_Boolean_Type :=
        Magick_Type.Magic_False;
      Fill_Method       : Fill_Rule;
      L_Cap             : Line_Cap;
      L_Join            : Line_Join;
      Mitre_Limit       : size_t := 0;
      Dash_Offset       : double := 0.0;
      Decorate          : Decoration_Type;
      Compose           : Composite.Composite_Operator;
      Text              : chars_ptr := New_String ("\0");
      Font              : chars_ptr := New_String ("\0");
      Metrics           : chars_ptr := New_String ("\0");
      Family            : chars_ptr := New_String ("\0");
      Face              : size_t := 0;
      Style             : Magick_Type.Style_Type;
      Stretch           : Magick_Type.Stretch_Type;
      Weight            : size_t := 0;
      Encoding          : chars_ptr := New_String ("\0");
      Point_Size        : double := 0.0;
      Density           : chars_ptr := New_String ("\0");
      Align             : Align_Type;
      Gravity           : Geometry.Gravity_Type;
      Server_Name       : chars_ptr;
      Dash_Pattern      : access double := Null;
      Clip_Mask         : chars_ptr := New_String ("\0");
      Bounds            : Core_Image.Segment_Info;
      Clip_Units        : Clip_Path_Units;
      Alpha             : Magick_Type.Quantum;
      Render            : Magick_Type.Magick_Boolean_Type :=
        Magick_Type.Magic_False;
      Element_Ref       : Element_Reference;
      Kerning           : double := 0.0;
      Interword_Spacing : double := 0.0;
      Interline_Spacing : double := 0.0;
      Direction         : Direction_Type;
      Debug             : Magick_Type.Magick_Boolean_Type :=
        Magick_Type.Magic_False;
      Signature         : size_t :=
        size_t (Method_Attribute.Magick_Core_Signature);
      Fill_Alpha        : double := 0.0;
      Stroke_Alpha      : double := 0.0;
      Clip_Path         : Magick_Type.Magick_Boolean_Type :=
        Magick_Type.Magic_False;
      Clipping_Mask     : access Core_Image.Image;
      Compliance        : Colour.Compliance_Type;
      Composite_Mask    : access Core_Image.Image;
   end record;
   pragma Convention (C_Pass_By_Copy, Draw_Info);

end Draw;
