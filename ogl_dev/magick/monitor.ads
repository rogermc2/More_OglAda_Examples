
with System;

with Interfaces.C;

with Magick_Type; use Magick_Type;

package Monitor is

   type Magick_Progress_Monitor is access function
     (Text : Interfaces.C.char_array; Offset : Magick_Offset_Type;
      Size : Magick_Size_Type; Client_Data : System.Address)
      return Magick_Boolean_Type;
   pragma Convention (C, Magick_Progress_Monitor);

--     Virtual_Pixel_Method_Access : VP_Method_Access;
--     pragma Import (C, Virtual_Pixel_Method_Access, "virtualpixelmethod_ptr");

end Monitor;
