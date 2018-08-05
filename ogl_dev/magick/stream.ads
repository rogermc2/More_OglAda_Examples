
with System;

with Interfaces.C; use Interfaces.C;

limited with Core_Image;

package Stream is

   type Stream_Handler is
     access function (anImage : access Core_Image.AI_Image; arg2 : System.Address;
                      arg3 : size_t) return size_t;
   pragma Convention (C, Stream_Handler);

   --   Stream_Info declared in Image because ofcircularities.

   --     function Read_Stream (Info : access Image.Image_Info;
   --                           Handler : Stream_Handler;
   --                           Except : access Magick_Exception.Exception_Info)
   --                           return access Image.Image;
   --
   --     function Write_Stream (Info : access Image.Image_Info;
   --                            anImage : access Image.Image;
   --                            Handler : Stream_Handler)
   --                            return Magick_Type.Magick_Boolean_Type;

end Stream;
