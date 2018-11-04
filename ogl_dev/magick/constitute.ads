
with Core_Image;

package Constitute is

   type Storage_Type is (Undefined_Pixel, Char_Pixel, Double_Pixel, Float_Pixel,
                         Integer_Pixel, Long_Pixel, Quantum_Pixel, Short_Pixel);
   pragma Convention (C, Storage_Type);

   Constitute_Exception : Exception;

   procedure Read_Image (theImage : in out Core_Image.API_Image;
                         Info : access Core_Image.API_Image_Info);
   procedure Read_Image (theImage  : in out Core_Image.API_Image;
                         File_Name : String);
   procedure Write_Image (theImage : Core_Image.API_Image;
                          Info : in out Core_Image.API_Image_Info);

end Constitute;
