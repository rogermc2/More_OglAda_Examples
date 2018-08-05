
with Interfaces.C; use Interfaces.C;

with Magick_Pixel;
with Magick_Type;
with Method_Attribute;
with Semaphore;

package Quantum is

   type Endian_Type is (Undefined_Endian,
                        LSB_Endian,
                        MSB_Endian);
   pragma Convention (C, Endian_Type);


   type Quantum_Alpha_Type is (Undefined_Quantum_Alpha,
                               Associated_Quantum_Alpha,
                               Disassociated_Quantum_Alpha);
   pragma Convention (C, Quantum_Alpha_Type);

   type Quantum_Format_Type is (Undefined_Quantum_Format,
                                Floating_Point_Quantum_Format,
                                Signed_Quantum_Format,
                                Unsigned_Quantum_Format);
   pragma Convention (C, Quantum_Format_Type);

   type Quantum_Type is (Undefined_Quantum,
                         Alpha_Quantum,
                         Black_Quantum,
                         Blue_Quantum,
                         CMYKA_Quantum,
                         CMYK_Quantum,
                         Cyan_Quantum,
                         GrayAlpha_Quantum,
                         Gray_Quantum,
                         Green_Quantum,
                         IndexAlpha_Quantum,
                         Index_Quantum,
                         Magenta_Quantum,
                         Opacity_Quantum,
                         Red_Quantum,
                         RGBA_Quantum,
                         BGRA_Quantum,
                         RGBO_Quantum,
                         RGB_Quantum,
                         Yellow_Quantum,
                         GrayPad_Quantum,  --  deprecated
                         RGBPad_Quantum,
                         CbYCrY_Quantum,
                         CbYCr_Quantum,
                         CbYCrA_Quantum,
                         CMYKO_Quantum,
                         BGR_Quantum,
                         BGRO_Quantum);
   pragma Convention (C, Quantum_Type);

 type Quantum_State is record
      Inverse_Scale  : double := 0.0;
      Pixel          : unsigned := 0;
      Bits           : size_t := 0;
      Mask           : access unsigned;
   end record;
   pragma Convention (C, Quantum_State);

 type Quantum_Info is record
      Depth        : size_t := 0;
      Quantum      : size_t := 0;
      Format       : Quantum_Format_Type;
      Minimum      : double := 0.0;
      Maximum      : double := 0.0;
      Scale        : double := 0.0;
      Pad          : size_t := 0;
      Min_Is_White : Magick_Type.Magick_Boolean_Type;
      Pack         : Magick_Type.Magick_Boolean_Type;
      Alpha        : Quantum_Alpha_Type;
      Num_Threads  : size_t := 0;
      Pixels       : access Magick_Pixel.Pixel_Info_Ptr;
      Extent       : size_t := 0;
      Endian       : Endian_Type;
      Q_State      : Quantum_State;
      Sema4        : Semaphore.Sem_Ptr := Null;
      Signature    : size_t :=
        size_t (Method_Attribute.Magick_Core_Signature);
   end record;
   pragma Convention (C, Quantum_Info);

end Quantum;
