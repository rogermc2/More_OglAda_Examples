
package Colour_Space is

  type Colourspace_Type is (Undefined_Colourspace,
                            RGB_Colourspace,
                            GRAY_Colourspace,
                            Transparent_Colourspace,
                            OHTA_Colourspace,
                            Lab_Colourspace,
                            XYZ_Colourspace,
                            YCbCr_Colourspace,
                            YCC_Colourspace,
                            YIQ_Colourspace,
                            YPbPr_Colourspace,
                            YUV_Colourspace,
                            CMYK_Colourspace,
                            sRGB_Colourspace,
                            HSB_Colourspace,
                            HSL_Colourspace,
                            HWB_Colourspace,
                            Rec_601_Luma_Colourspace,
                            Rec_601_YCbCr_Colourspace,
                            Rec_709_Luma_Colourspace,
                            Rec_709_YCbCr_Colourspace,
                            Log_Colourspace,
                            CMY_Colourspace,
                            Luv_Colourspace,
                            HCL_Colourspace,
                            LCH_Colourspace,
                            LMS_Colourspace);
   pragma Convention (C, Colourspace_Type);

end Colour_Space;
