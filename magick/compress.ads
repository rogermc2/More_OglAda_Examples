
package Compress is

  type Compression_Type is (Undefined_Compression,
                            No_Compression,
                            BZip_Compression,
                            DXT1_Compression,
                            DXT3_Compression,
                            DXT5_Compression,
                            Fax_Compression,
                            Group4_Compression,
                            JPEG_Compression,
                            JPEG_2000_Compression,      --  ISO/IEC std 15444-1
                            Lossless_JPEG_Compression,
                            LZW_Compression,
                            RLE_Compression,
                            Zip_Compression,
                            ZipS_Compression,
                            Piz_Compression,
                            Pxr24_Compression,
                            B44_Compression,
                            B44A_Compression,
                            LZMA_Compression,          --  Lempel-Ziv-Markov chain algorithm
                            JBIG1_Compression,         --  ISO/IEC std 11544 / ITU-T rec T.82
                            JBIG2_Compression);
   pragma Convention (C, Compression_Type);

   type Ascii_85_Info is record
      null;
   end record;
   pragma Convention (C_Pass_By_Copy, Ascii_85_Info);

end Compress;
