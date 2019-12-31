
package body Magick_Pixel is

   function To_AI_Pixel_Channel_Map (theMap : Pixel_Channel_Map)
                                     return AI_Pixel_Channel_Map is
      AI_Map : AI_Pixel_Channel_Map;
   begin
      AI_Map.Channel := theMap.Channel;
      AI_Map.Traits := theMap.Traits;
      AI_Map.Offset := double (theMap.Offset);
      return AI_Map;
   end To_AI_Pixel_Channel_Map;

   --  -------------------------------------------------------------------------

   function To_AI_Pixel_Info (Pix : Pixel_Info) return AI_Pixel_Info is
      AI_Pix : AI_Pixel_Info;
   begin
      AI_Pix.Storage_Class      := Pix.Storage_Class;
      AI_Pix.Colourspace        := Pix.Colourspace;
      AI_Pix.Alpha_Trait        := Pix.Alpha_Trait;
      AI_Pix.Fuzz               := double (Pix.Fuzz);
      AI_Pix.Depth              := size_t (Pix.Depth);
      AI_Pix.Count              := Magick_Size_Type (Pix.Count);
      AI_Pix.Red                := Magick_Real_Type (Pix.Red);
      AI_Pix.Green              := Magick_Real_Type (Pix.Green);
      AI_Pix.Blue               := Magick_Real_Type (Pix.Blue);
      AI_Pix.Black              := Magick_Real_Type (Pix.Black);
      AI_Pix.Alpha              := Magick_Real_Type (Pix.Alpha);
      AI_Pix.Index              := Magick_Real_Type (Pix.Index);
      return AI_Pix;
   end To_AI_Pixel_Info;

  --  -------------------------------------------------------------------------

   function To_Pixel_Info (Pix : AI_Pixel_Info) return Pixel_Info is
      use GL.Types;
      Info : Pixel_Info;
   begin
      Info.Storage_Class      := Pix.Storage_Class;
      Info.Colourspace        := Pix.Colourspace;
      Info.Alpha_Trait        := Pix.Alpha_Trait;
      Info.Fuzz               := Single (Pix.Fuzz);
      Info.Depth              := UInt (Pix.Depth);
      Info.Count              := UInt (Pix.Count);
      Info.Red                := Single (Pix.Red);
      Info.Green              := Single (Pix.Green);
      Info.Blue               := Single (Pix.Blue);
      Info.Black              := Single (Pix.Black);
      Info.Alpha              := Single (Pix.Alpha);
      Info.Index              := Single (Pix.Index);
      return Info;
   end To_Pixel_Info;

end Magick_Pixel;
