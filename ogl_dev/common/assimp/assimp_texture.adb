
package body Assimp_Texture is


   function Texture_Map_Size (theMap : AI_Texture_Map) return GL.Types.UInt is
   begin
      return GL.Types.UInt (theMap.Length);
   end Texture_Map_Size;

   --  -------------------------------------------------------------------------

   function To_AI_Texel_Map (Num_Texels : UInt; Data_Ptr : Texel_Array_Pointers.Pointer)
                             return AI_Texel_Map is
       Texel_Array : constant API_Texel_Array
                  := Texel_Array_Pointers.Value
                    (Data_Ptr, ptrdiff_t (Num_Texels));
      C_Texel  : API_Texel;
      aTexel   : AI_Texel;
      Texels   : AI_Texel_Map;
   begin
      for index in UInt range 1 .. Num_Texels loop
         C_Texel := Texel_Array (Interfaces.C.unsigned (index));
         aTexel.B := UInt (C_Texel.B);
         aTexel.G := UInt (C_Texel.G);
         aTexel.R := UInt (C_Texel.R);
         aTexel.A := UInt (C_Texel.A);
         Texels.Insert (index, aTexel);
      end loop;
      return Texels;
   end To_AI_Texel_Map;

   --  -------------------------------------------------------------------------

   function To_AI_Texture_Map (Num_Textures : Interfaces.C.unsigned := 0;
                               C_Ptrs_Array : Texture_Ptr_Array_Pointer)
                               return AI_Texture_Map is
      use Texture_Ptr_Array_Pointers;
      Ptrs_Array : API_Texture_Ptr_Array :=
          Value (C_Ptrs_Array, ptrdiff_t (Num_Textures));
      C_Texture : API_Texture;
      aTexture  : AI_Texture;
      Tex_Map   : AI_Texture_Map;
   begin
      for index in 1 .. Num_Textures loop
         C_Texture := Ptrs_Array (index).all;
         aTexture.Width := UInt (C_Texture.Width);
         aTexture.Height := UInt (C_Texture.Height);
         aTexture.Format_Hint :=
           Hint_String (To_Ada (C_Texture.Ach_Format_Hint));
         aTexture.PC_Data :=
           To_AI_Texel_Map (aTexture.Width * aTexture.Height, C_Texture.PC_Data_Ptr);
         Tex_Map.Insert (UInt (index), aTexture);
      end loop;
      return Tex_Map;
   end To_AI_Texture_Map;

end Assimp_Texture;
