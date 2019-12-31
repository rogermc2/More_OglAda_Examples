
with System;

with Interfaces.C;

with GL.Types;

with Assimp_Types;

package Metadata is

   type AI_Metadata_Type is (AI_Bool, AI_Int_32, AI_Int_64, AI_Float,
                             AI_Double, AI_AI_String, AI_AI_Vector_3D);
   pragma Convention (C, AI_Metadata_Type);

    type AI_Metadata is record
      Num_Properties : GL.Types.UInt := 0;
--        Keys           : access Assimp_Types.AI_String;
--        Values         : access AI_Metadata_Entry;
   end record;

 type API_Metadata_Entry is record
      Metadata_Type : AI_Metadata_Type;
      Data          : System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Metadata_Entry);

    type API_Metadata is record
      Num_Properties : Interfaces.C.unsigned := 0;
      Keys           : access Assimp_Types.API_String;
      Values         : access API_Metadata_Entry;
    end record;
   pragma Convention (C_Pass_By_Copy, API_Metadata);

private

   for AI_Metadata_Type use (AI_Bool         => 0,
                             AI_Int_32       => 1,
                             AI_Int_64       => 2,
                             AI_Float        => 3,
                             AI_Double       => 4,
                             AI_AI_String    => 5,
                             AI_AI_Vector_3D => 6);

end Metadata;
