
package body Core_Blob is

   procedure Set_Blob_Type (Info : in out Blob_Info; theType : Stream_Type) is
   begin
      Info.Stream_Kind := theType;
   end Set_Blob_Type;

end Core_Blob;
