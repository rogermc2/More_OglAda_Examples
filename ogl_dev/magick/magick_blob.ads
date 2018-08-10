
with Ada.Containers.Doubly_Linked_Lists;

with GL.Types;

package Magick_Blob is
   use GL.Types;

   package Blob_Package is new Ada.Containers.Doubly_Linked_Lists (UByte);
   subtype Blob_Data is Blob_Package.List;

end Magick_Blob;
