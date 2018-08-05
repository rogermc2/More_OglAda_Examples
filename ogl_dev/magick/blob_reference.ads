
with System;

with Interfaces.C.Strings;

with Core_Blob;
with Magick_Blob;
limited with Magick_Blob.API;
with Magick_Type;
with Thread;

package Blob_Reference is

   package Class_Blob_Ref is

      type Blob_Ref is limited record
         theAllocator : aliased Magick_Blob.Allocator;  -- ../Magick++/lib/Magick++/BlobRef.h:37
         Length       : aliased Interfaces.C.size_t;  -- ../Magick++/lib/Magick++/BlobRef.h:38
         Data         : System.Address;  -- ../Magick++/lib/Magick++/BlobRef.h:39
         Mutex_Lock   : aliased Thread.Class_Mutex_Lock.Mutex_Lock;  -- ../Magick++/lib/Magick++/BlobRef.h:46
         Ref_Count    : aliased Interfaces.C.size_t;  -- ../Magick++/lib/Magick++/BlobRef.h:47
      end record;
      pragma Import (Cpp, Blob_Ref);

      --  DO NOT USE, default constructor not implemented.
      --  Defined here only to prevent compiler warning message
      function New_Blob_Ref return Blob_Ref;
      pragma Cpp_Constructor (New_Blob_Ref, "_ZN6Magick7BlobRefC1EPKvm");

      function New_Blob_Ref (Data : System.Address; Length : Interfaces.C.size_t) return Blob_Ref;  -- ../Magick++/lib/Magick++/BlobRef.h:26
      pragma Cpp_Constructor (New_Blob_Ref, "_ZN6Magick7BlobRefC1EPKvm");

      procedure Delete_Blob_Ref (this : access Blob_Ref);
      pragma Import (CPP, Delete_Blob_Ref, "_ZN6Magick7BlobRefD1Ev");

      --  Decreases reference count and return the new count
      function Decrease (this : access Blob_Ref) return Interfaces.C.size_t;  -- ../Magick++/lib/Magick++/BlobRef.h:32
      pragma Import (CPP, decrease, "_ZN6Magick7BlobRef8decreaseEv");

      --  Increases reference count
      procedure increase (this : access Blob_Ref);  -- ../Magick++/lib/Magick++/BlobRef.h:35
      pragma Import (CPP, increase, "_ZN6Magick7BlobRef8increaseEv");

      function Operator_As (this : access Blob_Ref; arg2 : access constant Blob_Ref) return access Blob_Ref;  -- ../Magick++/lib/Magick++/BlobRef.h:44
      pragma Import (CPP, Operator_As, "_ZN6Magick7BlobRefaSERKS0_");

   end Class_Blob_Ref;

   type Blob_Ref_Ptr is access all Class_Blob_Ref.Blob_Ref;

end Blob_Reference;
