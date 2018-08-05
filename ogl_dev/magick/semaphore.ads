
with Interfaces.C;

with Magick_Type;
with Method_Attribute;
with Thread;

package Semaphore is

   type Semaphore_Info is private;
   type Sem_Ptr is access all Semaphore_Info;

   function Acquire_Semaphore_Info return Sem_Ptr;
   pragma Import (C, Acquire_Semaphore_Info, "AcquireSemaphoreInfo");

private

   --  From Semaphore.C
   type Semaphore_Info is record
      Mutex     : Thread.Magick_Mutex_Type;
      ID        : Thread.Magick_Thread_Type;
      Ref_Count : Magick_Type.ssize_t := 0;
      Signature : Interfaces.C.size_t :=
        Interfaces.C.size_t (Method_Attribute.Magick_Core_Signature);
   end record;
   pragma Convention (C_Pass_By_Copy, Semaphore_Info);

end Semaphore;
