
with Sys_PThread_Mutex;
with Sys_PThread_Types;

package Thread is

package Class_Mutex_Lock is
      type Mutex_Lock is limited record
         Mutex : aliased Sys_PThread_Mutex.PThread_Mutex_t;  -- ../Magick++/lib/Magick++2/Thread.h:52
      end record;
      pragma Import (CPP, Mutex_Lock);

      function New_Mutex_Lock return Mutex_Lock;  -- ../Magick++/lib/Magick++2/Thread.h:32
      pragma Cpp_Constructor (New_Mutex_Lock, "_ZN6Magick9MutexLockC1Ev");

      procedure Delete_Mutex_Lock (this : access Mutex_Lock);  -- ../Magick++/lib/Magick++2/Thread.h:35
      pragma Import (CPP, Delete_Mutex_Lock, "_ZN6Magick9MutexLockD1Ev");

      procedure Lock (this : access Mutex_Lock);  -- ../Magick++/lib/Magick++2/Thread.h:38
      pragma Import (CPP, Lock, "_ZN6Magick9MutexLock4lockEv");

      procedure Unlock (this : access Mutex_Lock);  -- ../Magick++/lib/Magick++2/Thread.h:41
      pragma Import (CPP, Unlock, "_ZN6Magick9MutexLock6unlockEv");

      function Operator_As (this : access Mutex_Lock;
                            Original : access constant Mutex_Lock)
                            return access Mutex_Lock;  -- ../Magick++/lib/Magick++2/Thread.h:49
      pragma Import (Cpp, Operator_As, "_ZN6Magick9MutexLockaSERKS0_");
   end;

   --  MagickMutexType from Thread.Private
   type Magick_Mutex_Type is new Sys_PThread_Mutex.PThread_Mutex_t;
   type Magick_Mutex_Lock is new Sys_PThread_Mutex.PThread_Mutex_t;

   type Magick_Thread_Type is new Sys_PThread_Types.PThread_t;

end Thread;
