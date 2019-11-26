
with Interfaces.C; use Interfaces.C;
with System;

package Sys_PThread_Types is

   type darwin_pthread_handler_rec_t is record
      routine : access procedure (arg1 : System.Address);  -- /usr/include/sys/_pthread/_pthreadypes.h:58
      arg     : System.Address;  -- /usr/include/sys/_pthread/_pthreadypes.h:59
      next    : access darwin_pthread_handler_rec_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:60
   end record;
   pragma Convention (C_Pass_By_Copy, darwin_pthread_handler_rec_t);  -- /usr/include/sys/_pthread/_pthreadypes.h:57

   subtype opaque_pthread_attr_opaque_array is Interfaces.C.char_array (0 .. 55);
   type opaque_pthread_attr_t is record
      sig    : aliased long := 0;  -- /usr/include/sys/_pthread/_pthreadypes.h:64
      opaque : aliased opaque_pthread_attr_opaque_array;  -- /usr/include/sys/_pthread/_pthreadypes.h:65
   end record;
   pragma Convention (C_Pass_By_Copy, opaque_pthread_attr_t);  -- /usr/include/sys/_pthread/_pthreadypes.h:63

   subtype opaque_pthread_cond_opaque_array is Interfaces.C.char_array (0 .. 39);
   type opaque_pthread_cond_t is record
      sig    : aliased long := 0;  -- /usr/include/sys/_pthread/_pthreadypes.h:69
      opaque : aliased opaque_pthread_cond_opaque_array;  -- /usr/include/sys/_pthread/_pthreadypes.h:70
   end record;
   pragma Convention (C_Pass_By_Copy, opaque_pthread_cond_t);  -- /usr/include/sys/_pthread/_pthreadypes.h:68

   subtype opaque_pthread_condattr_opaque_array is Interfaces.C.char_array (0 .. 7);
   type opaque_pthread_condattr_t is record
      sig    : aliased long := 0;  -- /usr/include/sys/_pthread/_pthreadypes.h:74
      opaque : aliased opaque_pthread_condattr_opaque_array;  -- /usr/include/sys/_pthread/_pthreadypes.h:75
   end record;
   pragma Convention (C_Pass_By_Copy, opaque_pthread_condattr_t);  -- /usr/include/sys/_pthread/_pthreadypes.h:73

   subtype opaque_pthread_mutex_opaque_array is Interfaces.C.char_array (0 .. 55);
   type opaque_pthread_mutex_t is record
      sig    : aliased long := 0;  -- /usr/include/sys/_pthread/_pthreadypes.h:79
      opaque : aliased opaque_pthread_mutex_opaque_array;  -- /usr/include/sys/_pthread/_pthreadypes.h:80
   end record;
   pragma Convention (C_Pass_By_Copy, opaque_pthread_mutex_t);  -- /usr/include/sys/_pthread/_pthreadypes.h:78

   subtype opaque_pthread_mutexattr_opaque_array is Interfaces.C.char_array (0 .. 7);
   type opaque_pthread_mutexattr_t is record
      sig    : aliased long := 0;  -- /usr/include/sys/_pthread/_pthreadypes.h:84
      opaque : aliased opaque_pthread_mutexattr_opaque_array;  -- /usr/include/sys/_pthread/_pthreadypes.h:85
   end record;
   pragma Convention (C_Pass_By_Copy, opaque_pthread_mutexattr_t);  -- /usr/include/sys/_pthread/_pthreadypes.h:83

   subtype opaque_pthread_once_opaque_array is Interfaces.C.char_array (0 .. 7);
   type opaque_pthread_once_t is record
      sig    : aliased long := 0;  -- /usr/include/sys/_pthread/_pthreadypes.h:89
      opaque : aliased opaque_pthread_once_opaque_array;  -- /usr/include/sys/_pthread/_pthreadypes.h:90
   end record;
   pragma Convention (C_Pass_By_Copy, opaque_pthread_once_t);  -- /usr/include/sys/_pthread/_pthreadypes.h:88

   subtype opaque_pthread_rwlock_opaque_array is Interfaces.C.char_array (0 .. 191);
   type opaque_pthread_rwlock_t is record
      sig    : aliased long := 0;  -- /usr/include/sys/_pthread/_pthreadypes.h:94
      opaque : aliased opaque_pthread_rwlock_opaque_array;  -- /usr/include/sys/_pthread/_pthreadypes.h:95
   end record;
   pragma Convention (C_Pass_By_Copy, opaque_pthread_rwlock_t);  -- /usr/include/sys/_pthread/_pthreadypes.h:93

   subtype opaque_pthread_rwlockattr_opaque_array is Interfaces.C.char_array (0 .. 15);
   type opaque_pthread_rwlockattr_t is record
      sig    : aliased long := 0;  -- /usr/include/sys/_pthread/_pthreadypes.h:99
      opaque : aliased opaque_pthread_rwlockattr_opaque_array;  -- /usr/include/sys/_pthread/_pthreadypes.h:100
   end record;
   pragma Convention (C_Pass_By_Copy, opaque_pthread_rwlockattr_t);  -- /usr/include/sys/_pthread/_pthreadypes.h:98

   subtype opaque_pthread_opaque_array is Interfaces.C.char_array (0 .. 8175);
   type opaque_pthread_t is record
      sig           : aliased long := 99;  -- /usr/include/sys/_pthread/_pthreadypes.h:104
      cleanup_stack : access darwin_pthread_handler_rec_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:105
      opaque        : aliased opaque_pthread_opaque_array;  -- /usr/include/sys/_pthread/_pthreadypes.h:106
   end record;
   pragma Convention (C_Pass_By_Copy, opaque_pthread_t);  -- /usr/include/sys/_pthread/_pthreadypes.h:103

   subtype darwin_pthread_attr_t is opaque_pthread_attr_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:109

   subtype darwin_pthread_cond_t is opaque_pthread_cond_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:110

   subtype darwin_pthread_condattr_t is opaque_pthread_condattr_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:111

   subtype darwin_pthread_key_t is unsigned_long;  -- /usr/include/sys/_pthread/_pthreadypes.h:112

   subtype darwin_pthread_mutex_t is opaque_pthread_mutex_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:113

   subtype darwin_pthread_mutexattr_t is opaque_pthread_mutexattr_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:114

   subtype darwin_pthread_once_t is opaque_pthread_once_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:115

   subtype darwin_pthread_rwlock_t is opaque_pthread_rwlock_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:116

   subtype darwin_pthread_rwlockattr_t is opaque_pthread_rwlockattr_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:117

   type darwin_pthread_t is access all opaque_pthread_t;  -- /usr/include/sys/_pthread/_pthreadypes.h:118
   subtype PThread_t is darwin_pthread_t;

end Sys_PThread_Types;
