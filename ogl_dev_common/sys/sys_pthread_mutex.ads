
with Sys_PThread_Types;

package Sys_PThread_Mutex is

   subtype PThread_Mutex_t is Sys_PThread_Types.opaque_pthread_mutex_t;
   -- /usr/include/sys/_pthread/_pthread_mutex_t.h:31

end Sys_PThread_Mutex;
