
with Interfaces.C; use Interfaces.C;

package Timer is

   type Timer_State is (Undefined_Timer_State,
                        Stopped_Timer_State,
                        Running_Timer_State);
   pragma Convention (C, Timer_State);

   type Timer is record
      Start  : double := 0.0;
      Stop   : double := 0.0;
      Total  : double := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, Timer);

   type Timer_Info is record
      User      : Timer;
      Elapsed   : Timer;
      State     : Timer_State := Undefined_Timer_State;
      Signature : size_t := 0;
   end record;
   pragma Convention (C_Pass_By_Copy, Timer_Info);

   --  GetTimerInfo() initializes the TimerInfo structure.
   procedure Get_Timer_Info (Info : access Timer_Info);
   pragma Import (C, Get_Timer_Info, "GetTimerInfo");

end Timer;
