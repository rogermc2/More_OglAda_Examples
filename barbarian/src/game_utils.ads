
package Game_Utils is
   --  Check_Param checks for given parameter in main's command - line arguments
   --  returns the argument number if present (1 to argc - 1)
   --  otherwise returns 0
--     function Check_Param (Check : String) return Integer;
   procedure Close_Game_Log;
   procedure Game_Log (Message : String);
   function Loop_I (Val, Min, Max : Integer) return Integer;
   function Max (L, R : Integer) return Integer;
   procedure Restart_Game_Log;
   procedure Set_Started_Game_With_Map_Param (State : Boolean);
   function Started_Game_With_Map_Param return Boolean;

end Game_Utils;
