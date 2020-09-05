
package Game_Utils is
   --  Check_Param checks for given parameter in main's command - line arguments
   --  returns the argument number if present (1 to argc - 1)
   --  otherwise returns 0
   function Check_Param (Check : String) return Integer;
   procedure Game_Log (Message : String);
   function Max (L, R : Integer) return Integer;

end Game_Utils;
