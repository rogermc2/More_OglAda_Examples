
with Glfw.Input.Keys;

with Audio;
with Input_Handler;

package body Menu_Support i

   --  -------------------------------------------------------------------------

procedure Process_Menu_Gr (Cursor_Current_Item : Integer := -1) is
      use Glfw.Input.Keys;
      use Input_Handler;
   begin
      case Cursor_Current_Item is
         when 0 => null;
         when 1 => null;
         when 2 => null;
         when 3 => null;
         when 4 => null;
         when 5 => null;
         when 6 => null;
         when 7 => null;
         when 8 => null;
         when 9 => null;
         when 10 => null;
         when 11 => null;
         when 12 => null;
         when 13 => null;
         when 14 => null;
         when 15 => null;
         when 16 => null;
         when others => null;
      end case;

   end Process_Menu_Gr;

end Menu_Support;
