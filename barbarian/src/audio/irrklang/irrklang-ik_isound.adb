
package body Irrklang.Ik_Isound is

   Sound_State : ISound;

   function Get_Is_Paused return Boolean is
   begin
      return Sound_State.Paused;
   end Get_Is_Paused;

   --  -------------------------------------------------------------------------


   procedure Set_Is_Paused (Set : Boolean := True) is
   begin
      Sound_State.Paused := Set;
   end Set_Is_Paused;

end Irrklang.Ik_Isound;
