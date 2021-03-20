
package Irrklang.Ik_Isound is

   type ISound is record
      Paused : Boolean := True;
   end record;

   function Get_Is_Paused return Boolean;
   procedure Set_Is_Paused (Set : Boolean := True);

end Irrklang.Ik_Isound;
