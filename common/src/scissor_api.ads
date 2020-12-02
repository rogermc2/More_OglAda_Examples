
with GL.Types; use GL.Types;

package Scissor_API is

   procedure Set_Scissor_Rectangle (X, Y : Int; Width, Height : Size);
   pragma Import (C, Set_Scissor_Rectangle, "glScissor");


end Scissor_API;
