
with Prop_Renderer;

package Prop_Renderer.Boulder is

   Boulder_Exception : Exception;

   procedure Update_Boulder (Prop_Index : Positive;
                             Script  : Prop_Renderer.Prop_Script;
                             Seconds : Float);

end Prop_Renderer.Boulder;
