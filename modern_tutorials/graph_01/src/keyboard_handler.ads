
with GL.Types;

with Input_Callback;

package Keyboard_Handler is

   type Mode_Range is new GL.Types.Int range 0 .. 2;
   type Status_Data is record
      Mode     : Mode_Range := 0;
      X_Offset : GL.Types.Single := 0.0;
      X_Scale  : GL.Types.Single := 1.0;
   end record;

   procedure Key_Down (Window : in out Input_Callback.Callback_Window;
                       Status      : in out Status_Data);

end Keyboard_Handler;
