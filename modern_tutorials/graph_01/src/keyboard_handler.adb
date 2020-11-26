
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Keys;

package body Keyboard_Handler is

   procedure Key_Down (Window : in out Input_Callback.Callback_Window;
                       Status : in out Status_Data) is
      use Glfw.Input.Keys;
      use GL.Types;
   begin
      if Input_Callback.Is_Key_Down (F1) then
         Status.Mode := 0;
         Put_Line ("Now drawing using lines.");
      elsif Input_Callback.Is_Key_Down (F2) then
         Status.Mode := 1;
         Put_Line ("Now drawing using points.");
      elsif Input_Callback.Is_Key_Down (F3) then
         Status.Mode := 2;
         Put_Line ("Now drawing using point sprites.");

      elsif Input_Callback.Is_Key_Down (Left) then
         Status.X_Offset := Status.X_Offset - 0.1;
      elsif Input_Callback.Is_Key_Down (Right) then
         Status.X_Offset := Status.X_Offset + 0.1;

      elsif Input_Callback.Was_Key_Pressed (Window, Up) then
         Status.X_Scale := 1.5 * Status.X_Scale;
      elsif Input_Callback.Is_Key_Down (Down) then
         Status.X_Scale := Status.X_Scale / 1.5;

      elsif Input_Callback.Is_Key_Down (Home) then
         Status.X_Offset := 0.0;
         Status.X_Scale := 1.0;
      end if;
   end ;

end Keyboard_Handler;
