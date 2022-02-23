<<<<<<< HEAD:khronos/tutorial_1/src/basic.adb
--  Program Basic
--  Author Roger Mc Murtrie
--  Created 10 August 2020
--  Based on khronos.org OpenGL Tutorial 1
=======
-- Program OpenGL_Fun
-- Author Roger Mc Murtrie
-- Created 6 May 2021
>>>>>>> master:games/fun/src/opengl_fun.adb

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

<<<<<<< HEAD:khronos/tutorial_1/src/basic.adb
procedure Basic is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "Khronous OpenGL Tutorial_1";
=======
procedure OpenGL_Fun is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "OpenGL_Fun";
>>>>>>> master:games/fun/src/opengl_fun.adb
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
<<<<<<< HEAD:khronos/tutorial_1/src/basic.adb
        Put ("Basic returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Basic.");
        Put_Line (Exception_Information (anError));
end Basic;
=======
        Put ("OpenGL_Fun returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in OpenGL_Fun.");
        Put_Line (Exception_Information (anError));
end OpenGL_Fun;
>>>>>>> master:games/fun/src/opengl_fun.adb
