
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Blending;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;

with Utilities;

with Camera;
with FB_Effects;
with Frustum;
with Game_Utils;
with GL_Utils;
with Particle_System_Shader_Manager;
with Prop_Renderer;
with Prop_Renderer_Support;
with Settings;

package body Particle_System is

   Scripts               : Particle_System_Manager.Particle_Script_List;
   Particle_Systems      : Particle_Systems_List;

   Basic_Particles_SP    : GL.Objects.Programs.Program;
   Last_Script_ID        : Natural := 0;
   Particles_Initialised : Boolean := False;

   function Get_Particle_Script_Number
     (Name : String; Script_Number : out Positive) return Boolean;

   --  ------------------------------------------------------------------------

   function Create_Particle_System
     (Script_Name : String; Start_Now, Always_Update, Always_Draw : Boolean)
      return Positive is
      Particle_Count : Integer;
      Script_Num     : Natural := 0;
      Script         : Particle_System_Manager.Particle_Script;
      P_System       : Particle_System;
      Last_Index     : Positive := 1;
   begin
      if Script_Name'Length < 1 then
         raise Particle_System_Exception with
           " Particle_System.Create_Particle_System Particles empty Script_Name.";
      elsif not Particles_Initialised then
         raise Particle_System_Exception with
           " Particle_System.Create_Particle_System Particles not initialised.";
      else
         if Get_Particle_Script_Number (Script_Name, Script_Num) then
            Script := Scripts.Element (Script_Num);
            Particle_Count := Integer (Script.Particle_Count);
            for index in 1 .. Particle_Count loop
               P_System.Particle_Positions.Append (Maths.Vec3_0);
               P_System.Particle_Ages.Append
                 (Single (index) * Script.Seconds_Between);
            end loop;
            P_System.Script_Index := Script_Num;
            P_System.Is_Running := Start_Now;
            P_System.Always_Update := Always_Update;
            P_System.Always_Draw := Always_Draw;
            Particle_Systems.Append (P_System);
            Last_Index := Particle_Systems.Last_Index;
         else
            raise Particle_System_Exception with
              " Particle_System.Create_Particle_System Particles invalid script number: "
              & Positive'Image (Script_Num);
         end if;
      end if;

      return Last_Index;

   end Create_Particle_System;

   --  ------------------------------------------------------------------------

   function Get_Particle_Script (Script_Index : Positive)
                                 return Particle_System_Manager.Particle_Script is
   begin
      return Scripts.Element (Script_Index);
   end Get_Particle_Script;

   --  -------------------------------------------------------------------------

   function Get_Particle_Script_Number
     (Name : String; Script_Number : out Positive) return Boolean is
      use Ada.Strings.Unbounded;
      use Particle_System_Manager.Script_Package;
      Curs          : Cursor := Scripts.First;
      Found         : Boolean := False;
   begin
      if Name'Length < 1 then
         raise Particle_System_Exception with
           " Particle_System.Get_Particle_Script_Number Particles empty Script_Name.";
      elsif not Particles_Initialised then
         raise Particle_System_Exception with
           " Particle_System.Get_Particle_Script_Number Particles not initialised.";
      else
         while Has_Element (Curs) and not Found loop
            --              Game_Utils.Game_Log (" Particle_System.Get_Particle_Script_Number" &
            --                                  " existing Script_Name: " & To_String (Element (Curs).Script_Name));
            Found := To_String (Element (Curs).Script_Name) = Name;
            if Found then
               Script_Number := To_Index (Curs);
            else
               Next (Curs);
            end if;
         end loop;
      end if;

      if not Found then
         Particle_System_Manager.Load_Particle_Script (Name, Scripts);
         Found := Get_Particle_Script_Number (Name, Script_Number);
         if not Found then
            raise Particle_System_Exception with
              " Particle_System.Get_Particle_Script_Number, particle: " & Name &
              " not found.";
         end if;
      end if;
      return Found;

   end Get_Particle_Script_Number;

   --  ------------------------------------------------------------------------

   function Get_Particle_System
     (System_ID : Positive; theSystem : in out Particle_System) return Boolean is
      Found : constant Boolean := System_ID <= Particle_Systems.Last_Index;
   begin
      if Found then
         theSystem := Particle_Systems.Element (System_ID);
      end if;
      return Found;
   end Get_Particle_System;

   --  ------------------------------------------------------------------------

   function Has_Particle_System (System_ID : Positive) return Boolean is
   begin
      return System_ID <= Particle_Systems.Last_Index;
   end Has_Particle_System;

   --  ------------------------------------------------------------------------

   procedure Init is
      use Particle_System_Manager;
   begin
      Game_Utils.Game_Log ("---- Initialising Particle Systems ----");
      Particle_System_Shader_Manager.Init (Basic_Particles_SP);
      Scripts.Clear;
      Particle_Systems.Clear;
      Load_Particle_Script ("torch_smoke.particles", Scripts);
      Load_Particle_Script ("blood_fountain.particles", Scripts);
      Load_Particle_Script ("blood_damage.particles", Scripts);
      Load_Particle_Script ("blood_artery_jet.particles", Scripts);
      Load_Particle_Script ("dust.particles", Scripts);
      Load_Particle_Script ("splash.particles", Scripts);
      Game_Utils.Game_Log ("---- Particle Systems Initialized ----");
      Particles_Initialised := True;
   end Init;

   --  ------------------------------------------------------------------------

   function Is_Running (System_ID : Positive) return Boolean is
      System : constant Particle_System := Particle_Systems.Element (System_ID);
   begin
      return System.Is_Running;
   end Is_Running;

   --  ------------------------------------------------------------------------

   function Length return Positive is
   begin
      return Particle_Systems.Last_Index;
   end Length;

   --  ------------------------------------------------------------------------

   procedure Render_Particle_Systems (Seconds : Single) is
      use GL.Blending;
      use GL.Objects.Textures;
      use GL.Toggles;
      use Particle_System_Shader_Manager;
      aSystem          : Particle_System;
      Script_ID        : Positive;
      aScript          : Particle_System_Manager.Particle_Script;
      On_Screen        : Boolean := False;
   begin
      if Particles_Initialised and Settings.Particles_Enabled then
         Enable (Blend);
         Set_Blend_Func (Src_Alpha, One_Minus_Src_Alpha);
         GL.Buffers.Depth_Mask (False);
         Enable (Vertex_Program_Point_Size);
         Last_Script_ID := 0;

         for index in Particle_Systems.First_Index ..
           Particle_Systems.Last_Index loop
            aSystem := Particle_Systems (index);
            if aSystem.Is_Running then
               Script_ID := aSystem.Script_Index;
               aScript := Scripts.Element (Script_ID);
               On_Screen := True;
               if aSystem.Always_Draw then
                  On_Screen := Frustum.Is_Sphere_In_Frustum
                    (aSystem.Emitter_World_Pos, aScript.Bounding_Radius);
               end if;
               if On_Screen or aSystem.Always_Update then
                  Update_Particle_System (index, Seconds);
               end if;
               if On_Screen then
                  GL_Utils.Bind_VAO (aScript.VAO);
                  if Script_ID /= Scripts.Last_Index then
                     Set_Active_Unit (0);
                     Targets.Texture_2D.Bind (aScript.Texture);
                     GL.Objects.Programs.Use_Program (Basic_Particles_SP);
                     if Camera.Is_Dirty then
                        Set_Perspective_View
                          (Camera.View_Matrix);
                     end if;
                     Set_Initial_Colour (aScript.Initial_Colour);
                     Set_Final_Colour (aScript.Final_Colour);
                     Set_Initial_Scale (aScript.Initial_Scale);
                     Set_Final_Scale (aScript.Final_Scale);
                     Set_Degrees (aScript.Degrees_Per_Second);
                     Set_Lifetime (aScript.Particle_Lifetime);
                     Set_Pixel_Width (Single (Settings.Framebuffer_Width) *
                                        FB_Effects.Current_SSAA);
                  end if;
                  GL.Objects.Vertex_Arrays.Draw_Arrays
                    (Points, 0, Int (aScript.Particle_Count));
               end if;
            end if;
         end loop;

         Disable (Vertex_Program_Point_Size);
         GL.Buffers.Depth_Mask (True);
         Disable (Blend);
      end if;
   end Render_Particle_Systems;

   --  ------------------------------------------------------------------------

   function Script_Index (System_ID : Positive) return Positive is
      System : constant Particle_System := Particle_Systems.Element (System_ID);
   begin
      return System.Script_Index;
   end Script_Index;

   --  ------------------------------------------------------------------------

   procedure Set_Particle_System_Heading (System_ID : Positive;
                                          Heading   : Maths.Degree) is
      theSystem : Particle_System;
   begin
      if Particles_Initialised then
         theSystem := Particle_Systems.Element (System_ID);
         theSystem.Rot_Mat := Maths.Rotate_Y_Degree (Singles.Identity4, Heading);
         Particle_Systems.Replace_Element (System_ID, theSystem);
      end if;
   end Set_Particle_System_Heading;

   --  ------------------------------------------------------------------------

   procedure Set_Particle_System_Position (System_ID         : Positive;
                                           Emitter_World_Pos : Singles.Vector3) is
      theSystem : Particle_System;
   begin
      if Particles_Initialised then
         theSystem := Particle_Systems.Element (System_ID);
         theSystem.Emitter_World_Pos := Emitter_World_Pos;
         Particle_Systems.Replace_Element (System_ID, theSystem);
      end if;
   end Set_Particle_System_Position;

   --  ------------------------------------------------------------------------

   procedure Start_Particle_System (System_ID : Positive) is
      use Particle_System_Manager;
      use GL_Utils;
      use Singles_Package;
      use Vector3_Package;
      theSystem    : Particle_System;
      Script_Index : Positive;
      Script       : Particle_System_Manager.Particle_Script;
      Ages         : Singles_Package.Vector := theSystem.Particle_Ages;
      Positions    : GL_Utils.Vector3_Package.Vector :=
                       theSystem.Particle_Positions;
      Age_Scale    : Single;

      procedure Update_Particle_Age (Age : in out Single) is
      begin
         Age := - Age_Scale * Script.Seconds_Between;
      end Update_Particle_Age;

      procedure Update_Particle_Position (Pos : in out Singles.Vector3) is
      begin
         Pos := theSystem.Emitter_World_Pos;
      end Update_Particle_Position;

   begin
      if Particles_Initialised then
         theSystem := Particle_Systems.Element (System_ID);
         Script_Index := theSystem.Script_Index;
         theSystem.Is_Running := True;
         theSystem.System_Age := 0.0;
         Script := Scripts.Element (Script_Index);
         for index in Ages.First_Index .. Ages.Last_Index loop
            Age_Scale:= Single (index -1);
            Ages.Update_Element (index, Update_Particle_Age'Access);
         end loop;

         for index in Positions.First_Index .. Positions.Last_Index loop
            Positions.Update_Element (Index, Update_Particle_Position'Access);
         end loop;
         Particle_Systems.Replace_Element (System_ID, theSystem);
      end if;
   end Start_Particle_System;

   --  ------------------------------------------------------------------------

   procedure Stop_Particle_System (System_ID : Positive) is
      theSystem : Particle_System;
   begin
      if Particles_Initialised then
         theSystem := Particle_Systems.Element (System_ID);
         theSystem.Is_Running := False;
         Particle_Systems.Replace_Element (System_ID, theSystem);
      end if;
   end Stop_Particle_System;

   --  ------------------------------------------------------------------------

   procedure Stop_Particle_Systems is
      use Systems_Package;
      Curs      : Cursor := Particle_Systems.First;
      theSystem : Particle_System;
   begin
      if Particles_Initialised then
         while Has_Element (Curs) loop
            Stop_Particle_System (To_Index (Curs));
            Next (Curs);
         end loop;
      end if;
   end Stop_Particle_Systems;

   --  ------------------------------------------------------------------------

   procedure Update_Dynamic_Buffers
     (aSystem : Particle_System;
      aScript : Particle_System_Manager.Particle_Script) is
      use GL_Utils;
      use Singles_Package;
      use Vector3_Package;
      use GL.Objects.Buffers;
      Vertices : constant Singles.Vector3_Array := GL_Utils.To_Vector3_Array
        (aSystem.Particle_Positions);
      Ages : constant Single_Array := GL_Utils.To_Single_Array
        (aSystem.Particle_Ages);
   begin
      Array_Buffer.Bind (aScript.Particle_World_Positions_VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Dynamic_Draw);
      Array_Buffer.Bind (aScript.Particle_Ages_VBO);
      Utilities.Load_Singles_Buffer (Array_Buffer, Ages, Dynamic_Draw);
   end Update_Dynamic_Buffers;

   --  ------------------------------------------------------------------------

   procedure Update_Particle_System (System_ID : Positive;
                                     Seconds   : Single) is
      use Maths;
      use GL_Maths;
      use Vec3_Package;
      theSystem      : Particle_System;
      theScript      : Particle_System_Manager.Particle_Script;
      Emmiter_Offs   : Singles.Vector3;
      Curr_Vec       : Singles.Vector3;
      Deg            : Degree;
      Tel_Rot        : Singles.Matrix4;
      Offset         : Singles.Vector4;
      F              : Single;
      Age            : Single;
   begin
      if (Particles_Initialised or Settings.Particles_Enabled) then
         theSystem := Particle_Systems.Element (System_ID);
         if theSystem.Is_Running then
            theSystem.System_Age := theSystem.System_Age + Float (Seconds);
            theScript := Scripts (theSystem.Script_Index);
            theSystem.Is_Running := theScript.Is_Looping or
              theSystem.System_Age < Float (theScript.Total_System_Seconds);
            if theSystem.Is_Running then
               Emmiter_Offs := theSystem.Emitter_World_Pos;
            end if;
            Particle_Systems.Replace_Element (System_ID, theSystem);
            if theScript.Rotate_Emitter_Around then
               Deg := Degree (theSystem.System_Age) *
                 theScript.Rotate_Emitter_Around_Degs_Per_S;
               Tel_Rot := Rotate_Y_Degree (Identity4, Deg);
               Offset := Tel_Rot * To_Vector4 (theScript.Rotate_Emitter_Around_Offs);
               Emmiter_Offs := Emmiter_Offs + To_Vector3 (Offset);
            end if;
            if theScript.Anim_Move_Emitter then
               F := 1.0 - Single (theSystem.System_Age) /
                 theScript.Total_System_Seconds;
                 Emmiter_Offs := Emmiter_Offs +
                   GL_Maths.Lerp (theScript.Anim_Move_Emitter_From,
                                    theScript.Anim_Move_Emitter_To, F);
            end if;

            for index in 1 .. theScript.Particle_Count loop
               Age := theSystem.Particle_Ages.Element (index) + Seconds;
               theSystem.Particle_Ages.Replace_Element (index, Age);
               if Age < 0.00001 then
                  theSystem.Particle_Positions.Replace_Element (index, Emmiter_Offs);
               else
                  if Age >= theScript.Particle_Lifetime then
                     theSystem.Particle_Ages.Replace_Element
                       (index, Single (index) * theScript.Seconds_Between);
                     theSystem.Particle_Positions.Replace_Element (index, Emmiter_Offs);
                  end if;
                  Curr_Vec := theScript.Particle_Initial_Velocity.Element (index) +
                    theScript.Acceleration * theSystem.Particle_Ages.Element (index);
                  Curr_Vec := To_Vector3 (theSystem.Rot_Mat * To_Vector4 (Curr_Vec));
                  Curr_Vec := Seconds * Curr_Vec +
                    theSystem.Particle_Positions.Element (index);
                  theSystem.Particle_Positions.Replace_Element (index, Curr_Vec);
               end if;
            end loop;
            Update_Dynamic_Buffers (theSystem, theScript);
         end  if;
      end if;
   end Update_Particle_System;

   --  ------------------------------------------------------------------------

end Particle_System;
