
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with Assimp_Util;

package body Animation is
   use Ada.Strings.Unbounded;

   function To_AI_Channels_List (Num_Channels : unsigned;
                                 Channels_Ptr : Node_Anim_Pointers.Pointer)
                                 return AI_Channels_List;
   function To_AI_Mesh_Anim_List (Num_Channels : unsigned;
                                  Mesh_Anims_Ptr : Mesh_Anim_Pointers.Pointer)
                                  return AI_Mesh_Anim_List;
   function To_AI_Mesh_Morph_List (Num_Channels : unsigned;
                                   Mesh_Morph_Ptr : Mesh_Morph_Anim_Pointers.Pointer)
                                   return Morph.AI_Mesh_Morph_List;
   function To_AI_Rotation_Keys (Num_Keys : unsigned;
                                 Rotation_Keys : Quat_Key_Pointers.Pointer)
                                 return AI_Quat_Key_List;
   function To_AI_Vector_Keys (Num_Keys : unsigned;
                               Vector_Keys : Vector_Key_Pointers.Pointer)
                               return AI_Vector_Key_List;
    function To_Vector4D (C_Quat : API_Vectors_Matrices.API_Quaternion)
                         return Singles.Vector4;

   --  ------------------------------------------------------------------------

   function To_AI_Animation_Map (Num_Animations : Interfaces.C.unsigned := 0;
                                 C_Array : API_Animation_Array)
                                 return AI_Animation_Map is
      anAnim     : AI_Animation;
      Anim_Map   : AI_Animation_Map;
   begin
      for index in 1 .. Num_Animations loop
         anAnim.Name := To_Unbounded_String (To_Ada (C_Array (index).Name.Data));
         anAnim.Duration := Single (C_Array (index).Duration);
         anAnim.Ticks_Per_Second := Single (C_Array (index).Ticks_Per_Second);
         if C_Array (index).Num_Channels > 0 then
            declare
               Chan_List : constant AI_Channels_List :=
                 To_AI_Channels_List (C_Array (index).Num_Channels,
                                      C_Array (index).Channels);
            begin
               anAnim.Channels := Chan_List;
            end;
         end if;

         if C_Array (index).Num_Mesh_Channels > 0 then
            declare
               Mesh_Anim_List : constant AI_Mesh_Anim_List :=
                 To_AI_Mesh_Anim_List (C_Array (index).Num_Channels,
                                       C_Array (index).Mesh_Channels);
            begin
               anAnim.Mesh_Channels := Mesh_Anim_List;
            end;
         end if;

         if C_Array (index).Num_Morphed_Mesh_Channels > 0 then
            declare
               Mesh_Morph_List : constant Morph.AI_Mesh_Morph_List :=
                 To_AI_Mesh_Morph_List (C_Array (index).Num_Morphed_Mesh_Channels,
                                        C_Array (index).Morphed_Mesh_Channels);
            begin
               anAnim.Morph_Mesh_Channels := Mesh_Morph_List;
            end;
         end if;
         Anim_Map.Insert (UInt (index), anAnim);
      end loop;
      return Anim_Map;

   exception
      when others =>
         Put_Line ("An exception occurred in Animation.To_AI_Animation_Map.");
         raise;
   end To_AI_Animation_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Channels_List (Num_Channels : unsigned;
                                 Channels_Ptr : Node_Anim_Pointers.Pointer)
                                 return AI_Channels_List is
      C_Channel_Array  : constant API_Node_Anim_Array
        := Node_Anim_Pointers.Value
          (Channels_Ptr, ptrdiff_t (Num_Channels));
      Node_Anim        : AI_Node_Anim;
      Chan_List        : AI_Channels_List;
      Quat_Key_List    : AI_Quat_Key_List;
      Scaling_Key_List : AI_Vector_Key_List;
      Vector_Key_List  : AI_Vector_Key_List;
   begin
      for chan in 1 .. Num_Channels loop
         Node_Anim.Name :=
           To_Unbounded_String (To_Ada (C_Channel_Array (chan).Name.Data));

         if C_Channel_Array (chan).Num_Position_Keys > 0 then
            Vector_Key_List :=
              To_AI_Vector_Keys (C_Channel_Array (chan).Num_Position_Keys,
                                 C_Channel_Array (chan).Position_Keys);
            Node_Anim.Position_Keys := Vector_Key_List;
         end if;

         if C_Channel_Array (chan).Num_Rotation_Keys > 0 then
            Quat_Key_List :=
              To_AI_Rotation_Keys (C_Channel_Array (chan).Num_Rotation_Keys,
                                   C_Channel_Array (chan).Rotation_Keys);
            Node_Anim.Rotation_Keys := Quat_Key_List;
         end if;

         if C_Channel_Array (chan).Num_Scaling_Keys > 0 then
            Scaling_Key_List :=
              To_AI_Vector_Keys (C_Channel_Array (chan).Num_Scaling_Keys,
                                 C_Channel_Array (chan).Scaling_Keys);
            Node_Anim.Scaling_Keys := Scaling_Key_List;
         end if;
         Chan_List.Append (Node_Anim);
      end loop;

      return Chan_List;

   exception
      when others =>
         Put_Line ("An exception occurred in Animation.To_AI_Channels_List.");
         raise;
   end To_AI_Channels_List;

   --  ------------------------------------------------------------------------

   function To_AI_Mesh_Anim_List (Num_Channels : unsigned;
                                  Mesh_Anims_Ptr : Mesh_Anim_Pointers.Pointer)
                                  return AI_Mesh_Anim_List is
      C_Array        : constant API_Mesh_Anim_Array
        := Mesh_Anim_Pointers.Value (Mesh_Anims_Ptr, ptrdiff_t (Num_Channels));
      Mesh_Anim : AI_Mesh_Anim;
      Mesh_Anim_List : AI_Mesh_Anim_List;
   begin
      for index in 1 .. Num_Channels loop
         Mesh_Anim.Name := To_Unbounded_String (To_Ada (C_Array (index).Name.Data));

         declare
            Mesh_Key    : AI_Mesh_Key;
            Mesh_Keys   : AI_Mesh_Key_List;
            C_Mesh_Keys : constant API_Mesh_Key_Array
              := Mesh_Key_Pointers.Value (C_Array (index).Keys,
                                          ptrdiff_t (C_Array (index).Num_Keys));
         begin
            for key in 1 .. C_Array (index).Num_Keys loop
               Mesh_Key.Time := Single (C_Mesh_Keys (key).Time);
               Mesh_Key.Value := UInt (C_Mesh_Keys (key).Value);
               Mesh_Keys.Append (Mesh_Key);
            end loop;
         end;
         Mesh_Anim_List.Append (Mesh_Anim);
      end loop;
      return Mesh_Anim_List;

   exception
      when others =>
         Put_Line ("An exception occurred in Animation.To_AI_Mesh_Anim_List.");
         raise;
   end To_AI_Mesh_Anim_List;

   --  ------------------------------------------------------------------------

   function To_AI_Mesh_Morph_List (Num_Channels : unsigned;
                                   Mesh_Morph_Ptr : Mesh_Morph_Anim_Pointers.Pointer)
                                   return Morph.AI_Mesh_Morph_List is
      C_Morph_Array   : constant API_Mesh_Morph_Anim_Array
        := Mesh_Morph_Anim_Pointers.Value
          (Mesh_Morph_Ptr, ptrdiff_t (Num_Channels));
      Mesh_Morph      : Morph.AI_Mesh_Morph_Anim;
      Mesh_Morph_List : Morph.AI_Mesh_Morph_List;
   begin
      for index in 1 .. Num_Channels loop
         Mesh_Morph.Name :=
           To_Unbounded_String (To_Ada (C_Morph_Array (index).Name.Data));
         declare
            Morph_Key   : Morph.AI_Mesh_Morph_Key;
            Morph_Keys  : Morph.AI_Mesh_Morph_Key_List;
            C_Keys      : constant Morph.API_Mesh_Morph_Key_Array
              := Morph.Mesh_Morph_Key_Pointers.Value
                (C_Morph_Array (index).Keys,  ptrdiff_t (C_Morph_Array (index).Num_Keys));
         begin
            for key in 1 .. C_Morph_Array (index).Num_Keys loop
               Morph_Key.Key_Time :=
                 Single (C_Keys (unsigned (key)).Key_Time);
               declare
                  Vals : constant Morph.API_Morph_Values_Array :=
                    Morph.Mesh_Morph_Value_Pointers.Value
                      (C_Keys (key).Values, ptrdiff_t (C_Keys (key).Num_Values_And_Weights));
                  Wgts : constant Morph.API_Morph_Weights_Array :=
                    Morph.Mesh_Morph_Weight_Pointers.Value
                      (C_Keys (key).Weights, ptrdiff_t (C_Keys (key).Num_Values_And_Weights));
               begin
                  --                    Num_VW : unsigned :=
                  --                      C_Keys (unsigned (key)).Num_Values_And_Weights;
                  --
                  --                   type API_Mesh_Morph_Key is record
                  --                       Key_Time               : Interfaces.C.double := 0.0;
                  --                       Values                 : Morph.API_Morph_Values_Array (1 .. Num_VW);
                  --                       Weights                : Morph.API_Morph_Weights_Array (1 .. Num_VW);
                  --                       Num_Values_And_Weights : unsigned := 0;
                  --                    end record;
                  --                    pragma Convention (C_Pass_By_Copy, API_Mesh_Morph_Key);
                  --
                  --                    type API_Mesh_Morph_Key_Array is array
                  --                      (Interfaces.C.unsigned range <>) of aliased API_Mesh_Morph_Key;
                  --                    pragma Convention (C, API_Mesh_Morph_Key_Array);
                  --
                  --                    C_Keys  : API_Mesh_Morph_Key_Array (1 .. Num_VW);

                  for val in 1 .. C_Morph_Array (index).Keys.Num_Values_And_Weights loop
                     Morph_Key.Values.Append (UInt (Vals (val)));
                     Morph_Key.Weights.Append (Single (Wgts (val)));
                  end loop;
               end;
               Morph_Keys.Append (Morph_Key);
            end loop;
            Mesh_Morph.Keys := Morph_Keys;
         end;
         Mesh_Morph_List.Append (Mesh_Morph);
      end loop;
      return Mesh_Morph_List;

   exception
      when others =>
         Put_Line ("An exception occurred in Animation.To_AI_Mesh_Morph_List.");
         raise;
   end To_AI_Mesh_Morph_List;

   --  ------------------------------------------------------------------------

   function To_AI_Vector_Keys (Num_Keys : unsigned;
                               Vector_Keys : Vector_Key_Pointers.Pointer)
                                  return AI_Vector_Key_List is
      C_Key_Array     : constant API_Vector_Key_Array
        := Vector_Key_Pointers.Value (Vector_Keys, ptrdiff_t (Num_Keys));
      Vector_Key      : AI_Vector_Key;
      Vector_Key_List : AI_Vector_Key_List;
   begin
      for key in 1 .. Num_Keys loop
         Vector_Key.Time := Single (C_Key_Array (key).Time);
         Vector_Key.Value :=
           Assimp_Util.To_OGL_Vector3 (C_Key_Array (key).Value);
         Vector_Key_List.Append (Vector_Key);
      end loop;
      return Vector_Key_List;

   exception
      when others =>
         Put_Line ("An exception occurred in Animation.To_AI_Vector_Keys.");
         raise;
   end To_AI_Vector_Keys;

   --  ------------------------------------------------------------------------

   function To_AI_Rotation_Keys (Num_Keys : unsigned;
                                 Rotation_Keys : Quat_Key_Pointers.Pointer)
                                    return AI_Quat_Key_List is
      C_Key_Array     : constant API_Quat_Key_Array
        := Quat_Key_Pointers.Value (Rotation_Keys, ptrdiff_t (Num_Keys));
      Rotation_Key      : AI_Quat_Key;
      Rotation_Key_List : AI_Quat_Key_List;
   begin
      for key in 1 .. Num_Keys loop
         Rotation_Key.Time := Single (C_Key_Array (key).Time);
         Rotation_Key.Value := To_Vector4D (C_Key_Array (key).Value);
         Rotation_Key_List.Append (Rotation_Key);
      end loop;
      return Rotation_Key_List;

   exception
      when others =>
         Put_Line ("An exception occurred in Animation.To_AI_Rotation_Keys.");
         raise;
   end To_AI_Rotation_Keys;

   --  ------------------------------------------------------------------------

   function To_Vector4D (C_Quat : API_Vectors_Matrices.API_Quaternion)
                            return Singles.Vector4 is
   begin
      return (Single (C_Quat.X), Single (C_Quat.Y),
              Single (C_Quat.Z), Single (C_Quat.W));
   end To_Vector4D;

   --  ------------------------------------------------------------------------

end Animation;
