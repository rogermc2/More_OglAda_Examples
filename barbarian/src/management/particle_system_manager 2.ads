
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Maths;

package Particle_System_Manager is
    use Singles;

    package Vec3_Package is new
      Ada.Containers.Doubly_Linked_Lists (Singles.Vector3);
    type Vec3_List is new Vec3_Package.List with null record;

    type Particle_Script is record
        Particle_Count                   : Int := 0;
        Particle_Initial_Velocity        : Vec3_List;
        Acceleration                     : Singles.Vector3 := Maths.Vec3_0;
        Initial_Colour                   : Singles.Vector4 := Maths.Vec4_0;
        Final_Colour                     : Singles.Vector4 := Maths.Vec4_0;
        Rotate_Emitter_Around_Offs       : Singles.Vector3 := Maths.Vec3_0;
        Anim_Move_Emitter_From           : Singles.Vector3 := Maths.Vec3_0;
        Anim_Move_Emitter_To             : Singles.Vector3 := Maths.Vec3_0;
        Script_Name                      : Unbounded_String :=
                                             To_Unbounded_String ("");
        Total_System_Seconds             : Int := 0;
        Particle_Lifetime                : Int := 0;
        Seconds_Between                  : Int := 0;
        Rotate_Emitter_Around_Degs_Per_S : Single := 0.0;
        Initial_Scale                    : Single := 0.0;
        Final_Scale                      : Single := 0.0;
        Degrees_Per_Second               : Single := 0.0;
        Bounding_Radius                  : Single := 0.0;
        VAO                              : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        VAO_Index                        : GL.Attributes.Attribute := 0;
        Texture                          : GL.Objects.Textures.Texture;
        Particle_World_Positions_VBO     : GL.Objects.Buffers.Buffer;
        Particle_Ages_VBO                : GL.Objects.Buffers.Buffer;
        Is_Looping                       : Boolean := False;
        Rotate_Emitter_Around            : Boolean := False;
        Anim_Move_Emitter                : Boolean := False;
    end record;

    package Script_Package is new
      Ada.Containers.Vectors (Positive, Particle_Script);
    type Particle_Script_List is new Script_Package.Vector with null record;

    procedure Load_Particle_Script (File_Name : String;
                                    Scripts : in out Particle_Script_List);

end Particle_System_Manager;
