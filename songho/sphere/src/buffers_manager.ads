
with GL.Objects.Buffers;
with GL.Types;

with Sphere;

package Buffers_Manager is
    use GL.Types.Singles;

    type Vertex is private;
    type Vertices_Array is array (GL.Types.UInt range <>) of Vertex;

    procedure Create_Index_Buffers
      (IBO_1, IBO_2 : in out GL.Objects.Buffers.Buffer;
       Sphere_1, Sphere_2 : Sphere.Sphere);
    procedure Create_Vertex_Buffers
      (VBO_1, VBO_2 : in out GL.Objects.Buffers.Buffer;
       Sphere_1, Sphere_2 : Sphere.Sphere);
    procedure Load_Vertex_Buffer (Vertex_Buffer : GL.Objects.Buffers.Buffer;
                                  aSphere : Sphere.Sphere);

private
    type Vertex is record
        Position : Vector3;
        Texture  : Vector2;
    end record;

end Buffers_Manager;
