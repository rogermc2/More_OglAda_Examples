#version 410

 layout (location = 0) in vec4 position;
 layout (location = 1) in vec2 texcoord;

 out VS_OUT
{
    vec4 colour;
    vec2 texcoord;
} vs_out;

uniform mat4 mv_matrix;
uniform mat4 projection_matrix;

void main()
{
  gl_Position = projection_matrix*mv_matrix*position;
    //    vs_out.colour = position*2.0 + vec4( 0.5, 0.5, 0.5, 0.0);
    vs_out.colour = vec4( 1.0, 0.0, 0.0, 1.0);

  vs_out.texcoord = texcoord;
}
