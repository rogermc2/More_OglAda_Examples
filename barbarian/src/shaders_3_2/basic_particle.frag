
#version 410 core
    
out vec4 frag_colour;
//in vec4 dummy;
in vec4 fcolour;
in float fdegree;

uniform sampler2D texture_map;

void main ()
    {
	//frag_colour = dummy;
	if (fcolour.a < 0.01)
        {
		discard;
        }
	vec2 texcoord = gl_PointCoord;
	if (fdegree > 0.0)
        {
		float rad = 0.0174532925 * fdegree;
		vec2 pre = texcoord * 2.0 - 1.0;
		mat2 rot;
		rot[0][0] = cos (rad);
		rot[0][1] = sin (rad);
		rot[1][0] = -sin (rad);
		rot[1][1] = cos (rad);
		texcoord = rot * pre;
		texcoord = (texcoord + 1.0) * 0.5;
        }
	frag_colour = fcolour * texture (texture_map, texcoord);
    }
