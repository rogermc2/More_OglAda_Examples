//
// Crongdor the Barbarian
// 2d text shader background box
// First version Anton Gerdelan, ?10 Decemeber 2014
// Latest code review 15 Dec 2014
//

#version 410 core
out vec4 frag_colour;

in vec2 fp;

uniform vec4 colour;
uniform vec2 viewportdims, scale;

float border_size_px = 2.0;

void main ()
    {
	if (colour.a < 0.01)
        {
		discard;
        }
	frag_colour = colour;
	
	// black borders
	vec2 size_px = scale * 0.5 * viewportdims;
	
	if (fp.x * size_px.x <= border_size_px)
        {
		frag_colour = vec4 (0.0, 0.0, 0.0, 1.0);
        }
    else if (fp.x * size_px.x >= size_px.x - border_size_px)
        {
		frag_colour = vec4 (0.0, 0.0, 0.0, 1.0);
        }
	if (-fp.y * size_px.y <= border_size_px)
        {
		frag_colour = vec4 (0.0, 0.0, 0.0, 1.0);
        }
    else if (-fp.y * size_px.y >= size_px.y - border_size_px)
        {
		frag_colour = vec4 (0.0, 0.0, 0.0, 1.0);
        }
    }
