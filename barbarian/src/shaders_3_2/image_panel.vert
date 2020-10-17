
#version 410 core

layout(location = 0) in vec2 vp;
// TODO replace this with pos/sca vec? (16 -> 4 floats)
uniform mat4 model_mat;
out vec2 st;

void main ()
    {
	st = vp * 0.5 + 0.5;
	gl_Position = model_mat * vec4 (vp, 0.0, 1.0);
    }
