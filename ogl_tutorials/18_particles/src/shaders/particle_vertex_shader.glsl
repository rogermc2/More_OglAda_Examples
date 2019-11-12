#version 410 core

layout(location = 0) in vec3 squareVertices;
layout(location = 1) in vec4 xyzs; // Position of the center of the particule and size of the square
layout(location = 2) in vec4 color;

// Output data ; will be interpolated for each fragment.
out vec2 UV;
out vec4 particlecolor;

// Values that stay constant for the whole mesh.
uniform vec3 CameraRight_worldspace;
uniform vec3 CameraUp_worldspace;
uniform mat4 VP; // Model-View-Projection matrix, but without the Model
                 // (the position is in BillboardPos; the orientation depends on the camera)

void main()
    {
	vec3 particleCenter_worldspace = xyzs.xyz;
    float particleSize = xyzs.w;
	
	vec3 vertexPosition_worldspace = particleCenter_worldspace
        + CameraRight_worldspace * squareVertices.x * particleSize
		+ CameraUp_worldspace * squareVertices.y * particleSize;

	gl_Position = VP * vec4(vertexPosition_worldspace, 1.0f);
	UV = squareVertices.xy + vec2(0.5, 0.5);
	particlecolor = color;
    }

