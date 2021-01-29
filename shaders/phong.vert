#version 130

uniform mat4 cam,proj,trans;
in vec3 vertexCoord, vertexNorm;
in vec2 texCoord;
in float shininess;
in vec4 specular;

out vec3 frag_position, frag_normal;
out vec2 texCoordFrag;
out float frag_shininess;
out vec4 frag_specular;

void main() {
    mat4 mv_matrix = cam * trans;
    vec4 eye_position = mv_matrix * vec4(vertexCoord,1);
    gl_Position = proj * eye_position;
    frag_position = eye_position.xyz;
    frag_normal = (mv_matrix * vec4(vertexNorm, 0.0)).xyz;
    texCoordFrag = texCoord;
    frag_shininess = shininess;
    frag_specular = specular;
}
