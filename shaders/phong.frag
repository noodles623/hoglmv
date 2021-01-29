#version 130

uniform mat4 cam;
uniform sampler2D tex[2];

in vec3 frag_position, frag_normal;
in vec2 texCoordFrag;
in float frag_shininess;
in vec4 frag_specular;

//const vec3 light_direction = vec3(0.408248, -0.816497, 0.408248);
vec3 light_direction = vec3(0.4, -0.8, -0.4);
vec4 light_diffuse = vec4(0.8, 0.8, 0.8, 0.0);
vec4 light_ambient = vec4(0.2, 0.2, 0.2, 1.0);
vec4 light_specular = vec4(1.0, 1.0, 1.0, 1.0);

out vec4 fragColor;

void main() {
    if (!gl_FrontFacing) light_diffuse = vec4(0.1,0.1,0.1,0.0);
    vec3 mv_light_direction = (cam * vec4(light_direction, 0.0)).xyz
	,normal = normalize(frag_normal)
	,eye = normalize(frag_position)
	,reflection = reflect(mv_light_direction, normal); // Irrelevant
    vec4 frag_diffuse = texture(tex[0], texCoordFrag);
    vec4 diffuse_factor
	= max (-dot(normal, mv_light_direction), 0.0) * light_diffuse;
    vec4 ambient_diffuse_factor 
	= diffuse_factor + light_ambient;
    vec4 specular_factor // Irrelevant
	= max(pow(-dot(reflection, eye), frag_shininess), 0.0)
	* light_specular;
    fragColor = specular_factor * frag_specular
              + ambient_diffuse_factor * frag_diffuse;
}
						
