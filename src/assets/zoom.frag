// Mandelbrot Zoom

#define itter 256
#define PI2 6.283185

uniform float time;
uniform vec2 resolution;
uniform float angle;
uniform vec2 center;
uniform float zoom;
uniform bool drag;
uniform vec4 rect;

bool between(float a, float b) {
	return a >= b && a < b + 1.0;
}

void main() {
	float select = 0.0;
	vec2 coord = gl_FragCoord.xy;
	coord.y = resolution.y - coord.y;
	if (drag)
		if (coord.x >= rect.x && coord.x <= rect.z && coord.y >= rect.y && coord.y <= rect.w)
		{
			if (between(coord.x, rect.x) || between(coord.x + 1.0, rect.z) || between(coord.y, rect.y) || between(coord.y + 1.0, rect.w)) 
				select = 1.0;
			else
				select = 0.5;
		}
	// locate the position 0, 0
	vec2 c = (gl_FragCoord.xy -vec2(0.5) * resolution.xy) / vec2(resolution.x);
	// apply a zoom facor
	c.x = c.x * 3.0 / zoom;
	c.y = c.y * -3.0 / -zoom;
	// rotate c based on angle
	float cosa = cos(0.2 * angle);
	float sina = sin(0.2 * angle);
	c = vec2(c.x * cosa - c.y * sina, c.x * sina + c.y  * cosa);
	// center c based on some input value
	c += center;
	// calculate the mandelbrot set
	vec2 z = vec2(0.0);
	float zxx = z.x * z.x;
	float zyy = z.y * z.y;
	float count = 0.0;
	for (int i = 0; i < itter; i++) {
		if (zxx + zyy > 4.0)
			break;
		z.y = 2.0 * z.x * z.y + c.y;
		z.x = zxx - zyy + c.x;
		zxx = z.x * z.x;
		zyy = z.y * z.y;
		count += 1.0;
	}
	// color the result
	float r = 0.0, g = 0.0, b = 0.0;
	float f = float(itter); 
	if (count < f) {
		float m = dot(z, z);
		count = count + 1.0 - log2(0.5 * log2(m));
		count = sqrt(count / f);
		r = sin(PI2 * count + 0.5);
		g = sin(PI2 * count - 1.0);
		b = cos(PI2 * count + 2.0);
	}
	if (select > 0.0) {
		float blink = mix(0.8, 1.0, sin(time * 3.0));
		select = select * blink;
	}
	gl_FragColor = mix(vec4(r, g, b, 1.0), vec4(1.0), select);
}
