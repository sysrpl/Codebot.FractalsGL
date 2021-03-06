        ??  ??                  {  0   ??
 T O U R . F R A G                 // Mandelbrot Tour

#define PI2 6.283185
#define SPEED 2.0

uniform vec2 resolution;
uniform float time;

void main(void) {
	// init tour locations
	vec2 tours[6];
	tours[0] = vec2(-1.25066, 0.02012);
	tours[1] = vec2(-0.745, 0.186);
	tours[2] = vec2(-0.7453,0.1127);
	tours[3] = vec2(-0.235125, 0.827215);
	tours[4] = vec2(-0.395375, 0.6799986);
	tours[5] = vec2(0.270, 0.482);
	// modify time factor t
	float t = mod(time * SPEED, 60.0);
	// index indicates which tour we are in
	int index = int(mod(time * SPEED, 60.0 * 6.0) / 60.0);
	// next indicates the percentage transition to the next tour
	float next = 0.0;
	if (t > 50.0) {
		next = (t - 50.0) / 10.0;
		next = smoothstep(0.0, 1.0, next);
	}
	if (t > 30.0) {
		t = 60.0 - t;
	}
	t = smoothstep(0.0, 29.0, t) * 29.0;
	float zoom = 0.62 + 0.38 * 0.25 * -t * 0.20;
	float cosa = cos(0.1 * (1.0 - zoom) * t);
	float sina = sin(0.1 * (1.0 - zoom) * t);
	if (mod(float(index), 2.0) == 0.0)
		sina = -sina;
	zoom = pow(zoom, 3.0);
	vec2 xy = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
	xy.x *= resolution.x/resolution.y;
	xy = vec2(xy.x * cosa - xy.y * sina, xy.x * sina + xy.y  * cosa);
	int n = int(mod(float(index) + 1.0, 6.0));
	vec2 c = tours[index] * (1.0 - next) + tours[n] * next;
	c = c + xy * zoom;
	vec2 z = vec2(0.0);
	float itter = 0.0;
	for (int i = 0; i < 256; i++) {
		if (z.x * z.x + z.y * z.y > 4.0)
			break;
		z = c + vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y);
		itter += 1.0;
	}
	float m = dot(z, z);
	itter = itter + 1.0 - log2(0.5 * log2(m));
	itter = sqrt(itter / 256.0);
	gl_FragColor = vec4(sin(PI2 * itter + 0.5), sin(PI2 * itter - 1.0),
		cos(PI2 * itter + 2.0), 1.0);
}
 P   0   ??
 T O U R . V E R T                 attribute vec2 Vertex;

void main()
{
	gl_Position = vec4(Vertex, 0.0, 1.0);
} 
/  8   ??
 Z O O M - H I . F R A G                   #version 400
 
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

bool between(double a, double b) {
	return a >= b && a < b + 1.0;
}

void main() {
	double select = 0.0;
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
	dvec2 c = (gl_FragCoord.xy -vec2(0.5) * resolution.xy) / vec2(resolution.x);
	// apply a zoom facor
	c.x = c.x * 3.0 / zoom;
	c.y = c.y * -3.0 / -zoom;
	// rotate c based on angle
	float cosa = cos(0.2 * angle);
	float sina = sin(0.2 * angle);
	c = dvec2(c.x * cosa - c.y * sina, c.x * sina + c.y  * cosa);
	// center c based on some input value
	c += center;
	// calculate the mandelbrot set
	dvec2 z = dvec2(0.0);
	double zxx = z.x * z.x;
	double zyy = z.y * z.y;
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
		float m = float(dot(z, z)) * 1.5;
		count = count + 1.0 - log2(0.5 * log2(m));
		count = sqrt(count / f);
		r = sin(PI2 * count + 0.5);
		g = sin(PI2 * count - 1.0);
		b = cos(PI2 * count + 2.0);
	}
	if (select > 0.0) {
		double blink = mix(0.8, 1.0, sin(time * 3.0));
		select = select * blink;
	}
	gl_FragColor = mix(vec4(r, g, b, 1.0), vec4(1.0), float(select));
}
 O   8   ??
 Z O O M - H I . V E R T                   attribute vec2 Vertex;

void main()
{
	gl_Position = vec4(Vertex, 0.0, 1.0);
}
   8   ??
 Z O O M - L O . F R A G                   #define itter 256
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
		float m = float(dot(z, z)) * 1.5;
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
	gl_FragColor = mix(vec4(r, g, b, 1.0), vec4(1.0), float(select));
}
  O   8   ??
 Z O O M - L O . V E R T                   attribute vec2 Vertex;

void main()
{
	gl_Position = vec4(Vertex, 0.0, 1.0);
}
