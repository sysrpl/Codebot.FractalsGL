// Mandelbrot Tour

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
