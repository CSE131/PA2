void main() {
	vec2 v2;
	vec3 v3;
	vec4 v4;

	v2.x;		// good
	v2.y;		// good
	v2.yx;		// good
	v2.xx;		// good

	v3.z;		// good
	v3.zy;		// good
	v3.zxy;		// good
	v3.yyy;		// good

	v4.w;		// good
	v4.xw;		// good
	v4.yxw;		// good
	v4.yxxw;	// good

	v2.xxy;		// bad
	v2.ax;		// bad

	v3.d;		// bad
	v3.kxk;		// bad
	v3.xxxx;	// bad

	v4.wwwd;	// bad
	v4.sax;		// bad
	v4.xyzww;	// bad

}