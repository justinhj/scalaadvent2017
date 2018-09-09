// -*- compile-command:"g++ advent.cpp -o advent"; -*-

#include <stdio.h>

// set b 67
// set c b
// jnz a 2 LABEL1
// jnz 1 5 LABEL2
// LABEL1  mul b 100
// sub b -100000
// set c b
// sub c -17000
// LABEL2 set f 1
// set d 2
// LABEL5 set e 2
// LABEL4 set g d
// mul g e
// sub g b
// jnz g 2
// set f 0
// LABEL3 sub e -1
// set g e
// sub g b
// jnz g -8  LABEL4
// sub d -1
// set g d
// sub g b
// jnz g -13 LABEL5
// jnz f 2 LABEL6
// sub h -1
// LABEL6 set g b
// sub g c
// jnz g 2 LABEL7
// jnz 1 3 END 
// LABEL7 sub b -17
// jnz 1 -23   LABEL2
// END

int main(int argc, char **args) {

	int a = 0; // set to 1 for step 2
	int e = 0;
	int h = 0;
	int b = 67;
	int f = 0;
	int d = 0;
	int g = 0;
	int c = b;

	printf("starting\n");
	
	if(a != 0) {
		goto label1;
	}

	goto label2;

 label1:
	printf("label1\n");
	b = (b * 100);
	b = b - -100000;
	c = b;
	c = c - -17000;

 label2:

	f = 1;
	d = 2;

 label5:	
	e = 2;

 label4:
	g = d;
	g = g * e;
	g = g - b;

	if(g != 0) goto label3;
	
	f = 0;
	
 label3:
	printf("label3 f=%d\n", f);
	e = e - -1;
	g = e;
	g = g - b;

	if(g != 0) goto label4;

	d = d - -1;

	g = d;
	g = g - b;

	if(g != 0) goto label5;
	
	if(f != 0) goto label6;

	h = h - -1;
 label6:
	g = b;
	g = g - c;

	if(g != 0) goto label7;

	goto end;
 label7:
	printf("label17\n");
	b = b - -17; 
	goto label2 ;
 end:
	
	printf("haha b is %d\n", b);


}
