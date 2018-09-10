// -*- compile-command:"g++ advent.cpp -o advent"; -*-

#include <stdio.h>
#include <iostream>


/*

Register usage notes

b starts at 106700
c starts at 123700 (+ 106700 117000)
d starts at 2
e starts at 2, is incremented in l3 by 1
f starts at 1, set to 0 by g 

g resets at l4 to d * e then - b
  if it's zero then f is set to 0 

  resets in l3 to (e - b)



loop starts with b=106700 and c=123700

l2

 f=1 and d=2 

l5

 e = 2

l4 

  g = d (d starts at zero)

  g *= e (2)

  subtract b from g (106700) 

  if g != 0 go to l3

  f = 0 

l3 

  e = e - -1 
  g = (e - b)

  if(g != 0) got to l4

  


 */


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

	int a = 1; // set to 1 for step 2
	int b = 67;
	int c = b;
	int d = 0;
	int e = 0;
	int f = 0;
	int g = 0;
	int h = 0;
	
	int mul = 0;
	
	printf("starting\n");
	
	if(a != 0) {
		goto label1;
	}

	goto label2;

 label1:
	printf("label1\n");

	b = (b * 100); mul ++;
	b = b - -100000;
	c = b;
	c = c - -17000;

	printf("start a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//	std::cin.get();
	
	// Code never comes back here
	// b=106700, c=123700

	// diff between b and c is 17000
	
 label2:

	// will loop back here until g is zero 
	
	printf("label2 a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//	std::cin.get();
	f = 1;
	d = 2;

 label5:
	printf("label5 a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//	std::cin.get();
	e = 2;

	// e starts at 
	
 label4:

	// loops back here until g is zero

	// will find if d is a factor of b (106700) by increasing values of e
	// if d * e = b then we found a factor 
	
	printf("label4 a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//	std::cin.get();
	g = d;
	g = g * e; mul ++;
	g = g - b;        // check if (d*e) == b

	if(g != 0) goto label3;  // if it is NOT go to label 3

	printf("RESET F  a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//std::cin.get();

	f = 0; 
	
	
 label3:
	printf("label3 a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//	std::cin.get();

	e = e - -1;  // increment e 
	g = e;
	g = g - b;

	if(g != 0) goto label4; // CHECK IF WE CHECKED ALL NUMBERS LESS THAN e

	// DONE CHECKING UP TO e
	
	d = d - -1; // NEXT d

	g = d;
	g = g - b; // check if d == b, we're done checking all the d's 

	if(g != 0) goto label5; // set e to 2 again , try all the e's
	
	if(f != 0) goto label6; // f is set to zero if we found a factor

	h = h - -1; // increment h
		
	printf("INC H f=%d g=%d, h=%d, b=%d e=%d\n", f, g, h, b, e);
	std::cin.get();
	

 label6:
	printf("label6 f=%d g=%d, h=%d, b=%d e=%d\n", f, g, h, b, e);
	std::cin.get();
		
	g = b;
	g = g - c; // CHECK IF WE CHECKED ALL THE NUMBERS 

	if(g != 0) goto label7; // will exit when g is zero

	goto end;
 label7:
	printf("label7 f=%d g=%d, h=%d, b=%d e=%d\n", f, g, h, b, e);
	std::cin.get();
		
	b = b - -17; // counts up 17 at a time 
	goto label2 ;
 end:
	
	printf("mul count is %d\n", mul);
	printf("h is %d\n", h);


}
