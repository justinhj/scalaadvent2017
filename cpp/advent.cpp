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

	// TEMP HACK view smaller runs by changing the bounding variables b and c
	//	b = 17;
	//c = 85;
	
	printf("start a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//	std::cin.get();
	
	// Code never comes back here
	// b=106700, c=123700

	// diff between b and c is 17000
	
 label2: // FOR D = 2 to B, we're going to check all D's to see if they are prime or not
	     // B starts at 106700 and continues until it is 123700

	// will loop back here until g is zero 
	
	//	printf("label2 a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//	std::cin.get();
	f = 1; // reset the f flag (f is cleared if the (d*e) == current b
	d = 2;

 resetE:
	//	printf("resetE a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//std::cin.get();
	e = 2;

	// e starts at 
	
 label4:

	// loops back here until g is zero

	// will find if d is a factor of b (106700) by increasing values of e
	// if d * e = b then we found a factor 
	
	//	printf("label4 a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//std::cin.get();

	g = d;
	g = g * e; mul ++;
	g = g - b;        // check if (d*e) == b

	//	if(g != 0) goto nextE;  // if it is NOT go to label 3

    if(b%d != 0) {
		goto nextE;
	}
	
	printf("Clear F because d(%d) divides into b(%d) e(%d) times  a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", d,b,e,a,b,c,d,e,f,g,h);
	printf("b(%d) mod d(%d) = %d\n", b, d, b%d);
	//    std::cin.get();

	f = 0; 
	
	
 nextE:
	//	printf("nextE a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//std::cin.get();

	e = e - -1;  // increment e 
	g = e;
	g = g - b;

	//	if(g != 0) goto label4; // CHECK IF WE CHECKED ALL NUMBERS LESS THAN e

	// DONE CHECKING UP TO e
	
	d = d - -1; // GET THE NEXT d

	g = d;
	g = g - b; // check if d == b, we're done checking all the d's 

	if(g != 0) goto resetE; // set e to 2 again , try all the e's

	if(f == 0) {
		printf("WILL INC H a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);

		//std::cin.get();
	}
	else {
		//		printf("WILL NOT INC H a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);

		//std::cin.get();

	}
	
	if(f != 0) goto label6; // f is set to zero if we found a factor

	h = h - -1; // increment h
		
	//	printf("INC H a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d\n", a,b,c,d,e,f,g,h);
	//std::cin.get();
	

 label6:
	printf("label6 f=%d g=%d, h=%d, b=%d e=%d\n", f, g, h, b, e);
	//std::cin.get();
		
	g = b;
	g = g - c; // CHECK IF WE CHECKED ALL THE NUMBERS (d)

	if(g != 0) goto label7; // will exit when g is zero

	goto end;
 label7:
	printf("label7 NEXT B  f=%d g=%d, h=%d, b=%d e=%d\n", f, g, h, b, e);
	//std::cin.get();
		
	b = b - -17; // counts up 17 at a time 
	goto label2 ;
 end:
	
	//	printf("mul count is %d\n", mul);
	printf("h is %d\n", h);


}
