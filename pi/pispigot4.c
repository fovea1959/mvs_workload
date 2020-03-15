#include "stdio.h"
#include "stdlib.h"
#define SCALE 10000
#define ARRINIT 2000

// from http://www.codecodex.com/wiki/Calculate_digits_of_pi#C
//
// last digit is correct with invoked w/ 10000 'digits'

void pi_digits(int digits) {
 int carry = 0;
 int arr[digits+1];
 for (int i = 0; i <= digits; ++i) 
  arr[i] = ARRINIT;
 int k = 0;
 printf("3.");
 for (int i = digits; i > 0; i -= 14) {
  int sum = 0;
  for (int j = i; j > 0; --j) {
   sum = sum * j + SCALE * arr[j];
   //printf ("SUMx %10d\n", sum);
   //printf ("J    %10d\n", j);
   arr[j] = sum % (j * 2 - 1);
   sum /= j * 2 - 1;
   //printf ("SUMy %10d\n", sum);
  }
  if (k == 0) {
   printf ("%01d", carry + sum/SCALE - 3000);
  } else {
   printf ("%04d", carry + sum / SCALE);
  } 
  k++;
  //printf ("D4   %10d\n", carry + sum / SCALE);
  //printf ("CARRY%10d\n", carry);
  //printf ("SUM  %10d\n", sum);
  carry = sum % SCALE;
 }
 printf("\n");
}

int main (int argc, char** argv) {
 int n = argc == 2 ? atoi(argv[1]) : 100;
 pi_digits(n);
 return 0;
}
