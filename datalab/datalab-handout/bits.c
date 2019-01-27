/* 
 * CS:APP Data Lab 
 * 
 * <Please put your name and userid here>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* We do not support C11 <threads.h>.  */
/* 
 * bitOr - x|y using only ~ and & 
 *   Example: bitOr(6, 5) = 7
 *   Legal ops: ~ &
 *   Max ops: 8
 *   Rating: 1
 */
int bitOr(int x, int y) {
  // x or y are true if and only if x and y are not both false
  // Demorgan's Laws
  // Turns all 0s into 1s, then compares, then switches them again
  return ~((~x) & (~y));
}
/* 
 * evenBits - return word with all even-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int evenBits(void) {
  // 0x55 in binary is 01010101
  // shift left 8 and expand it to 16 bits long
  // repeat shift with 16 to get 32 bit word of all even bits
  int word = 0x55;
  word = word | word<<8;
  return word | word<<16;
}
/* 
 * minusOne - return a value of -1 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 2
 *   Rating: 1
 */
int minusOne(void) {
  // 0 in binary is 00000000
  // flip it and it will be 11111111
  // which is signed and equals -1
  return ~0;
}
/* 
 * allEvenBits - return 1 if all even-numbered bits in word set to 1
 *   Examples allEvenBits(0xFFFFFFFE) = 0, allEvenBits(0x55555555) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int allEvenBits(int x) {
  // get the 32 bit word with even bits
  // compare if x has all even bits with the &
  // XOR on the result of that to check if all bits match the original even num
  // ! on the final because if it is all even, the XOR would return all 0s so you need to flip it
  // I would call this to get even but it's against the rules :( int even = evenBits();
  int even = 0x55;
  even = even | even<<8;
  even = even | even<<16;
  return !((x & even)^even);
}
/* 
 * anyOddBit - return 1 if any odd-numbered bit in word set to 1
 *   Examples anyOddBit(0x5) = 0, anyOddBit(0x7) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int anyOddBit(int x) {
  // get and odd mask
  // (x & odd) to see if any of the odd digits are 1
  // !! double bang to change this to a 1 or zero depending on if there are any odd digits that are 1
  int odd = 0xAA;
  odd = odd | odd<<8;
  odd = odd | odd<<16;
  return !!(x & odd);
}
/* 
 * byteSwap - swaps the nth byte and the mth byte
 *  Examples: byteSwap(0x12345678, 1, 3) = 0x56341278
 *            byteSwap(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= n <= 3, 0 <= m <= 3
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 2
 */
int byteSwap(int x, int n, int m) {
  int t = 0;
  // left shift n and m 3 because they have to be between 0 and 3
  n = n<<3;
  m = m<<3;
  // AND compare 0xff to a (right shifted x by n and XOR x>>m)
  // do this in order to get the difference between the shifted bits
  // then AND compare it to 0xff to fill remaining bits
  t = 0xff & ((x>>n) ^ (x>>m));
  // swap the bits in x based on what you've gotten for t
  // XOR in order to get the bits that are in t<<n and t<<m into X
  // XOR instead of OR because we want the changed bits
  x = x^(t<<n);
  x = x^(t<<m);
  return x;
}
/* 
 * addOK - Determine if can compute x+y without overflow
 *   Example: addOK(0x80000000,0x80000000) = 0,
 *            addOK(0x80000000,0x70000000) = 1, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int addOK(int x, int y) {
  // checks is there is overflow from the biggest digits, hence the >>31
  int firstCheck = !((x>>31) ^ (y>>31));
  // checks if there is any overflow from the addition of the two and
  int secondCheck = ((x+y)>>31) ^ (x>>31);
  // if both of these checks returns 0 then adding is OK
  return !(firstCheck & secondCheck);
}
/* 
 * conditional - same as x ? y : z 
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int conditional(int x, int y, int z) {
  // checks for zero or one
  // makes if true: -2
  // makes if false: -1
  int mask1 = !!x;
  // makes if true: -1
  // makes if false: 0
  int mask2 = ~mask1 + 1;
  // if mask2 is 0 then (mask2 & y) is 0 so it returns (~mask2 & z)
  return (mask2 & y) + (~mask2 & z);
}
/* 
 * isAsciiDigit - return 1 if 0x30 <= x <= 0x39 (ASCII codes for characters '0' to '9')
 *   Example: isAsciiDigit(0x35) = 1.
 *            isAsciiDigit(0x3a) = 0.
 *            isAsciiDigit(0x05) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 3
 */
int isAsciiDigit(int x) {
  // adds the minimum ascii number to it
  int a = x + (~0x2f);
  // gets last digit of number to see if it is larger than 0x2f
  a = a >> 31;
  // adds maximum ascii number to it
  int b = 58 + ~x;
  // gets last digit of number to see if it is less than 0x39
  b = b >> 31;
  // returns 1 if both a and b are 0s
  return !(a|b);
}
/* 
 * replaceByte(x,n,c) - Replace byte n in x with c
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: replaceByte(0x12345678,1,0xab) = 0x1234ab78
 *   You can assume 0 <= n <= 3 and 0 <= c <= 255
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 3
 */
int replaceByte(int x, int n, int c) {
  // get all 1s
  int mask = 0xFF;
  // shift n because it is between 0 and 3
  int rep = n << 3;
  // shift the mask by rep in order to get new mask without old bytes
  mask = ~(mask << rep);
  // shift c by rep to get it ready to be inserted
  c = c << rep;
  // take out old byte and replace it with new one
  return (x & mask) | c;
}
/* reverseBits - reverse the bits in a 32-bit integer,
              i.e. b0 swaps with b31, b1 with b30, etc
 *  Examples: reverseBits(0x11111111) = 0x88888888
 *            reverseBits(0xdeadbeef) = 0xf77db57b
 *            reverseBits(0x88888888) = 0x11111111
 *            reverseBits(0)  = 0
 *            reverseBits(-1) = -1
 *            reverseBits(0x9) = 0x90000000
 *  Legal ops: ! ~ & ^ | + << >> and unsigned int type
 *  Max ops: 90
 *  Rating: 4
 */
int reverseBits(int x) {
  // make it unsigned first
  unsigned reverse_x = x;

  // create masks for each time you need to reverse the bits
  int mask_08_switch = 0xFF | (0xFF << 16);
  int mask_04_switch = mask_08_switch ^ (mask_08_switch << 4);
  int mask_02_switch = mask_04_switch ^ (mask_04_switch << 2);
  int mask_01_switch = mask_02_switch ^ (mask_02_switch << 1);

  // reverse the bits at each different bit step (1,2,4,8,16)
  reverse_x = (reverse_x >> 16) | (reverse_x << 16);
  reverse_x = ((reverse_x & ~mask_08_switch) >>  8) | ((reverse_x & mask_08_switch) <<  8);
  reverse_x = ((reverse_x & ~mask_04_switch) >>  4) | ((reverse_x & mask_04_switch) <<  4);
  reverse_x = ((reverse_x & ~mask_02_switch) >>  2) | ((reverse_x & mask_02_switch) <<  2);
  reverse_x = ((reverse_x & ~mask_01_switch) >>  1) | ((reverse_x & mask_01_switch) <<  1);

  return reverse_x;
}
/*
 * satAdd - adds two numbers but when positive overflow occurs, returns
 *          maximum possible value, and when negative overflow occurs,
 *          it returns minimum positive value.
 *   Examples: satAdd(0x40000000,0x40000000) = 0x7fffffff
 *             satAdd(0x80000000,0xffffffff) = 0x80000000
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 30
 *   Rating: 4
 */
int satAdd(int x, int y) {
  // combination of logic from addOk and conditional
  // adds the two numbers
  int xPlusY = x + y;
  // left shifts by 31 to get negOverflow
  int negOverflow = 0x1 << 31;
  // flips negOverflow to get posOverflow
  int posOverflow = ~negOverflow;
  // creates a mask of the overflow value if there is one
  int overflowMask = ((x ^ xPlusY) & (y ^ (xPlusY))) >> 31;
  // gets whether overflow is positive or negative
  int signMask = x >> 31;
  // conditional on if x and y can be added but if they cannot be added then it returns the overflow value
  return ((xPlusY) & ~overflowMask) | (overflowMask & ((signMask & negOverflow) | (~signMask & posOverflow)));
}
/*
 * Extra credit
 */
/* 
 * float_abs - Return bit-level equivalent of absolute value of f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument..
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_abs(unsigned uf) {
  // mask the sign bit then check if all exponent bits are set to one
  unsigned absUf = uf & 0x7fffffff;

  // if the frac bits are some value, the number is NaN. Return it.
  // Otherwise just return the absolute.
  if ((absUf & 0x7f800000) ^ 0x7f800000) {
    return absUf;
  }
  else {
    if (absUf << 9) {
      return uf;
    }
    else {
      return absUf;
    }
  }
}
/* 
 * float_f2i - Return bit-level equivalent of expression (int) f
 *   for floating point argument f.
 *   Argument is passed as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point value.
 *   Anything out of range (including NaN and infinity) should return
 *   0x80000000u.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
int float_f2i(unsigned uf) {
  /* mask out the numerator and the exponent. E can be calculated by subtracting
   * the bias from exponent. The int is then the fraction bit shifted by 23 - E,
   * test for E = 0 and it is pretty easy to see (will get 1). The direction of the
   * shift is chosen based on if 23 - exponent is greater than or less than 0. Next, if E is
   * less than 0 then the int will always be 0. If E is greater than 30 then overflow
   * will occur. Finally, if the sign is negative return negative with ~(x) + 1, otherwise
   * return the first calculated value.
   */
  unsigned numerator = (uf & 0x7FFFFF) + 0x800000;
  int exponent = ((uf & 0x7f800000) >> 23) - 127;
  int eShift = 23 - exponent;
  unsigned ufti = numerator;

  if(eShift >= 0)
    ufti = ufti >> eShift;
  else
    ufti = ufti << (~eShift + 1);

  if (exponent < 0)
    return 0;
  if (exponent > 30)
    return 0x80000000;
  if (uf & 0x80000000)
    return ~ufti + 1;
  else 
    return ufti;
}
/* 
 * float_half - Return bit-level equivalent of expression 0.5*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_half(unsigned uf) {
  /* Mask out relevant terms. Chuck out the case where uf is NaN or inf. Now if the exponent
   * is greater than one then subtracting one will divid uf by 2. If the exponent is less
   * or equal to one then the frac can simply be shifted over to the right by one to divide
   * uf by 2. Note that it is odd that the normalized case of 0000 0001 is grouped with the
   * denormalized cases, but it makes since E is the same for the denormalized and for 
   * 0000 0001. A rounder value is added in the case that the fraction needs to be rounded to
   * even. The case occurs when the two least significant bits are both set to 1.
   */
  unsigned fraction = uf & 0x7fffff;
  unsigned exponent = uf & 0x7f800000;
  unsigned sign = uf & 0x80000000;
  unsigned rounder = 0;

  if (!((uf & 0x7f800000) ^ 0x7f800000))
    return uf;

  if (exponent > 0x800000){
    return sign + (exponent - 0x800000) + fraction;
  }
  else {
    if ((fraction & 0x3) ^ 0x3)
      rounder = 0;
    else
      rounder = 1;
    return sign + ((exponent + fraction) >> 1) + rounder;
  }
}
