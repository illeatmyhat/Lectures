#include <stdio.h>

int main() {
	int intValue = 1135846729;
	// This four-byte integer value is saved somewhere on the stack. When we use "intValue",
	// the type system knows that it is of type "int", and that is used to emit meaningful
	// CPU instructions to do integer arithmetic.

	int *pToInt = &intValue;
	printf("*pToInt = %d\n", *pToInt);

	// pToInt contains the address of intValue, and when dereferenced, will read a 4-byte
	// integer value from the address that it points to.

	// C is very type-unsafe when it comes to pointers, and allows things like this:
	float *pToFloat = (float*)&intValue;

	// pToFloat contains the address of intValue, but intValue is not a 4-byte float. When we
	// dereference pToFloat, the computer will attempt to read a 4-byte float from the address
	// that pToFloat points to. Since there are indeed 4 bytes there, the program won't crash...
	// it will just interpret those 4 bytes as if they were for a floating point number.

	printf("%d interpreted as a float = %f\n", *pToInt, *pToFloat);


	// To obtain dynamic memory, we use malloc, which requires knowing the # of bytes to allocate.
	int *oneInt = (int*)malloc(sizeof(int));
	// I now own this memory and can use it how I see fit.
	*oneInt = 100;
	*(float*)oneInt = 3.14159;

	// Arrays are allocated this way, too.
	int *arrayInt = (int*)malloc(10 * sizeof(int));
	arrayInt[0] = 100;
	arrayInt[1] = 90;
	// etc.

	// The key observation is that malloc gives us a chunk of X bytes and then doesn't care
	// what we do with it. The C type system limits the values that can be assigned to 
	// that chunk, but that type system is very flexible...

	char *someValues = (int*)malloc(sizeof(int) + sizeof(float));
	// The compiler only knows this is a pointer to a character. Secretly it points to 8 bytes.
	// I can manipulate those bytes however I want.
	*(int*)someValues = 100; // Draw this out.

	// someValues points to a block of 8 bytes. If I tell the compiler it actually points to
	// an int, then I can assign an int to the first 4 of those bytes.

	// What if I want to assign to another part of the block? Pointer arithmetic!
	// "someValues + x" will address memory at "x" increments past wherever someValues starts,
	// where one "increment" is the size of the thing someValues points to -- 1 byte in this case.

	*(float*)(someValues + 4) = 3.14159f;

	// I can now print those values as if they were really int/float.
	printf("Address %p has int value %d; address %p has float value %f\n",
		someValues, *(int*)someValues, someValues + 4, *(float*)(someValues + 4));

}