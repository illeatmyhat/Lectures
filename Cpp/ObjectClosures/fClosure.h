#ifndef __FCLOSURE_H
#define __FCLOSURE_H

// This is the HOW of closures. A closure over a function actually defines a class.
class fClosure {
private:
	// Note the members of this class: they are the variables captured by the closure.
	int a, b;

public:
	// The closure is constructed with deep (early) bindings to the captured values.
	fClosure(int _a, int _b) : a(_a), b(_b) {}

	// operator() is the "invoke" operator. It allows a C++ class to define what happens
	// when you () an instance of that class. We use it here to simulate a function call.
	int operator()(int x) { // calling the closure with 1 argument.
		return x + a + b;
	}
};

#endif
