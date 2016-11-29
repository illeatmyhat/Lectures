#include <iostream>
#include <functional>
#include <algorithm>

#include "fClosure.h"

std::function<int(int)> GetClosure();
std::function<int(int)> GetFClosure();

int main() {
	std::function<int(int)> func = GetClosure();
	// Here, func is a copy of the closure created by GetClosure. 
	// We can invoke it like a function and everything works fine.
	std::cout << func(10) << std::endl;
	// But how does this really work? We know the WHAT of closures...
	// Now for the HOW.







	// To demonstrate our object closure:
	std::function<int(int)> closure = GetFClosure();
	std::cout << closure(10) << std::endl;
}

std::function<int(int)> GetClosure() {
	int a = 1, b = 2;

	// Create an anonymous function. This function captures the local a and b variables by
	// value (deep binding). The function is then returned (copied by value). 
	auto f = [a, b](int x) {
		return x + a + b;
	};
	return f;
}

std::function<int(int)> GetFClosure() {
	int a = 1, b = 2;
	return fClosure(a, b);
}