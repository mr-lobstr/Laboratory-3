#ifndef TESTS_AND_PROCEDURES_H
#define TESTS_AND_PROCEDURES_H

#include <set>
#include <cmath>
#include "modular_arithmetic.h"
#include "rand_func.h"

bool test_Miller (const std::vector<long long>& primeDivisors, long long n, int t);


bool test_Pocklington (const std::vector<long long>& primeDivisors_F, long long n, int t);


bool test_probability (long long n, int k);


std::pair<long long, int>
procedure_Miller (const std::vector<long long>& primes, int bitSize, int t);


std::pair<long long, int>
procedure_Pocklington (const std::vector<long long>& primes, int bitSize, int t);


long long procedure_gost (long long q, int bitSize, double (*rand_01)());

#endif