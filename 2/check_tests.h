#ifndef CHECK_TESTS_H
#define CHECK_TESTS_H

#include <fstream>
#include <vector>
#include "test_and_procedures.h"

void check_composite (std::istream& in, std::ostream& out, const std::vector<long long>& primes);


void check_Miller (std::istream& in, std::ostream& out, const std::vector<long long>& primes, int iterations);


void check_Pocklington (std::istream& in, std::ostream& out, const std::vector<long long>& primes, int iterations);


void check_gost (std::istream& in, std::ostream& out);

#endif