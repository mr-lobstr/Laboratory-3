#ifndef MODULAR_ARIPHMETIC_H
#define MODULAR_ARIPHMETIC_H

#include <vector>
#include <cmath>
#include <algorithm>
#include <iterator>

// возвращает вектор простых чисел до n
std::vector<long long> table_primes_to (long long n);


// возведение в степень по модулю
long long pow_mod(long long a, long long q, long long n);

#endif