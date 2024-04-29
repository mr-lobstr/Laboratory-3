#ifndef RANDOM_FUNCTIONS_H
#define RANDOM_FUNCTIONS_H

#include <cmath>
#include <set>
#include <vector>
#include <random>
#include <chrono>

// возвращает случайное число из промежутка [0, 1]
double rand_0_to_1();


// возвращает случайное целое из промежутка [a, b]
long long rand_from_to(long long a, long long b);


// возвращает набор из count целых случайных чисел из промежутка [a, b]
std::vector<long long> rand_numbers_from_to (long long a, long long b, int count);


// возвращает случайное простое число из primes, не превышающее maxValue
long long rand_prime_to (const std::vector<long long>& primes, long long maxValue);

// генерирует случайное составное число размера bitSize
// возвращает само число и все его простые делители
std::pair<long long, std::vector<long long>>
rand_num_factorisation (const std::vector<long long>& primes, int bitSize);

#endif