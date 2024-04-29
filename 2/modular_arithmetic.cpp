#include "modular_arithmetic.h"
using namespace std;

vector<long long> table_primes_to (long long n)
{
    if (n < 2)
        return {};
    
    vector<long long> primes = {2};

    for (long long i = 3; i < n; ++i)
    {
        long long lastPrime = n - 1;
        for (auto prime : primes)
        {
            if (prime > sqrt(i) or i % prime == 0)
            {
                lastPrime = prime;
                break;
            }
        }

        if (i % lastPrime != 0)
            primes.push_back(i);
    }

    return primes;
}


long long pow_mod(long long a, long long q, long long n)
{
    int result = 1;

    for (; q > 0; q /= 2)
    {
        if (q % 2 == 1)
            result = (result * a) % n;

        a = (a * a) % n;
    }

    return result;
}