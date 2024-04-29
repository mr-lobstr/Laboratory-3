#include "rand_func.h"
using namespace std;

long long rand_from_to(long long a, long long b)
{
    long long mn = min(a, b), mx = max(a, b);

    std::mt19937_64 gen(chrono::system_clock::now().time_since_epoch().count());
    return gen() % (mx - mn + 1) + mn;
}


double rand_0_to_1()
{
    return rand_from_to(0, RAND_MAX) / static_cast<double>(RAND_MAX);
}


vector<long long> rand_numbers_from_to (long long min, long long max, int count)
{
    vector<long long> randNumbers (count);

    for (auto& num : randNumbers)
        num = rand_from_to(min, max);

    return randNumbers;
}


long long rand_prime_to (const vector<long long>& primes, long long maxValue)
{
    auto ind = rand_from_to (0, primes.size()-1);

    while (primes[ind] > maxValue)
    {
        ind = rand_from_to (0, ind);
    }
    return primes[ind];
};


pair<long long, vector<long long>>
rand_num_factorisation (const vector<long long>& primes, int bitSize)
{
    long long maxFactor = (1ll << bitSize) - 1;
    long long composite = 1;
    set<long long> divisors;

    while (maxFactor > 2)
    {
        long long prime = rand_prime_to (primes, maxFactor);
        long long factor = pow (prime, rand_from_to (1, 5));

        divisors.insert(prime);

        while (factor > maxFactor)
        {
            factor /= prime;
        }

        composite *= factor;
        maxFactor /= factor;
    }

    return make_pair (composite, vector<long long> (divisors.begin(), divisors.end()));
}