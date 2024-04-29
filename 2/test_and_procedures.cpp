#include "test_and_procedures.h"
using namespace std;

bool test_Miller (const vector<long long>& primeDivisors, long long n, int t)
{
    if (n == 2)
        return Number_is::prime;

    if (n % 2 == 0 or n < 2)
        return Number_is::composite;
    
    auto randNumbers = rand_numbers_from_to(1, n-1, t);

    for (auto aRnd : randNumbers)
    {
        if (pow_mod(aRnd, n - 1, n) != 1)
                return Number_is::composite;
    }

    for (auto q : primeDivisors)
    {
        auto pred = [n, &q] (long long aRnd)
        {
            return pow_mod(aRnd, (n - 1) / q, n) == 1;
        };

        if (all_of(randNumbers.begin(), randNumbers.end(), pred))
            return Number_is::composite;
    }
    return Number_is::prime;
}


bool test_Pocklington (const vector<long long>& primeDivisors_F, long long n, int t)
{
    if (n == 2)
        return Number_is::prime;

    if (n % 2 == 0 or n < 2)
        return Number_is::composite;

    auto randNumbers = rand_numbers_from_to(1, n-1, t);

    for (auto aRnd : randNumbers)
    {
        auto pred = [n, &aRnd] (long long q)
        {
            return pow_mod(aRnd, (n - 1) / q, n) == 1;
        };

        if (pow_mod(aRnd, n - 1, n) != 1)
                return Number_is::composite;

        if (not any_of(primeDivisors_F.begin(), primeDivisors_F.end(), pred))
            return Number_is::prime;
    }
    return Number_is::composite;
}


bool test_probability (long long n, int k)
{
    if (n == 2)
        return Number_is::prime;

    if (n % 2 == 0 or n < 2)
        return Number_is::composite;

    long long t = n - 1, s = 0;

    while (t % 2 == 0)
    {
        t /= 2;
        ++s;
    }

    for (int i = 0; i < k; ++i)
    {
        long long a = rand_from_to(2, n - 2);
        long long x = pow_mod(a, t, n);

        if (x == 1 or x == n - 1)
            continue;

        for (int j = 0; j < s - 1; ++j)
        {
           x = pow_mod(x, 2, n);

            if (x == 1 or x == n - 1)
                break;
        }

        if (x == 1 or x != n - 1)
            return Number_is::composite;
    }
    
    return Number_is::prime;
}

pair<long long, int>
procedure_Miller (const vector<long long>& primes, int bitSize, int t)
{
    int count = 0;

    while (true)
    {
        auto [m, primeDivisors] = rand_num_factorisation(primes, bitSize - 1);

        long long n = 2 * m + 1;
        if (m % 2 == 0)
        {
            primeDivisors.push_back(2);
        }

        if (test_Miller(primeDivisors, n, 200))
            return make_pair(n, count);

        if (test_probability(n, 6))
            ++count;
    }        
}


pair<long long, int>
procedure_Pocklington (const vector<long long>& primes, int bitSize, int t)
{
    int count = 0;

    while (true)
    {
        int bitSizeF = bitSize / 2 + 1;
        long long R = rand_from_to(pow(2, bitSizeF - 1), pow(2, bitSizeF));
        R = (R % 2 == 0) ? R : R - 1;

        auto [F, primeDivisors] = rand_num_factorisation(primes, bitSizeF);

        long long n = R*F + 1;

        if (test_Pocklington (primeDivisors, n, t))
            return make_pair (n, count);

        if (test_probability(n, 6))
            ++count;
    }  
}


long long procedure_gost (long long q, int bitSize, double (*rand_01)())
{
    while (true)
    {
        long long u = 0;
        double common = pow(2.0, bitSize - 1) / q;
        long long N = ceil(common) + ceil(common * rand_01());

        N = (N % 2 == 0) ? N : N + 1;
     
        for (long long u = 0; true; u += 2)
        {
            auto p = (N + u) * q + 1;

            if (p > pow(2, bitSize))
                break;

            if (pow_mod(2, p - 1, p) == 1 and pow_mod(2, N + u, p) != 1)
                return p;
        }       
    }
}