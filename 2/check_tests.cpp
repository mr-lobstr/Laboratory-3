#include "check_tests.h"
using namespace std;

vector<long long> prime_divisors (const vector<long long>& primes, long long n)
{
    vector<long long> primeDivisors;

    for (auto prime : primes)
    {
        if (prime > n)
            return primeDivisors;

        if (n % prime == 0)
        {
            
            primeDivisors.push_back(prime);
        }
    }

    return primeDivisors;
}


void check_composite (istream& in, ostream& out, const vector<long long>& primes)
{
    long long number, R;

    while (in >> number >> R)
    {
        auto divisors = prime_divisors (primes, number - 1);
        auto divisors_F = prime_divisors (primes, (number - 1) / R);

        char res1 = test_Miller(divisors, number, 1) ? '+' : '-';
        char res2 = test_Pocklington(divisors_F, number, 1) ? '+' : '-';

        out << res1 << " " << res2 << endl;
    }
}


void check_Miller (istream& in, ostream& out, const vector<long long>& primes, int iterations)
{
    long long number;

    while (in >> number)
    {
        auto divisors = prime_divisors (primes, number - 1);
        double count_false = 0;

        for (int i = 0; i < iterations; ++i)
        {
            if (not test_Miller(divisors, number, 1))
                ++count_false;
        }

        out << count_false / iterations << endl;
    }
}


void check_Pocklington (istream& in, ostream& out, const vector<long long>& primes, int iterations)
{
    long long number, R;

    while (in >> number >> R)
    {
        auto divisors_F = prime_divisors (primes, (number - 1) / R);

        double count_false = 0;

        for (int i = 0; i < iterations; ++i)
        {
            if (not test_Pocklington(divisors_F, number, 1))
                ++count_false;
        }

        out << count_false / iterations << endl;
    }
}


double func_null()
{
    return 0;
}

void check_gost (istream& in, ostream& out)
{
    long long q, t;

    while (in >> q >> t)
    {
        out << procedure_gost(q, t, func_null) << endl;
    }
}