#include <iostream>
#include <iomanip>
#include <sstream>
#include "test_and_procedures.h"
#include "check_tests.h"
using namespace std;

void check_tests (ostream& out, int iterations)
{
    ifstream f1 ("check/composition.txt");
    ifstream f2 ("check/Miller.txt");
    ifstream f3 ("check/Pocklington.txt");
    ifstream f4 ("check/gost.txt");

    auto primes = table_primes_to(6000);

    out << "Проверка таблицей составных чисел" << endl;
    out << string (80, '-') << endl;
    check_composite(f1, out, primes);

    out << "\nПроверка теста Миллера" << endl;
    out << string (80, '-') << endl;
    check_Miller (f2, out, primes, iterations);

    out << "\nПроверка теста Поклингтона" << endl;
    out << string (80, '-') << endl;
    check_Pocklington (f3, out, primes, iterations);

    out << "\nПроверка теста по ГОСТу" << endl;
    out << string (80, '-') << endl;
    check_gost (f4, out);

    f1.close();
    f2.close();
    f3.close();
    f4.close();
}

int main()
{
    auto primes500 = table_primes_to(500);
    int bitSize = 13, t = 5, count = 10;
    int numsLen = (stringstream {} << pow(2, bitSize)).str().size();

    for (int i = 0; i < count; ++i)
    {
        auto [num, k] = procedure_Miller(primes500, bitSize, t);

        char testProbRes = test_probability(num, 6) ? '+' : '-';

        cout << setw(2) << right << i + 1 << ")";
        cout << setw(numsLen) << right << num << " " << testProbRes << " " << k << endl;
    }

    int iterations = 1000;
    ofstream out ("result_check.txt");

    check_tests (out, iterations);

    out.close();
}