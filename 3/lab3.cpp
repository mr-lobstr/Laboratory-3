#include <iostream>
#include <vector>
#include <cmath>
using namespace std;

using coffee_type = vector<pair<double, double>>;

coffee_type coffee (double Tsr, double Tc, double coefHeat, int t)
{
    coffee_type coffeeInTime;
    
    for (int i = 0; i < t; ++i)
    {
        coffeeInTime.emplace_back(i, Tc);
        Tc -= (coefHeat * (Tc - Tsr));
    }

    return coffeeInTime;
}


pair<double, double> aprox (const coffee_type& coffeeInTime)
{
    double sumXY = 0;
    double sumX = 0;
    double sumY = 0;
    double sumSqrX = 0;
    size_t n = coffeeInTime.size();

    for (auto [x, y] : coffeeInTime)
    {
        sumXY += (x * y);
        sumX += x;
        sumY += y;
        sumSqrX += pow(x, 2);
    }

    double a =  (n * sumXY - sumX * sumY) / (n * sumSqrX - pow(sumX, 2));
    double b = (sumY - a * sumX) / n;

    return make_pair(a, b);
}


double korrel (const coffee_type& coffeeInTime)
{
    double mediumX = 0;
    double mediumY = 0;
    
    for (auto [x, y] : coffeeInTime)
    {
        mediumX += x;
        mediumY += y;
    }

    mediumX /= coffeeInTime.size();
    mediumY /= coffeeInTime.size();

    double sumDxDy = 0;
    double sumSqrDx = 0;
    double sumSqrDy = 0;

    for (auto [x, y] : coffeeInTime)
    {
        sumDxDy += ((x - mediumX) * (y - mediumY));
        sumSqrDx += pow(x - mediumX, 2);
        sumSqrDy += pow(y - mediumY, 2);
    }

    return sumDxDy / sqrt(sumSqrDx * sumSqrDy);
}


int main()
{
    int t;
    double Tc, Tsr, coefHeat;
    cin >> Tsr >> Tc >> coefHeat >> t;

    auto coffeInTime = coffee(Tsr, Tc, coefHeat, t);
    auto [a, b] = aprox(coffeInTime);
    double k = korrel(coffeInTime);


    cout << "Время" << "\t" << "Температура кофе" << endl;

    for (auto [time, temperature] : coffeInTime)
    {
        cout << time << "\t" << temperature << endl;
    }

    cout << "\nЛиния апроксимации: " << "T = " << a << " * t + " << b << endl;
    cout << "\nКоэффициент корреляции " << k << endl;
}