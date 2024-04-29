#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <fstream>
#include <cmath>
using namespace std;

double function (double x);

size_t columns_width (size_t titleWidth, double x0, double x1, int prec)
{
	size_t x0_len = (stringstream {} << fixed << setprecision(prec) << x0).str().size();
	size_t x1_len = (stringstream {} << fixed << setprecision(prec) << x1).str().size();

	return max(titleWidth, max(x0_len, x1_len));
}

int main()
{
	double x0 = 3.12345, x1 = 6, step = 0.1;

	string title_arg = "argument:";
	string title_res = "results:";

	size_t width = columns_width(title_arg.size(), x0, x1, 3) + 1;

	cout << "Table" << endl;
	cout << left << setw(width) << title_arg << title_res << endl;
	cout << fixed << setprecision(3);

	for (; x0 <= x1; x0 += step)
	{
		x0 = (abs(x0) < step / 10) ? 0 : x0;
		cout << left << setw(width) << x0 << function(x0) << endl;
	}	
}

double func1 (double x)
{
	return -(x + 3) / 2;
}

double func2 (double x)
{
	return 2 * cos(2 * x);
}

double func3 (double x)
{
	return (x + 3) / 2;
}

double function (double x)
{
	if (x <= -2)
	{
		return func1 (x);
	}
	else if (-2 < x and x < 2)
	{
		return func2 (x);
	}
	else
	{
		return func3 (x);
	}
}