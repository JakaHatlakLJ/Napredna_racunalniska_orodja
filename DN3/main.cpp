#include <iostream>
#include <cmath>

// Calculate arctan(x) using Taylor series
double calculateArctan(double x) {
    double sum = 0.0;
    double term = x;
    double power = x * x;
    int n = 1;

    while (fabs(term) > 1e-8) {
        sum += term;
        term = -term * power / (2 * n + 1);
        power *= x * x;
        n++;
    }

    return sum;
}

// Compute e^(3x)
double calculateExp3x(double x) {
    return exp(3 * x);
}

// Trapezoidal method for integration
double trapezoidalIntegration(double (*f)(double), double lowerBound, double upperBound, int numIntervals) {
    double h = (upperBound - lowerBound) / numIntervals;
    double sum = 0.5 * (f(lowerBound) + f(upperBound));

    for (int i = 1; i < numIntervals; i++) {
        double x = lowerBound + i * h;
        sum += f(x);
    }

    return h * sum;
}

int main() {
    double pi = acos(-1.0); // value of PI
    double lowerBound = 0.0; // lower bound of the integral
    double upperBound = pi / 4.0; // upper bound of the integral
    int numIntervals = 1000; // number of intervals

    double result = trapezoidalIntegration([](double x) {
        return calculateExp3x(x) * calculateArctan(x / 2.0);
    }, lowerBound, upperBound, numIntervals);

    std::cout << "Estimated integral: " << result << std::endl;

    return 0;
}

