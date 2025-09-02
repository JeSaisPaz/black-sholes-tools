#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define pi (355.0 / 113.0)

// Approximation for e
const double e = 2.71828;

// Function prototypes
double nPrime(double term);
double cdfApprox(double x);
double optionCalculator(double stockPrice, double strikePrice, double interestRate, double timeToExpiration, double stockVolatility, int optChoice);
double calculateHistoricalVolatility(const char *filename);
void greekAnalysis(double stockPrice, double strikePrice, double interestRate, double timeToExpiration, double stockVolatility);

// =============================
// Main Program
// =============================
int main() {
    double stockPrice, strikePrice, interestRate, timeToExpiration, stockVolatility;
    int optChoice, analysisChoice;
    char fileName[32];

    printf("Stock price: ");
    scanf("%lf", &stockPrice);
    printf("Strike price: ");
    scanf("%lf", &strikePrice);
    printf("Interest rate (e.g., 0.05 for 5%%): ");
    scanf("%lf", &interestRate);
    printf("Time to Expiration (in years, e.g., 0.5 for 6 months): ");
    scanf("%lf", &timeToExpiration);
    printf("CSV file containing 1 price per line: ");
    scanf("%31s", fileName);
    
    // Calculate historical volatility from file
    stockVolatility = calculateHistoricalVolatility(fileName);

    if (stockVolatility < 0) {
        printf("Failed to calculate volatility.\n");
        return 1;
    }

    printf("Do you want Greek analytics? (1 for yes, 0 for no): ");
    scanf("%d", &analysisChoice);

    if (analysisChoice == 1) {
        greekAnalysis(stockPrice, strikePrice, interestRate, timeToExpiration, stockVolatility);
    }

    printf("1: Call or 2: Put: ");
    scanf("%d", &optChoice);

    double optionPrice = optionCalculator(stockPrice, strikePrice, interestRate, timeToExpiration, stockVolatility, optChoice);
    if (optionPrice >= 0) {
        printf("Option Price: %.4f\n", optionPrice);
    }

    return 0;
}

// =============================
// Standard Normal PDF
// =============================
double nPrime(double x) {
    return (1.0 / sqrt(2.0 * pi)) * exp(-0.5 * x * x);
}

// =============================
// CDF Approximation (Abramowitz & Stegun)
// =============================
double cdfApprox(double x) {
    const double p = 0.2316419;
    const double a1 = 0.319381530;
    const double a2 = -0.356563782;
    const double a3 = 1.781477937;
    const double a4 = -1.821255978;
    const double a5 = 1.330274429;

    double k = 1.0 / (1.0 + p * fabs(x));
    double k_sum = a1 * k + a2 * pow(k, 2) + a3 * pow(k, 3) + a4 * pow(k, 4) + a5 * pow(k, 5);
    double cnd = 1.0 - nPrime(x) * k_sum;

    return (x < 0.0) ? (1.0 - cnd) : cnd;
}

// =============================
// Black-Scholes Option Calculator
// =============================
double optionCalculator(double stockPrice, double strikePrice, double interestRate, double timeToExpiration, double stockVolatility, int optChoice) {
    double d1 = (log(stockPrice / strikePrice) + (interestRate + 0.5 * pow(stockVolatility, 2)) * timeToExpiration) / (stockVolatility * sqrt(timeToExpiration));
    double d2 = d1 - stockVolatility * sqrt(timeToExpiration);

    if (optChoice == 1) {
        // Call
        return stockPrice * cdfApprox(d1) - strikePrice * exp(-interestRate * timeToExpiration) * cdfApprox(d2);
    } else if (optChoice == 2) {
        // Put
        return strikePrice * exp(-interestRate * timeToExpiration) * cdfApprox(-d2) - stockPrice * cdfApprox(-d1);
    } else {
        printf("Invalid option choice.\n");
        return -1;
    }
}

// =============================
// Historical Volatility Calculator
// =============================
double calculateHistoricalVolatility(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        return -1.0;
    }

    double prices[1000];
    int n = 0;
    while (fscanf(file, "%lf", &prices[n]) == 1 && n < 999) {
        n++;
    }
    fclose(file);

    if (n < 2) {
        fprintf(stderr, "Error: At least 2 prices required.\n");
        return -1.0;
    }

    double logReturns[999], sum = 0.0, mean = 0.0, variance = 0.0;

    for (int i = 1; i < n; i++) {
        logReturns[i - 1] = log(prices[i] / prices[i - 1]);
        sum += logReturns[i - 1];
    }
    mean = sum / (n - 1);

    for (int i = 0; i < n - 1; i++) {
        variance += pow(logReturns[i] - mean, 2);
    }
    variance /= (n - 2);

    double volatility = sqrt(variance) * sqrt(252.0);  // Annualized
    return volatility;
}

// =============================
// Greeks Analysis
// =============================
void greekAnalysis(double stockPrice, double strikePrice, double interestRate, double timeToExpiration, double stockVolatility) {
    double d1 = (log(stockPrice / strikePrice) + (interestRate + 0.5 * pow(stockVolatility, 2)) * timeToExpiration) / (stockVolatility * sqrt(timeToExpiration));
    double d2 = d1 - stockVolatility * sqrt(timeToExpiration);

    double deltaCall = cdfApprox(d1);
    double deltaPut = deltaCall - 1.0;
    double gamma = nPrime(d1) / (stockPrice * stockVolatility * sqrt(timeToExpiration));
    double thetaCall = - (stockPrice * nPrime(d1) * stockVolatility) / (2.0 * sqrt(timeToExpiration))
                       - interestRate * strikePrice * exp(-interestRate * timeToExpiration) * cdfApprox(d2);
    double thetaPut = - (stockPrice * nPrime(d1) * stockVolatility) / (2.0 * sqrt(timeToExpiration))
                      + interestRate * strikePrice * exp(-interestRate * timeToExpiration) * cdfApprox(-d2);
    double vega = stockPrice * nPrime(d1) * sqrt(timeToExpiration);
    double rhoCall = strikePrice * timeToExpiration * exp(-interestRate * timeToExpiration) * cdfApprox(d2);
    double rhoPut = -strikePrice * timeToExpiration * exp(-interestRate * timeToExpiration) * cdfApprox(-d2);

    printf("\n===== Option Greeks Explanation =====\n\n");

    printf("Delta (Call):     %.6f\n", deltaCall);
    printf("  → Measures how much the CALL option price changes when the stock price increases by $1.\n\n");

    printf("Delta (Put):      %.6f\n", deltaPut);
    printf("  → Measures how much the PUT option price changes when the stock price increases by $1.\n\n");

    printf("Gamma:            %.6f\n", gamma);
    printf("  → Measures how much Delta changes when the stock price increases by $1.\n");
    printf("    (Same for both calls and puts — higher gamma = more sensitivity.)\n\n");

    printf("Theta (Call):     %.6f\n", thetaCall);
    printf("  → Measures how much value the CALL option loses per year due to time decay.\n\n");

    printf("Theta (Put):      %.6f\n", thetaPut);
    printf("  → Measures how much value the PUT option loses per year due to time decay.\n\n");

    printf("Vega:             %.6f\n", vega);
    printf("  → Measures how much the option price changes when implied volatility increases by 1%%.\n\n");

    printf("Rho (Call):       %.6f\n", rhoCall);
    printf("  → Measures how much the CALL option price changes when interest rates rise by 1%%.\n\n");

    printf("Rho (Put):        %.6f\n", rhoPut);
    printf("  → Measures how much the PUT option price changes when interest rates rise by 1%%.\n\n");

    printf("======================================\n");
}
