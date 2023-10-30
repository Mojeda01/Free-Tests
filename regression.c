// Custom statistics and regression c script
// Date: 27.06.2023
// Writing this while studying Financue.

#include <stdio.h>
#include <math.h>

// Sample Average Formula Function, this is the same as mean
double sample_average(int arr[], int size){
	double sum = 0.0;
	for (int i = 0; i < size; i++){
		sum += arr[i];
	}
	return sum / size;
}

// Variance formula function, also remember size=n
double calculate_variance(int arr[], int size){
	double mean = sample_average(arr, size);
	double sum = 0.0;

	for (int i = 0; i < size; i++){
		sum += (arr[i] - mean) * (arr[i] - mean);
	}
	return sum / size;
}

// Standard Deviation Formula Function
double standard_deviation(int arr[], int size){
	return sqrt(sample_average(arr, size));
}

// Covariance Deviation Formula Function
double covariance(int x[], int y[], int size){
	double mean_x = sample_average(x, size);
	double mean_y = sample_average(y, size);
	double sum = 0.0;

	for (int i = 0; i < size; i++){
		sum += (x[i] - mean_x) * (y[i] - mean_y);
	}
	return sum / size;
}

// Correlation Formula Function, and size=n
double correlation(int x[], int y[], int size){
	double cov = covariance(x, y, size);
	double std_dev_x = standard_deviation(x, size);
	double std_dev_y = standard_deviation(y, size);

	return cov / (std_dev_x * std_dev_y);
}
// ******************************************************************************************
// Regression Test

// A simple structure to hold slope and y-intercept
typedef struct {
	double m;
	double b;
} LinearModel;

// Implementing linear regression using least squares method
LinearModel linear_regression(double x[], double y[], int size){
	double sum_x = 0.0, sum_y = 0.0, sum_x2 = 0.0, sum_xy = 0.0;

	for (int i = 0; i < size; i++){
		sum_x += x[i];
		sum_y += y[i];
		sum_x2 += x[i] * x[i];
		sum_xy += x[i] * y[i];
	}

	double m = (size * sum_xy - sum_x * sum_y) / (size * sum_x2 - sum_x * sum_x);
	double b = (sum_y - m * sum_x) / size;

	LinearModel model = {m, b};
	return model;

}


int main(){
	
	// Testing sample_average() function
	int sequence[] = {1, 2, 3, 4, 5};
	int size = sizeof(sequence) / sizeof(sequence[0]);	//	This is essentially n
	double avg = sample_average(sequence, size);

	printf("S_average: Sequence[] =  %f \n", avg);

	// Testing the cov, corr, std funcs
	int x[] = {1, 2, 3, 4, 5};
	int y[] = {2, 3, 5, 7, 10};
	int size_2 = sizeof(x) / sizeof(x[0]); // size=n
	
	printf("\n");
	printf("Standard Dev of X: %f \n", calculate_variance(x, size_2));
	printf("Covariance of X and Y: %f \n", covariance(x, y, size_2));
	printf("Correlation of X and Y: %f \n", correlation(x, y, size_2));

	// Testing Linear Regression functions
	double x2[] = {1, 2, 3, 4, 5};
	double y2[] = {2, 4, 6, 8, 10};
	int size2 = sizeof(x2) / sizeof(x2[0]);
	LinearModel model = linear_regression(x2, y2, size2);

	printf("Linear Regression: y = %fx + %f \n", model.m, model.b);

	return 0;
}
