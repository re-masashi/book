#include <stdio.h>
#include <unordered_map>
#include <string>
#include <iostream>
#include <variant>
#include <memory>
using namespace std;

extern "C" {

    int println(int n) {
        printf("%d\n", n); // Print integer followed by a newline
        return n; // Return the integer
    }

    int index_arr(int arr[], int i) {
        return arr[i]; // Return the value at index i in the array arr
    }
}