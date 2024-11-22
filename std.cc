#include <stdio.h>
using namespace std;

extern "C" {

    int println(int n) {
        printf("%d\n", n); // Print integer followed by a newline
        return n; // Return the integer
    }

    int printstr(char* str){
        printf("%s\n", str);
        return 0;
    }

}