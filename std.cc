#include <stdio.h>
#include <gc.h>

using namespace std;

extern "C" {

    int println(int n) {
        printf("%d\n", n); // Print integer followed by a newline
        return n; // Return the integer
    }

    int printstrln(char* str){
        printf("%s\n", str);
        return 0;
    }

    int printstr(char* str){
        printf("%s", str);
        return 0;
    }

    void* my_malloc(int size) {
        return GC_malloc(size);
    }
}