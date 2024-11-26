#include <stdio.h>
#include <gc.h>
#include <stdlib.h>

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

    char* int_to_str(int n){
        int length = snprintf( NULL, 0, "%d", n );
        char* str = (char*)GC_malloc( length + 1 );
        snprintf( str, length + 1, "%d", n );
        return str;
    }

    char* float_to_str(float n){
        int length = snprintf( NULL, 0, "%f", n );
        char* str = (char*)GC_malloc( length + 1 );
        snprintf( str, length + 1, "%f", n );
        return str;
    }

    char* bool_to_str(int n){
        char* str;
        if (n!=0){
            str = (char*)"true";
        }else{
            str = (char*)"false";
        }
        return str;
    }
}