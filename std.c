#include <stdio.h>
#include <gc.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <assert.h>

#ifndef VEC_INIT_CAPACITY
#define VEC_INIT_CAPACITY 64
#endif

struct Response{
    char* resp;
    int close;
};

typedef struct Vec {
    void **items;
    int size;
    int capacity;
} Vec;


    void Vec_init(Vec *vec) {
        vec->capacity = VEC_INIT_CAPACITY;
        vec->size = 0;
        vec->items = GC_malloc(sizeof(void *) * vec->capacity);
    }

    void Vec_init_with_cap(Vec *vec, int cap) {
        vec->capacity = cap;
        vec->size = 0;
        vec->items = GC_malloc(sizeof(void *) * vec->capacity);
    }

    void* Vec_at(Vec *vec, int index) {
        assert(index < vec->size && index >= 0);
        return *(vec->items + sizeof(void *) * index);
    }

    void Vec_resize(Vec *vec, int new_size) {
        assert(new_size > 0);
        vec->capacity = new_size;
        void **new_items = malloc(sizeof(void *) * vec->capacity);
        new_items = vec->items;
        free(vec->items);
        vec->items = new_items;
    }


    void Vec_push(Vec *vec, void *item) {
        if (vec->size == vec->capacity) {
            Vec_resize(vec, vec->capacity * 2);
        }
        *(vec->items + sizeof(void *) * vec->size) = item;
        vec->size++;
    }

    void Vec_insert(Vec *vec, int index, void *item) {
        assert(index > 0 && index < vec->size);
        if(vec->size == vec->capacity) {
            Vec_resize(vec, vec->capacity * 2);
        }
        for (int i = vec->size - 1; i >= index; i--) {
            *(vec->items + sizeof(void *) * (i + 1)) = *(vec->items +        sizeof(void *) * i);
        }
        *(vec->items + sizeof(void *) * index) = item;
        vec->size++;
    }

    void* Vec_pop(Vec *vec) {
        assert(vec->size > 0);
        if(vec->size <= vec->capacity / 4) {
            Vec_resize(vec, vec->capacity / 2);
        }
        void *ret_val = *(vec->items + sizeof(void *) * (vec->size-1));
     
        *(vec->items + sizeof(void *) * (vec->size-1)) = NULL;
        vec->size--;    return ret_val;
    }

    void Vec_delete(Vec *vec, int index) {
        assert(index > 0 && index < vec->size);
        if(vec->size <= vec->capacity / 4) {
            // half the capacity in resize
            Vec_resize(vec, vec->capacity / 2);
        }
        *(vec->items + sizeof(void *) * index) = NULL;
        for (int i = index; i < vec->size; i++) {
            *(vec->items + sizeof(void *) * i) = *(vec->items + sizeof(void *) * (i+1));
        }
        vec->size--;
    }

    void Vec_remove(Vec *vec, void *item) {
        for(int i = 0; i < vec->size; i++) {
            if (*(vec->items + sizeof(void *) * i) == item) {
                Vec_delete(vec, i);
            }
        }
    }

    int Vec_find(Vec *vec, void *item) {
        for(int i = 0; i < vec->size; i++) {
            if (*(vec->items + sizeof(void *) * i) == item) {
                return i;
             }
        }
        return -1;
    }

    int printint(int n) {
        printf("%d", n); // Print integer followed by a newline
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

    char* concat(const char *s1, const char *s2)
    {
        char *result = (char*)GC_malloc(strlen(s1) + strlen(s2) + 1); // +1 for the null-terminator
        strcpy(result, s1);
        strcat(result, s2);
        return result;
    }

    char* stringrepeat(const char *s, const int n)
    {
        char *result = (char*)GC_malloc(strlen(s)*n+ 1); // +1 for null-terminator
        for (int i = 0; i < n; ++i)
        {
            strcpy(result, s);
        }
        return result;
    }

    int wait(int duration)
    {
        time_t start = time(NULL);
        double end = duration;
        time_t now;
        do {
            now = time(NULL);
        } while (difftime(now, start) < end);
        return 0;
    }

    int run_socket_server(int addr, int port, void* (*handler)(char*)){
        int server_fd = socket(AF_INET, SOCK_STREAM, 0);

        struct sockaddr_in server_sockaddr;
        server_sockaddr.sin_family = AF_INET; // tcp
        server_sockaddr.sin_addr.s_addr = htonl(addr==0?INADDR_ANY:INADDR_LOOPBACK);
        server_sockaddr.sin_port = htons(port);

        struct sockaddr_in *client_sockaddr = (struct sockaddr_in*) malloc(sizeof(struct sockaddr_in));
        socklen_t server_socklen = sizeof(server_sockaddr);
        socklen_t client_socklen = sizeof(client_sockaddr);

        if (bind(server_fd, (struct sockaddr*)&server_sockaddr,sizeof(server_sockaddr))< 0)
        {
            printf("Error! Bind has failed\n");
            exit(0);
        }
        if (listen(server_fd, 3) < 0)
        {
            printf("Error! Can't listen\n");
            exit(0);
        }

        const size_t buffer_len = 256;
        char *buffer = (char*)malloc(buffer_len * sizeof(char));
        struct Response *response = NULL;
        time_t last_operation;
        __pid_t pid = -1;

        while (1) {
            int client_fd = accept(server_fd, (struct sockaddr *) &client_sockaddr, &client_socklen);

            pid = fork();

            if (pid == 0) {
                close(server_fd);

                if (client_fd == -1) {
                    exit(0);
                }

                last_operation = clock();
                while (1) {
                    read(client_fd, buffer, buffer_len);

                    if (buffer == "close") {
                        printf("Process %d: ", getpid());
                        close(client_fd);
                        printf("Closing session with `%d`. Bye!\n", client_fd);
                        break;
                    }

                    if (strlen(buffer) == 0) {
                        clock_t d = clock() - last_operation;
                        double dif = 1.0 * d / CLOCKS_PER_SEC;

                        if (dif > 5.0) {
                            printf("Process %d: ", getpid());
                            close(client_fd);
                            printf("Connection timed out after %.3lf seconds. ", dif);
                            printf("Closing session with `%d`. Bye!\n", client_fd);
                            break;
                        }
                        continue;
                    }

                    printf("Process %d: ", getpid());
                    printf("Received `%s`. Processing... ", buffer);

                    // printf("strcmp in C %d\n", strcmp(buffer, "exit"));
                    response = (struct Response*) handler(buffer);
                    bzero(buffer, buffer_len * sizeof(char));

                    send(client_fd, response->resp, strlen(response->resp), 0);
                    // printf("Responded with `%s`. Close `%d` Waiting for a new query...\n", response->resp, response->close);

                    last_operation = clock();
                    if (response->close==1)
                    {
                        printf("closing");
                        break;
                    }
                }
                free(buffer);
                exit(0);
            }
            else {
                close(client_fd);
            }
        }
        return 0;
    }
