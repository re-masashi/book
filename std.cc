#include <stdio.h>
#include <gc.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>

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

    int run_server(int addr, int port, char* (*handler)(char*)){
        int server_fd, new_socket;
        ssize_t valread;
        struct sockaddr_in address;
        int opt = 1;
        socklen_t addrlen = sizeof(address);
        char buffer[1024] = { 0 };
        char* hello = (char*)"Hello from server";

        // Creating socket file descriptor
        if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
            perror("socket failed");
            exit(EXIT_FAILURE);
        }

        // Forcefully attaching socket to the port 8080
        if (setsockopt(server_fd, SOL_SOCKET,
                       SO_REUSEADDR | SO_REUSEPORT, &opt,
                       sizeof(opt))) {
            perror("setsockopt");
            exit(EXIT_FAILURE);
        }
        address.sin_family = AF_INET;
        address.sin_addr.s_addr = htonl(addr==0?INADDR_ANY:INADDR_LOOPBACK);
        address.sin_port = htons(port);
        if (bind(server_fd, (struct sockaddr*)&address,
                 sizeof(address))
            < 0) {
            perror("bind failed");
            exit(EXIT_FAILURE);
        }
        printf("listening on port %d...\n", port);
        while(true){
            if (listen(server_fd, 3) < 0) {
                perror("listen");
                exit(EXIT_FAILURE);
            }
            if ((new_socket
                 = accept(server_fd, (struct sockaddr*)&address,
                          &addrlen))
                < 0) {
                perror("accept");
                exit(EXIT_FAILURE);
            }
            valread = read(new_socket, buffer,
                           1024 - 1); // subtract 1 for the null
                                      // terminator at the end
            printf("%s\n", buffer);
            char* resp = handler(buffer);
            send(new_socket, resp, strlen(resp), 0);
            printf("Hello message sent\n");

            // closing the connected socket
            close(new_socket);
        }
        // closing the listening socket
        close(server_fd);
        return 0;
    }
}