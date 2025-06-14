#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define PORT 8080
#define MAX_REQUEST_SIZE 4096

void handle_request(int client_socket) {
    char request[MAX_REQUEST_SIZE];
    ssize_t request_size = read(client_socket, request, sizeof(request));
    
    // 간단한 응답 생성
    const char* response = "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nHello, World!";
    
    write(client_socket, response, strlen(response));
    
    close(client_socket);
}

int main() {
    int server_fd, client_socket;
    struct sockaddr_in address;
    int opt = 1;
    int addrlen = sizeof(address);

    // 소켓 생성
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        perror("socket failed");
        exit(EXIT_FAILURE);
    }

    // 소켓 옵션 설정
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt))) {
        perror("setsockopt failed");
        exit(EXIT_FAILURE);
    }
    
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(PORT);

    // 소켓에 주소 할당
    if (bind(server_fd, (struct sockaddr *)&address, sizeof(address))<0) {
        perror("bind failed");
        exit(EXIT_FAILURE);
    }

    // 연결 요청 대기
    if (listen(server_fd, 3) < 0) {
        perror("listen failed");
        exit(EXIT_FAILURE);
    }

    printf("Server listening on port %d\n", PORT);

    while (1) {
        // 클라이언트 연결 수락
        if ((client_socket = accept(server_fd, (struct sockaddr *)&address, (socklen_t*)&addrlen))<0) {
            perror("accept failed");
            exit(EXIT_FAILURE);
        }

        // 요청 처리
        handle_request(client_socket);
    }

    return 0;
}

