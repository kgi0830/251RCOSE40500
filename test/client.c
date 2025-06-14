#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define SERVER_IP "127.0.0.1"
#define SERVER_PORT 8080
#define MAX_RESPONSE_SIZE 4096

void send_request() {
    int sockfd;
    struct sockaddr_in server_addr;
    char request[1024] = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n";
    char response[MAX_RESPONSE_SIZE];

    // 소켓 생성
    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        perror("socket failed");
        exit(EXIT_FAILURE);
    }

    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(SERVER_PORT);
    server_addr.sin_addr.s_addr = inet_addr(SERVER_IP);

    // 서버에 연결
    if (connect(sockfd, (struct sockaddr *)&server_addr, sizeof(server_addr)) == -1) {
        perror("connect failed");
        exit(EXIT_FAILURE);
    }

    // 서버로 요청 전송
    if (send(sockfd, request, strlen(request), 0) == -1) {
        perror("send failed");
        exit(EXIT_FAILURE);
    }

    // 응답 수신
    ssize_t response_size = recv(sockfd, response, sizeof(response) - 1, 0);
    if (response_size == -1) {
        perror("recv failed");
        exit(EXIT_FAILURE);
    }

    response[response_size] = '\0';

    printf("Response:\n%s\n", response);

    close(sockfd);
}

int main() {
    send_request();

    return 0;
}

