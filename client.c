#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main() {
    int client_socket;
    struct sockaddr_in server_address;

    // 소켓 생성
    if ((client_socket = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        perror("socket failed");
        return 1;
    }

    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(8080);

    // 서버 IP 주소 설정
    if (inet_pton(AF_INET, "127.0.0.1", &server_address.sin_addr) <= 0) {
        perror("inet_pton failed");
        return 1;
    }

    // 서버에 연결
    if (connect(client_socket, (struct sockaddr*)&server_address, sizeof(server_address)) < 0) {
        perror("connect failed");
        return 1;
    }

    // 잘못된 요청 전송
    const char* request = "GET /nonexistent_file HTTP/1.1\r\nHost: localhost\r\n\r\n";
    if (send(client_socket, request, strlen(request), 0) < 0) {
        perror("send failed");
        return 1;
    }

    // 서버 응답 수신
    char response[4096];
    ssize_t response_size = recv(client_socket, response, sizeof(response), 0);
    if (response_size < 0) {
        perror("recv failed");
        return 1;
    }

    // 응답 출력
    printf("Response:\n%s\n", response);

    // 소켓 종료
    close(client_socket);

    return 0;
}

