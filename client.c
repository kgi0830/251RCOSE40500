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

    // ���� ����
    if ((client_socket = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        perror("socket failed");
        return 1;
    }

    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(8080);

    // ���� IP �ּ� ����
    if (inet_pton(AF_INET, "127.0.0.1", &server_address.sin_addr) <= 0) {
        perror("inet_pton failed");
        return 1;
    }

    // ������ ����
    if (connect(client_socket, (struct sockaddr*)&server_address, sizeof(server_address)) < 0) {
        perror("connect failed");
        return 1;
    }

    // �߸��� ��û ����
    const char* request = "GET /nonexistent_file HTTP/1.1\r\nHost: localhost\r\n\r\n";
    if (send(client_socket, request, strlen(request), 0) < 0) {
        perror("send failed");
        return 1;
    }

    // ���� ���� ����
    char response[4096];
    ssize_t response_size = recv(client_socket, response, sizeof(response), 0);
    if (response_size < 0) {
        perror("recv failed");
        return 1;
    }

    // ���� ���
    printf("Response:\n%s\n", response);

    // ���� ����
    close(client_socket);

    return 0;
}

