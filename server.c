#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/select.h>
#include <errno.h>

#define MAX_REQUEST_SIZE 4096
#define MAX_CLIENTS 10

void send_response(int client_socket, const char* response);
void send_file_response(int client_socket, const char* file_path);
void handle_request(int client_socket, const char* folder_path);

int main(int argc, char* argv[]) {
    if (argc != 3) {
        //printf("Usage: %s <port> <folder_path>\n", argv[0]);
        return 1;
    }

    int port = atoi(argv[1]);
    const char* folder_path = argv[2];

    int server_fd, client_socket;
    struct sockaddr_in address;
    int opt = 1;
    int addrlen = sizeof(address);

    // ���� ����
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        //perror("socket failed");
        return 1;
    }

    // ���� �ɼ� ����
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt))) {
        //perror("setsockopt failed");
        return 1;
    }

    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);

    // ���Ͽ� �ּ� �Ҵ�
    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) < 0) {
        //perror("bind failed");
        return 1;
    }

    // ���� ��û ���
    if (listen(server_fd, 3) < 0) {
        //perror("listen failed");
        return 1;
    }

    //printf("Server listening on port %d\n", port);

    fd_set read_fds;
    int max_fd, activity;
    int client_sockets[MAX_CLIENTS];
    memset(client_sockets, 0, sizeof(client_sockets));

    while (1) {
        // ���� ���� �ʱ�ȭ
        FD_ZERO(&read_fds);

        // ���� ������ ���� ���տ� �߰�
        FD_SET(server_fd, &read_fds);
        max_fd = server_fd;

        // Ŭ���̾�Ʈ ������ ���� ���տ� �߰�
        for (int i = 0; i < MAX_CLIENTS; ++i) {
            client_socket = client_sockets[i];

            if (client_socket > 0) {
                FD_SET(client_socket, &read_fds);
            }

            if (client_socket > max_fd) {
                max_fd = client_socket;
            }
        }

        // I/O multiplexing
        activity = select(max_fd + 1, &read_fds, NULL, NULL, NULL);

        if ((activity < 0) && (errno != EINTR)) {
            //printf("select error");
        }

        // Ŭ���̾�Ʈ ���� ó��
        if (FD_ISSET(server_fd, &read_fds)) {
            if ((client_socket = accept(server_fd, (struct sockaddr*)&address, (socklen_t*)&addrlen)) < 0) {
                //perror("accept failed");
                return 1;
            }

            // Ŭ���̾�Ʈ ������ �迭�� �߰�
            for (int i = 0; i < MAX_CLIENTS; ++i) {
                if (client_sockets[i] == 0) {
                    client_sockets[i] = client_socket;
                    break;
                }
            }
        }

        // Ŭ���̾�Ʈ ��û ó��
        for (int i = 0; i < MAX_CLIENTS; ++i) {
            client_socket = client_sockets[i];

            if (FD_ISSET(client_socket, &read_fds)) {
                handle_request(client_socket, folder_path);
                client_sockets[i] = 0;
            }
        }
    }

    return 0;
}

void send_response(int client_socket, const char* response) {
    write(client_socket, response, strlen(response));
}

void send_file_response(int client_socket, const char* file_path) {
    FILE* file = fopen(file_path, "rb");
    if (file == NULL) {
        const char* not_found_response = "HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\n\r\nNot found";
        send_response(client_socket, not_found_response);
        return;
    }

    // ���� ũ�� Ȯ��
    fseek(file, 0L, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0L, SEEK_SET);

    // ���� ��� ����
    char response_header[MAX_REQUEST_SIZE];
    snprintf(response_header, sizeof(response_header),
             "HTTP/1.1 200 OK\r\nContent-Length: %ld\r\n\r\n", file_size);

    // ��� ����
    send_response(client_socket, response_header);

    // ���� ���� ����
    char buffer[MAX_REQUEST_SIZE];
    size_t bytes_read;
    while ((bytes_read = fread(buffer, 1, sizeof(buffer), file)) > 0) {
        write(client_socket, buffer, bytes_read);
    }

    fclose(file);
}

void handle_request(int client_socket, const char* folder_path) {
    char request[MAX_REQUEST_SIZE];
    ssize_t request_size = read(client_socket, request, sizeof(request));

    // ��û �Ľ�
    const char* method = strtok(request, " \t\r\n");
    const char* path = strtok(NULL, " \t\r\n");

    if (method == NULL || path == NULL) {
        const char* bad_request_response = "HTTP/1.1 400 Bad Request\r\nContent-Length: 12\r\n\r\nBad Request";
        send_response(client_socket, bad_request_response);
        close(client_socket);
        return;
    }

    if (strcmp(method, "GET") == 0) {
        // ���� ��û ó��
        char file_path[MAX_REQUEST_SIZE];
        snprintf(file_path, sizeof(file_path), "%s/%s", folder_path, path);

        // ��ο� ���� ���丮 ���� �õ� ����
        if (strstr(file_path, "../") != NULL) {
            const char* forbidden_response = "HTTP/1.1 403 Forbidden\r\nContent-Length: 9\r\n\r\nForbidden";
            send_response(client_socket, forbidden_response);
            close(client_socket);
            return;
        }

        send_file_response(client_socket, file_path);
    } else {
        // �������� �ʴ� �޼��� ó��
        const char* not_supported_response = "HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 15\r\n\r\nMethod not allowed";
        send_response(client_socket, not_supported_response);
    }

    close(client_socket);
}
