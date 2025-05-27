#include "TCPConnection.h"
#include <iostream>
#include <errno.h>

TCPConnection::TCPConnection(const std::string& ip, int port) 
    : serverIP(ip), serverPort(port), isConnected(false) {
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        std::cerr << "Error creating socket" << std::endl;
        return;
    }

    memset(&serverAddr, 0, sizeof(serverAddr));
    serverAddr.sin_family = AF_INET;
    serverAddr.sin_port = htons(serverPort);
    
    if (inet_pton(AF_INET, serverIP.c_str(), &serverAddr.sin_addr) <= 0) {
        std::cerr << "Invalid address" << std::endl;
        close(sockfd);
        return;
    }
}

TCPConnection::~TCPConnection() {
    if (isConnected) {
        Disconnect();
    }
}

bool TCPConnection::Connect() {
    if (connect(sockfd, (struct sockaddr*)&serverAddr, sizeof(serverAddr)) < 0) {
        std::cerr << "Connection failed" << std::endl;
        return false;
    }
    isConnected = true;
    return true;
}

void TCPConnection::Disconnect() {
    if (isConnected) {
        close(sockfd);
        isConnected = false;
    }
}

bool TCPConnection::SendData(const std::string& data) {
    if (!isConnected) return false;
    
    ssize_t bytesSent = send(sockfd, data.c_str(), data.length(), 0);
    if (bytesSent < 0) {
        std::cerr << "Send failed" << std::endl;
        return false;
    }
    return true;
}

std::string TCPConnection::ReceiveData() {
    if (!isConnected) return "";
    
    char buffer[1024];
    ssize_t bytesRead = recv(sockfd, buffer, sizeof(buffer) - 1, 0);
    
    if (bytesRead < 0) {
        std::cerr << "Receive failed" << std::endl;
        return "";
    }
    
    buffer[bytesRead] = '\0';
    return std::string(buffer);
} 