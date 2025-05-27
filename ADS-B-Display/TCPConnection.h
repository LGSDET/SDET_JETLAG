#ifndef TCPConnectionH
#define TCPConnectionH

#include <string>
#include <cstring>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>

class TCPConnection {
private:
    int sockfd;
    std::string serverIP;
    int serverPort;
    bool isConnected;
    struct sockaddr_in serverAddr;

public:
    TCPConnection(const std::string& ip = "192.168.0.1", int port = 5000);
    ~TCPConnection();
    
    bool Connect();
    void Disconnect();
    bool SendData(const std::string& data);
    std::string ReceiveData();
    bool IsConnected() const { return isConnected; }
    
    void SetServerIP(const std::string& ip) { serverIP = ip; }
    void SetServerPort(int port) { serverPort = port; }
    std::string GetServerIP() const { return serverIP; }
    int GetServerPort() const { return serverPort; }
};

#endif 