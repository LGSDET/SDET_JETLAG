#include "HealthMonitor_Comm_TCPSocket.h"

// VCL 관련 헤더들은 구현 파일에서만 include
#include <vcl.h>
#include <IdTCPClient.hpp>
#include <stdexcept>

// VCL String과 std::string 간 변환 유틸리티 함수들
namespace {
    std::string VclStringToStd(const String& vclStr) {
        return std::string(AnsiString(vclStr).c_str());
    }
    
    String StdStringToVcl(const std::string& stdStr) {
        return String(stdStr.c_str());
    }
}

THealthMonitorNetwork::THealthMonitorNetwork(void* owner) {
    isConnected = false;
    tcpClient = nullptr;
    vclOwner = owner;  // VCL Owner 저장 (소켓 재생성 시 사용)
}

THealthMonitorNetwork::~THealthMonitorNetwork() {
    try {
        if (tcpClient) {
            TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
            if (client->Connected()) {
                client->Disconnect();
            }
            delete client;
            tcpClient = nullptr;
        }
    } catch (...) {
        // 소멸자에서는 예외 무시
    }
    isConnected = false;
}

bool THealthMonitorNetwork::Connect(const std::string& ipAddress, int port) {
    try {
        // DEBUG_REMOVE_LATER: 연결 시도 시작 로그
        printf("=== TCP CONNECT DEBUG ===\n");
        printf("Attempting to connect to %s:%d\n", ipAddress.c_str(), port);
        
        // 기존 소켓이 있으면 완전히 삭제
        if (tcpClient) {
            printf("Disconnecting existing client\n");  // DEBUG_REMOVE_LATER
            Disconnect();
            Sleep(200);  // 서버의 연결 정리 대기 시간 증가
        }
        
        // 새로운 TCP 클라이언트 객체 생성
        printf("Creating new TIdTCPClient\n");  // DEBUG_REMOVE_LATER
        TComponent* owner = static_cast<TComponent*>(vclOwner);
        TIdTCPClient* client = new TIdTCPClient(owner);
        
        // TCP 클라이언트 설정
        client->Host = StdStringToVcl(ipAddress);
        client->Port = port;
        client->ConnectTimeout = 5000;
        client->ReadTimeout = 5000;
        
        // 이벤트 핸들러는 nullptr (콜백 방식 사용)
        client->OnConnected = nullptr;
        client->OnDisconnected = nullptr;
        
        printf("Client configured. Host: %s, Port: %d\n", ipAddress.c_str(), port);  // DEBUG_REMOVE_LATER
        
        // 새로운 클라이언트 저장
        tcpClient = static_cast<void*>(client);
        
        // 연결 시도
        printf("Calling client->Connect()...\n");  // DEBUG_REMOVE_LATER
        client->Connect();
        printf("Connect() call completed\n");  // DEBUG_REMOVE_LATER
        
        // 연결 상태 상세 확인
        bool connected = client->Connected();
        printf("Connection status: %s\n", connected ? "CONNECTED" : "NOT CONNECTED");  // DEBUG_REMOVE_LATER
        
        if (client->Socket) {
            printf("Socket object exists\n");  // DEBUG_REMOVE_LATER
            printf("Socket connected: %s\n", client->Socket->Connected() ? "YES" : "NO");  // DEBUG_REMOVE_LATER
        } else {
            printf("Socket object is NULL\n");  // DEBUG_REMOVE_LATER
        }
        
        // 연결 성공 여부 확인 및 상태 업데이트
        if (connected) {
            printf("Connection successful - updating state\n");  // DEBUG_REMOVE_LATER
            isConnected = true;
            if (onConnectedCallback) {
                onConnectedCallback();
            }
            printf("========================\n");  // DEBUG_REMOVE_LATER
            return true;
        } else {
            printf("Connection failed - cleaning up\n");  // DEBUG_REMOVE_LATER
            // 연결 실패시 생성한 객체 삭제
            delete client;
            tcpClient = nullptr;
            printf("========================\n");  // DEBUG_REMOVE_LATER
            return false;
        }
    } catch (const std::exception& e) {
        printf("Exception occurred: %s\n", e.what());  // DEBUG_REMOVE_LATER
        // 예외 발생시 정리
        if (tcpClient) {
            TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
            delete client;
            tcpClient = nullptr;
        }
        isConnected = false;
        printf("========================\n");  // DEBUG_REMOVE_LATER
        return false;
    } catch (...) {
        printf("Unknown exception occurred\n");  // DEBUG_REMOVE_LATER
        // 예외 발생시 정리
        if (tcpClient) {
            TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
            delete client;
            tcpClient = nullptr;
        }
        isConnected = false;
        printf("========================\n");  // DEBUG_REMOVE_LATER
        return false;
    }
}

void THealthMonitorNetwork::Disconnect() {
    try {
        // DEBUG_REMOVE_LATER: 연결 해제 시작 로그
        printf("=== TCP DISCONNECT DEBUG ===\n");
        
        TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
        
        // TCP 클라이언트 객체가 있으면 완전히 삭제
        if (client) {
            printf("Client object exists, disconnecting...\n");  // DEBUG_REMOVE_LATER
            
            try {
                // 연결되어 있으면 먼저 해제
                if (client->Connected()) {
                    printf("Client is connected, calling Disconnect()\n");  // DEBUG_REMOVE_LATER
                    client->Disconnect();
                    printf("Disconnect() completed\n");  // DEBUG_REMOVE_LATER
                } else {
                    printf("Client was not connected\n");  // DEBUG_REMOVE_LATER
                }
            } catch (...) {
                printf("Exception during Disconnect()\n");  // DEBUG_REMOVE_LATER
            }
            
            // 객체 완전 삭제 (소멸자가 모든 리소스 정리)
            printf("Deleting client object\n");  // DEBUG_REMOVE_LATER
            delete client;
            tcpClient = nullptr;
            printf("Client object deleted\n");  // DEBUG_REMOVE_LATER
        } else {
            printf("No client object to disconnect\n");  // DEBUG_REMOVE_LATER
        }
        
        // 연결 해제 후 상태 초기화
        isConnected = false;
        if (onDisconnectedCallback) {
            onDisconnectedCallback();
        }
        printf("============================\n");  // DEBUG_REMOVE_LATER
    } catch (...) {
        printf("Exception during disconnect process\n");  // DEBUG_REMOVE_LATER
        // 연결 해제 중 예외는 무시하되 상태는 초기화
        tcpClient = nullptr;  // 예외 발생시에도 포인터 초기화
        isConnected = false;
        if (onDisconnectedCallback) {
            onDisconnectedCallback();
        }
        printf("============================\n");  // DEBUG_REMOVE_LATER
    }
}

bool THealthMonitorNetwork::SendCommand(const std::string& command) {
    try {
        TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
        if (!client || !client->Connected() || !client->Socket) {
            return false;
        }
        
        client->Socket->WriteLn(StdStringToVcl(command));
        return true;
    } catch (...) {
        Disconnect();
        return false;
    }
}

std::string THealthMonitorNetwork::ReceiveResponse() {
    try {
        TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
        if (!client || !client->Connected() || !client->Socket) {
            return "";
        }
        
        String vclResponse = client->Socket->ReadLn();
        return VclStringToStd(vclResponse);
    } catch (...) {
        Disconnect();
        return "";
    }
}

void THealthMonitorNetwork::OnTCPConnected() {
    isConnected = true;
    if (onConnectedCallback) {
        onConnectedCallback();
    }
}

void THealthMonitorNetwork::OnTCPDisconnected() {
    isConnected = false;
    if (onDisconnectedCallback) {
        onDisconnectedCallback();
    }
}

// VCL 이벤트 핸들러 구현 (Borland C++ 클로저용)
void __fastcall THealthMonitorNetwork::OnConnected(TObject* Sender) {
    OnTCPConnected();
}

void __fastcall THealthMonitorNetwork::OnDisconnected(TObject* Sender) {
    OnTCPDisconnected();
} 