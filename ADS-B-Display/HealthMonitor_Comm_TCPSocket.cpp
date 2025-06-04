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
        // 기존 소켓이 있으면 완전히 삭제
        if (tcpClient) {
            Disconnect();
            Sleep(200);  // 서버의 연결 정리 대기 시간
        }
        
        // 새로운 TCP 클라이언트 객체 생성
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
        
        // 새로운 클라이언트 저장
        tcpClient = static_cast<void*>(client);
        
        // 연결 시도
        client->Connect();
        
        // 연결 성공 여부 확인 및 상태 업데이트
        if (client->Connected()) {
            isConnected = true;
            if (onConnectedCallback) {
                onConnectedCallback();
            }
            return true;
        } else {
            // 연결 실패시 생성한 객체 삭제
            delete client;
            tcpClient = nullptr;
            return false;
        }
    } catch (...) {
        // 예외 발생시 정리
        if (tcpClient) {
            TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
            delete client;
            tcpClient = nullptr;
        }
        isConnected = false;
        return false;
    }
}

void THealthMonitorNetwork::Disconnect() {
    try {
        TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
        
        // TCP 클라이언트 객체가 있으면 완전히 삭제
        if (client) {
            try {
                // 연결되어 있으면 먼저 해제
                if (client->Connected()) {
                    client->Disconnect();
                }
            } catch (...) {
                // Disconnect 예외 무시
            }
            
            // 객체 완전 삭제 (소멸자가 모든 리소스 정리)
            delete client;
            tcpClient = nullptr;
        }
        
        // 연결 해제 후 상태 초기화
        isConnected = false;
        if (onDisconnectedCallback) {
            onDisconnectedCallback();
        }
    } catch (...) {
        // 연결 해제 중 예외는 무시하되 상태는 초기화
        tcpClient = nullptr;  // 예외 발생시에도 포인터 초기화
        isConnected = false;
        if (onDisconnectedCallback) {
            onDisconnectedCallback();
        }
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