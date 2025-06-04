#include "HealthMonitor_Communication_Network.h"

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
    
    try {
        // VCL 객체 생성
        TComponent* vclOwner = static_cast<TComponent*>(owner);
        TIdTCPClient* client = new TIdTCPClient(vclOwner);
        
        // TCP 클라이언트 설정
        client->Port = 5001;
        client->ConnectTimeout = 5000;
        client->ReadTimeout = 5000;
        
        // 이벤트 핸들러 설정 (람다로 내부 메서드 호출)
        client->OnConnected = [this](TObject* Sender) { OnTCPConnected(); };
        client->OnDisconnected = [this](TObject* Sender) { OnTCPDisconnected(); };
        
        tcpClient = static_cast<void*>(client);
    } catch (...) {
        tcpClient = nullptr;
        isConnected = false;
    }
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
        TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
        if (!client) return false;
        
        // 이전 연결이 있다면 완전히 정리
        if (client->Connected()) {
            Disconnect();
        }
        
        // 소켓 재초기화
        if (client->Socket) {
            client->Socket->Close();
            client->Socket->Open();
        }
        
        // 연결 설정
        client->Host = StdStringToVcl(ipAddress);
        client->Port = port;
        
        // 연결 시도
        client->Connect();
        return client->Connected();
    } catch (...) {
        isConnected = false;
        return false;
    }
}

void THealthMonitorNetwork::Disconnect() {
    try {
        TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
        if (client && client->Connected()) {
            client->Disconnect();
        }
        if (client && client->Socket) {
            client->Socket->Close();
        }
    } catch (...) {
        // 연결 해제 중 예외는 무시
    }
    isConnected = false;
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