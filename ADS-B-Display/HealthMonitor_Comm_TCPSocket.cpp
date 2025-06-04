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
    
    try {
        // VCL 객체 생성
        TComponent* vclOwner = static_cast<TComponent*>(owner);
        TIdTCPClient* client = new TIdTCPClient(vclOwner);
        
        // TCP 클라이언트 설정
        client->Port = 5001;
        client->ConnectTimeout = 5000;
        client->ReadTimeout = 5000;
        
        // 이벤트 핸들러는 nullptr (Borland C++ 호환성 문제로 인한 대안)
        client->OnConnected = nullptr;
        client->OnDisconnected = nullptr;
        
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
            // 소켓 정리를 위한 짧은 대기
            Sleep(100);
        }
        
        // 소켓 강제 정리 및 재초기화
        try {
            if (client->Socket) {
                client->Socket->Close();
                // 소켓 바인딩 해제를 위한 대기
                Sleep(50);
            }
        } catch (...) {
            // 소켓 정리 중 예외 무시
        }
        
        // 새로운 소켓으로 완전 재초기화
        try {
            if (client->Socket) {
                client->Socket->Open();
            }
        } catch (...) {
            // 소켓 열기 실패시 무시하고 계속 진행
        }
        
        // 연결 설정
        client->Host = StdStringToVcl(ipAddress);
        client->Port = port;
        client->ConnectTimeout = 5000;
        client->ReadTimeout = 5000;
        
        // 연결 시도 
        client->Connect();
        
        // 연결 성공 여부 확인 및 상태 업데이트 (이벤트 핸들러 대신)
        if (client->Connected()) {
            isConnected = true;
            if (onConnectedCallback) {
                onConnectedCallback();
            }
            return true;
        }
        return false;
    } catch (...) {
        isConnected = false;
        return false;
    }
}

void THealthMonitorNetwork::Disconnect() {
    try {
        TIdTCPClient* client = static_cast<TIdTCPClient*>(tcpClient);
        
        // 연결 상태 확인 후 강제 해제
        if (client) {
            try {
                if (client->Connected()) {
                    client->Disconnect();
                    // 연결 해제 완료를 위한 대기
                    Sleep(50);
                }
            } catch (...) {
                // Disconnect 예외 무시
            }
            
            // 소켓 강제 정리
            try {
                if (client->Socket) {
                    client->Socket->Close();
                    // 소켓 완전 정리를 위한 대기
                    Sleep(50);
                }
            } catch (...) {
                // 소켓 정리 예외 무시
            }
        }
        
        // 연결 해제 후 상태 초기화
        isConnected = false;
        if (onDisconnectedCallback) {
            onDisconnectedCallback();
        }
    } catch (...) {
        // 연결 해제 중 예외는 무시하되 상태는 초기화
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