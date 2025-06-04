#ifndef HealthMonitor_Comm_TCPSocketH
#define HealthMonitor_Comm_TCPSocketH

#include <string>
#include <functional>
#include <vcl.h>
#include <Classes.hpp>  // TObject 정의를 위해 추가

// 순전히 VCL TCP 통신만 담당하는 클래스
class THealthMonitorNetwork {
private:
    void* tcpClient;  // TIdTCPClient* (VCL 타입을 헤더에서 숨김)
    void* vclOwner;   // TComponent* (소켓 재생성 시 사용)
    bool isConnected;
    
    // 콜백 함수들
    std::function<void()> onConnectedCallback;
    std::function<void()> onDisconnectedCallback;
    
    // VCL 이벤트 핸들러들 (내부적으로만 사용)
    void OnTCPConnected();
    void OnTCPDisconnected();
    
    // VCL 이벤트 핸들러 메서드들 (Borland C++ 클로저용) 
    void __fastcall OnConnected(TObject* Sender);
    void __fastcall OnDisconnected(TObject* Sender);
    
public:
    THealthMonitorNetwork(void* owner);  // VCL TComponent* owner
    ~THealthMonitorNetwork();
    
    // 네트워크 연결 관리
    bool Connect(const std::string& ipAddress, int port = 5001);
    void Disconnect();
    bool IsConnected() const { return isConnected; }
    
    // 데이터 송수신
    bool SendCommand(const std::string& command);
    std::string ReceiveResponse();
    
    // 콜백 설정
    void SetOnConnected(std::function<void()> callback) { onConnectedCallback = callback; }
    void SetOnDisconnected(std::function<void()> callback) { onDisconnectedCallback = callback; }
};

#endif 