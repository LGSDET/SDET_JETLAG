#ifndef HealthMonitor_Communication_NetworkH
#define HealthMonitor_Communication_NetworkH

#include <string>
#include <functional>

// 순전히 VCL TCP 통신만 담당하는 클래스
class THealthMonitorNetwork {
private:
    void* tcpClient;  // TIdTCPClient* (VCL 타입을 헤더에서 숨김)
    bool isConnected;
    
    // 콜백 함수들
    std::function<void()> onConnectedCallback;
    std::function<void()> onDisconnectedCallback;
    
    // VCL 이벤트 핸들러들 (내부적으로만 사용)
    void OnTCPConnected();
    void OnTCPDisconnected();
    
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