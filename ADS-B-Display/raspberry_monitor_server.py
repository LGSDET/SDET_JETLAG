import socket
import psutil
import time
from gpiozero import CPUTemperature

def get_system_info():
    # CPU 사용량
    cpu_percent = psutil.cpu_percent()
    
    # 메모리 사용량
    memory = psutil.virtual_memory()
    memory_percent = memory.percent
    
    # CPU 온도
    try:
        cpu = CPUTemperature()
        temp = cpu.temperature
    except:
        temp = 0  # 온도 센서를 사용할 수 없는 경우
        
    # 디스크 사용량
    disk = psutil.disk_usage('/')
    disk_percent = disk.percent
    
    # 데이터 포맷: "CPU:50|MEM:60|TEMP:45|DISK:75"
    return f"CPU:{cpu_percent}|MEM:{memory_percent}|TEMP:{temp}|DISK:{disk_percent}"

def main():
    # 서버 설정
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.bind(('0.0.0.0', 5001))  # 모든 IP에서의 연결 허용
    server_socket.listen(1)
    
    print("모니터링 서버가 시작되었습니다. (포트: 5001)")
    
    while True:
        try:
            client_socket, addr = server_socket.accept()
            print(f"클라이언트가 연결되었습니다: {addr}")
            
            while True:
                # 시스템 정보 수집 및 전송
                info = get_system_info()
                client_socket.send((info + "\n").encode())
                time.sleep(1)  # 1초 대기
                
        except ConnectionResetError:
            print("클라이언트 연결이 종료되었습니다.")
        except Exception as e:
            print(f"에러 발생: {e}")
        finally:
            try:
                client_socket.close()
            except:
                pass

if __name__ == "__main__":
    main() 