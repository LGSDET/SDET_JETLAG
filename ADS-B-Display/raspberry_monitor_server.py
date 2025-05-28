import socket
import psutil
import time
from gpiozero import CPUTemperature
from datetime import datetime, timedelta
import subprocess

def get_power_info():
    try:
        # vcgencmd 명령어로 전압과 전류 정보 가져오기
        voltage = subprocess.check_output(['vcgencmd', 'measure_volts', 'core']).decode()
        current = subprocess.check_output(['vcgencmd', 'measure_current']).decode()
        
        # 전압 파싱 (V=1.2000V 형식에서 숫자만 추출)
        voltage_value = float(voltage.split('=')[1].replace('V', ''))
        
        # 전류 파싱 (I=1.2000A 형식에서 숫자만 추출)
        current_value = float(current.split('=')[1].replace('A', ''))
        
        return voltage_value, current_value
    except:
        return 0.0, 0.0

def get_system_info():
    # CPU 사용량 (현재/최대)
    cpu_percent = psutil.cpu_percent(interval=None)
    cpu_count = psutil.cpu_count()
    cpu_max = 100.0
    cpu_info = f"CPU:{cpu_percent:.1f}/{cpu_max:.1f}"
    
    # 메모리 사용량 (현재/최대, MB 단위)
    memory = psutil.virtual_memory()
    mem_used = memory.used / (1024 * 1024)  # Convert to MB
    mem_total = memory.total / (1024 * 1024)  # Convert to MB
    memory_info = f"MEM:{int(mem_used)}/{int(mem_total)}"
    
    # CPU 온도 (현재/최대)
    try:
        cpu = CPUTemperature()
        temp = cpu.temperature
    except:
        temp = 0  # 온도 센서를 사용할 수 없는 경우
    temp_max = 85.0
    temp_info = f"TEMP:{temp:.1f}/{temp_max:.1f}"
    
    # 디스크 사용량 (현재/최대)
    disk = psutil.disk_usage('/')
    disk_info = f"DISK:{disk.percent}/{100}"
    
    # Uptime
    boot_time = datetime.fromtimestamp(psutil.boot_time())
    uptime = datetime.now() - boot_time
    hours = int(uptime.total_seconds() // 3600)
    minutes = int((uptime.total_seconds() % 3600) // 60)
    seconds = int(uptime.total_seconds() % 60)
    uptime_info = f"UPTIME:{hours:02d}:{minutes:02d}:{seconds:02d}"
    
    # 전원 상태
    voltage, current = get_power_info()
    power_info = f"POWER:{voltage:.1f}V/{current:.1f}A"
    
    # 모든 정보를 파이프(|)로 구분하여 반환
    return f"{cpu_info}|{memory_info}|{temp_info}|{disk_info}|{uptime_info}|{power_info}"

def main():
    # 서버 설정
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.bind(('0.0.0.0', 5001))  # 모든 IP에서의 연결 허용
    server_socket.listen(1)
    
    print("모니터링 서버가 시작되었습니다. (포트: 5001)")
    
    while True:
        client_socket = None
        try:
            client_socket, addr = server_socket.accept()
            print(f"클라이언트가 연결되었습니다: {addr}")
            
            while True:
                start_time = time.time()  # 현재 시간 기록
                
                # 시스템 정보 수집 및 전송
                info = get_system_info()
                client_socket.send((info + "\n").encode())
                
                # 정확한 1초 간격 유지
                elapsed_time = time.time() - start_time
                sleep_time = max(0, 1.0 - elapsed_time)  # 음수가 되지 않도록 보장
                if sleep_time > 0:
                    time.sleep(sleep_time)
                
        except ConnectionResetError:
            print("클라이언트 연결이 종료되었습니다.")
        except socket.error as e:
            print(f"소켓 에러 발생: {e}")
        except Exception as e:
            print(f"에러 발생: {e}")
        finally:
            if client_socket:
                try:
                    client_socket.close()
                except:
                    pass
            time.sleep(1)  # 재연결 시도 전 잠시 대기

if __name__ == "__main__":
    main() 