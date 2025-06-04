import socket
import psutil
import time
from gpiozero import CPUTemperature
from datetime import datetime, timedelta
import subprocess
import zlib  # CRC32를 위해 추가

class Timer:
    def __init__(self):
        self.reset()
    
    def reset(self):
        """타이머를 0으로 초기화"""
        self.start_time = time.time_ns() // 1_000_000  # 나노초를 밀리초로 변환
    
    def get_elapsed(self):
        """타이머 시작부터 경과된 시간을 밀리초 단위로 반환"""
        current = time.time_ns() // 1_000_000
        return current - self.start_time

def get_power_info():
    try:
        # 전압 정보 가져오기 (core 전압)
        voltage_output = subprocess.check_output(['vcgencmd', 'measure_volts', 'core'], stderr=subprocess.STDOUT).decode().strip()
        voltage_match = voltage_output.split('=')[1].replace('V', '')
        voltage_value = float(voltage_match)

        # 전압이 유효한지 확인
        if voltage_value < 0.5 or voltage_value > 5.5:  # 라즈베리파이 정상 전압 범위
            voltage_value = 5.0  # 기본값 설정

        # 전류는 측정이 어려우므로 일반적인 동작 전류 추정값 사용
        current_value = 0.6  # 라즈베리파이 일반적인 동작 전류

        return voltage_value, current_value

    except Exception as e:
        print(f"Power info error: {str(e)}")
        return 5.0, 0.6  # 오류 발생 시 기본값 반환

def get_system_info():
    # DEBUG_REMOVE_LATER: 알람 테스트를 위한 15초 주기 임계값 초과 데이터 생성
    current_time = time.time()
    # 30초 주기로 15초는 임계값 초과, 15초는 정상값
    is_alarm_mode = int(current_time // 15) % 2 == 0
    
    if is_alarm_mode:
        # DEBUG_REMOVE_LATER: 임계값 초과 값들 (알람 발생)
        print("DEBUG: 알람 모드 - 임계값 초과 데이터 전송")
        cpu_percent = 85.5  # CPU 임계값 80% 초과
        
        # 메모리 사용량 - 실제 총 메모리를 기준으로 80% 초과 설정
        memory = psutil.virtual_memory()
        mem_total = memory.total / (1024 * 1024)  # MB 단위
        mem_used = int(mem_total * 0.85)  # 85% 사용 (80% 임계값 초과)
        
        temp = 75.2  # 온도 임계값 70°C 초과
        disk_percent = 95  # 디스크 임계값 90% 초과
        
        cpu_info = f"CPU:{cpu_percent:.1f}/100.0"
        memory_info = f"MEM:{mem_used}/{int(mem_total)}"
        temp_info = f"TEMP:{temp:.1f}/85.0"
        disk_info = f"DISK:{disk_percent}/100"
        
    else:
        # DEBUG_REMOVE_LATER: 정상 값들
        print("DEBUG: 정상 모드 - 임계값 이하 데이터 전송")
        cpu_percent = psutil.cpu_percent(interval=None)
        if cpu_percent > 50:  # 실제값이 높으면 강제로 낮춤
            cpu_percent = 25.5
        
        # 메모리 사용량 (실제값 또는 안전한 값)
        memory = psutil.virtual_memory()
        mem_used = memory.used / (1024 * 1024)
        mem_total = memory.total / (1024 * 1024)
        mem_percent = (mem_used / mem_total) * 100
        if mem_percent > 60:  # 실제값이 높으면 강제로 낮춤
            mem_used = int(mem_total * 0.45)  # 45% 사용
        
        # CPU 온도 (실제값 또는 안전한 값)
        try:
            cpu = CPUTemperature()
            temp = cpu.temperature
            if temp > 50:  # 실제값이 높으면 강제로 낮춤
                temp = 42.5
        except:
            temp = 42.5
        
        # 디스크 사용량 (실제값 또는 안전한 값)
        disk = psutil.disk_usage('/')
        disk_percent = int(disk.percent)
        if disk_percent > 70:  # 실제값이 높으면 강제로 낮춤
            disk_percent = 55
        
        cpu_info = f"CPU:{cpu_percent:.1f}/100.0"
        memory_info = f"MEM:{int(mem_used)}/{int(mem_total)}"
        temp_info = f"TEMP:{temp:.1f}/85.0"
        disk_info = f"DISK:{disk_percent}/100"
    
    # Uptime (공통)
    boot_time = datetime.fromtimestamp(psutil.boot_time())
    uptime = datetime.now() - boot_time
    days = uptime.days
    hours = int((uptime.seconds % 86400) // 3600)
    minutes = int((uptime.seconds % 3600) // 60)
    seconds = int(uptime.seconds % 60)
    
    if days > 0:
        uptime_info = f"UPTIME:{days}d {hours:02d}:{minutes:02d}:{seconds:02d}"
    else:
        uptime_info = f"UPTIME:{hours:02d}:{minutes:02d}:{seconds:02d}"
    
    # 전원 상태 (공통)
    voltage, current = get_power_info()
    power_info = f"POWER:{voltage:.1f}V/{current:.1f}A"
    
    # 모든 정보를 파이프(|)로 구분하여 반환
    return f"{cpu_info}|{memory_info}|{temp_info}|{disk_info}|{uptime_info}|{power_info}"

def calculate_crc32(data):
    """데이터의 CRC32 체크섬을 계산하여 8자리 16진수 문자열로 반환"""
    crc = zlib.crc32(data.encode()) & 0xFFFFFFFF
    return f"{crc:08x}"

def main():
    # 서버 설정
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.bind(('0.0.0.0', 5001))  # 모든 IP에서의 연결 허용
    server_socket.listen(1)
    
    print("모니터링 서버가 시작되었습니다. (포트: 5001)")
    print("DEBUG_REMOVE_LATER: 알람 테스트 모드 - 15초마다 임계값 초과/정상값 번갈아 전송")
    
    while True:
        client_socket = None
        timer = Timer()
        try:
            client_socket, addr = server_socket.accept()
            print(f"클라이언트가 연결되었습니다: {addr}")
            
            while True:
                try:
                    # 타이머 초기화
                    timer.reset()
                    
                    # 시스템 정보 수집
                    info = get_system_info()
                    
                    # 타이머 값을 포함하여 데이터 구성
                    elapsed_time = timer.get_elapsed()
                    data = f"TIMER={elapsed_time}|{info}"
                    crc32 = calculate_crc32(data)
                    message = f"{data}|CRC={crc32}\n"
                    
                    # 데이터 전송
                    client_socket.send(message.encode())
                    
                    # 정확한 1초 간격 유지
                    time.sleep(1.0)
                
                except BrokenPipeError:
                    print("클라이언트가 연결을 종료했습니다. (Broken Pipe)")
                    break
                except ConnectionResetError:
                    print("클라이언트가 연결을 강제 종료했습니다.")
                    break
                except socket.error as e:
                    if e.errno == 32:  # Broken pipe
                        print("클라이언트와의 연결이 끊어졌습니다. (Error 32)")
                    else:
                        print(f"소켓 에러 발생: {e}")
                    break
                
        except Exception as e:
            print(f"에러 발생: {e}")
        finally:
            if client_socket:
                try:
                    client_socket.shutdown(socket.SHUT_RDWR)
                    client_socket.close()
                    print("클라이언트 소켓이 정상적으로 종료되었습니다.")
                except:
                    pass
            time.sleep(0.5)

if __name__ == "__main__":
    main() 