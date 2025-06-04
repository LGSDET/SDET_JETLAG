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
    disk_info = f"DISK:{int(disk.percent)}/{100}"
    
    # Uptime
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
    
    # 전원 상태
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