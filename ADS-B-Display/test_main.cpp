#include <vcl.h>
#pragma hdrstop

#include <iostream>
#include <tchar.h>
#include "HealthMonitor_Alert.h"
#include "HealthMonitor_Communication.h"

void Test_ParseCPUMetric() {
    THealthMonitorCommunication comm(nullptr);
    CPUMetricData data = comm.ParseCPUMetric("45.5/100.0");
    if (data.isValid && data.usage == 45.5)
        std::cout << "ParseCPUMetric: PASS" << std::endl;
    else
        std::cout << "ParseCPUMetric: FAIL" << std::endl;
}

void Test_IsCPUAlert() {
    THealthMonitorAlert alert;
    CPUMetricData data = {85.0, true};
    if (alert.IsCPUAlert(data))
        std::cout << "IsCPUAlert: PASS" << std::endl;
    else
        std::cout << "IsCPUAlert: FAIL" << std::endl;
}

int _tmain(int argc, _TCHAR* argv[])
{
    try {
        Test_ParseCPUMetric();
        Test_IsCPUAlert();
        // 필요한 만큼 다른 테스트 함수도 추가
    } catch (Exception &e) {
        std::cout << "Exception: " << e.Message.c_str() << std::endl;
    }
    return 0;
}
