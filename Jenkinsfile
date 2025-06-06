pipeline {
    agent any

    environment {
        PATH = "C:\\Program Files\\mingw64\\bin;" +
               "C:\\Python\\Python311\\Scripts;" +
               "C:\\Program Files\\Cppcheck;" +
               "${env.PATH}"
        GTEST_DIR = "gtest"
        GTEST_INCLUDE = "gtest\\googletest\\include"
        GTEST_LIB     = "gtest\\build\\lib"
        REPORT_DIR    = "ADS-B-Display\\tests\\coverage"
    }

    stages {
        stage('Clean Workspace') {
            steps {
                deleteDir()
            }
        }

        stage('Checkout') {
            steps {
                checkout scm
            }
        }

        stage('Clone and Build GoogleTest') {
            steps {
                bat """
                if not exist "%GTEST_DIR%" git clone https://github.com/google/googletest.git "%GTEST_DIR%"
                cd "%GTEST_DIR%"
                if not exist "build" mkdir build
                cd build
                cmake .. -G "MinGW Makefiles" -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++
                mingw32-make
                cd ..\\..
                """
            }
        }

        stage('Build Unit Tests with Coverage Flags') {
            steps {
                bat """
                if not exist "ADS-B-Display\\tests\\bin" mkdir ADS-B-Display\\tests\\bin

                g++ -std=c++17 -g -O0 --coverage -I "%GTEST_INCLUDE%" -L "%GTEST_LIB%" ^
                  ADS-B-Display\\tests\\HealthMonitor_Alert_test.cpp ^
                  ADS-B-Display\\HealthMonitor_Alert.cpp ^
                  -lgtest -lgtest_main -o ADS-B-Display\\tests\\bin\\HealthMonitor_Alert_test.exe

                g++ -std=c++17 -g -O0 --coverage -I "%GTEST_INCLUDE%" -L "%GTEST_LIB%" ^
                  ADS-B-Display\\tests\\HealthMonitor_Comm_Parsing_test.cpp ^
                  ADS-B-Display\\HealthMonitor_Comm_Parsing.cpp ^
                  -lgtest -lgtest_main -o ADS-B-Display\\tests\\bin\\HealthMonitor_Comm_Parsing_test.exe
                """
            }
        }

        stage('Run Unit Tests') {
            steps {
                bat """
                ADS-B-Display\\tests\\bin\\HealthMonitor_Alert_test.exe
                ADS-B-Display\\tests\\bin\\HealthMonitor_Comm_Parsing_test.exe
                """
            }
        }

        stage('Static Analysis with Cppcheck') {
            steps {
                catchError(buildResult: 'SUCCESS', stageResult: 'UNSTABLE') {
                    bat """
                    if not exist cppcheck_report mkdir cppcheck_report
        
                    cppcheck --enable=all --inconclusive --std=c++17 --language=c++ --xml --xml-version=2 ^
                        ADS-B-Display\\HealthMonitor_Alert.cpp ^
                        ADS-B-Display\\HealthMonitor_Alert.h ^
                        ADS-B-Display\\HealthMonitor_Comm_Parsing.cpp ^
                        ADS-B-Display\\HealthMonitor_Comm_Parsing.h ^
                        ADS-B-Display\\HealthMonitor_Comm_TCPSocket.cpp ^
                        ADS-B-Display\\HealthMonitor_Comm_TCPSocket.h ^
                        ADS-B-Display\\HealthMonitor_MetricData.h ^
                        ADS-B-Display\\HealthMonitor_UI.cpp ^
                        ADS-B-Display\\HealthMonitor_UI.h ^
                        2> cppcheck_report\\cppcheck.xml
                    """
                }
        
                // XML 유효성 검사 후 파싱 시도
                script {
                    def xmlContent = readFile('cppcheck_report/cppcheck.xml')
                    if (!xmlContent.trim().startsWith('<?xml')) {
                        error("cppcheck.xml is not a valid XML file. Actual content:\n${xmlContent.take(200)}")
                    }
                }
        
                recordIssues tools: [cppCheck(pattern: 'cppcheck_report/cppcheck.xml')]
            }
        }

        stage('Publish Cppcheck Report') {
            steps {
                publishHTML([
                    reportDir: "cppcheck_report",
                    reportFiles: 'index.html',
                    reportName: 'Cppcheck Static Analysis',
                    keepAll: true,
                    alwaysLinkToLastBuild: true,
                    allowMissing: false
                ])
            }
        }

        // stage('Generate Coverage Report with gcovr') {
        //     steps {
        //         bat """
        //         if not exist "%REPORT_DIR%" mkdir "%REPORT_DIR%"
        //         gcovr -r . --html --html-details -o %REPORT_DIR%\\index.html
        //         """
        //     }
        // }

        // stage('Publish HTML Coverage Report') {
        //     steps {
        //         publishHTML([
        //             reportDir: "${REPORT_DIR}",
        //             reportFiles: 'index.html',
        //             reportName: 'Gcovr Coverage Report',
        //             keepAll: true,
        //             alwaysLinkToLastBuild: true,
        //             allowMissing: false
        //         ])
        //     }
        // }
        stage('Generate Coverage Report with gcovr (Cobertura)') {
            steps {
                bat """
                if not exist "%REPORT_DIR%" mkdir "%REPORT_DIR%"
                gcovr -r . --cobertura -o %REPORT_DIR%\\coverage.xml
                """
            }
        }

        stage('Publish Cobertura Coverage Report') {
            steps {
                cobertura coberturaReportFile: "${REPORT_DIR}/coverage.xml", autoUpdateHealth: false, autoUpdateStability: false
            }
        }
    }

    post {
        always {
            archiveArtifacts artifacts: "${REPORT_DIR}/**/*", allowEmptyArchive: true
        }
    }
}
