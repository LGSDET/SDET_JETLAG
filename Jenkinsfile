pipeline {
    agent any

    environment {
        PATH = "C:\\Program Files\\mingw64\\bin;" +
               "C:\\Python\\Python311\\Scripts;" +
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

        stage('Generate Coverage Report with gcovr') {
            steps {
                bat """
                if not exist "%REPORT_DIR%" mkdir "%REPORT_DIR%"
                gcovr -r . --html --html-details -o %REPORT_DIR%\\index.html
                """
            }
        }

        stage('Publish Coverage Report') {
            steps {
                publishHTML([
                    reportDir: "${REPORT_DIR}",
                    reportFiles: 'index.html',
                    reportName: 'Gcovr Coverage Report'
                ])
            }
        }
    }

    post {
        always {
            archiveArtifacts artifacts: "${REPORT_DIR}/**/*", allowEmptyArchive: true
        }
    }
}

