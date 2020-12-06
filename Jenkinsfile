pipeline {
    agent any
    stages {
        stage('deps') {
            steps {
                sh './make_deps.sh'
            }
        }
        stage('compile') {
            steps {
                sh 'sbt compile'
            }
        }
        stage('test') {
            steps {
                sh 'sbt test'
                junit 'target/test-reports/*.xml'
            }
        }
        stage('coverage') {
            steps {
                sh 'sbt jacoco'
                jacoco
                    classPattern: 'target/scala*/classes',
                    execPattern: 'target/scala*/jacoco/data/*.exec',
                    sourceInclusionPattern: '**/*.scala',
                    sourcePattern: 'src/main/scala'
            }
        }
        stage('assembly') {
            steps {
                sh 'sbt assembly'
            }
        }
    }
    post { 
        always { 
            cleanWs()
        }
        success {
            archiveArtifacts
                artifacts: 'target/scala*/firrtl2digitaljs-assembly*.jar',
                followSymlinks: false
        }
    }
}