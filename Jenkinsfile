pipeline {
    agent { dockerfile true }
    stages {
        stage('Compile') {
            steps {
                sh 'sbt compile'
            }
        }
    }
}