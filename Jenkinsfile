pipeline {
    agent { docker { image 'mozilla/sbt' } }
    stages {
        stage('Compile') {
            steps {
                sh 'sbt compile'
            }
        }
    }
}