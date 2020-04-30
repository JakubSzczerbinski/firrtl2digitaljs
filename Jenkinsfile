pipeline {
    agent { docker { image 'hseeberger/scala-sbt:8u222_1.3.7_2.12.10' } }
    stages {
        stage('Compile') {
            steps {
                sh 'sbt compile'
            }
        }
    }
}