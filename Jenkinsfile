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
    }
}