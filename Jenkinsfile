pipeline {
    agent { dockerfile true }
    stages {
        stage('Compile') {
            steps {
                sh 'sbt -Dsbt.global.base=.sbt -Dsbt.boot.directory=.sbt -Dsbt.ivy.home=.ivy2 compile'
            }
        }
    }
}