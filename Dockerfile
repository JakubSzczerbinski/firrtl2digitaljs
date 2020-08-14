FROM openjdk:8

RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
    apt-get update && \
    apt-get install -y sbt tar

RUN git clone https://github.com/freechipsproject/firrtl.git -b 1.3-release && cd firrtl && \
    sbt -Dsbt.global.base=/.sbt -Dsbt.boot.directory=/.sbt -Dsbt.ivy.home=/.ivy2 compile && \
    sbt -Dsbt.global.base=/.sbt -Dsbt.boot.directory=/.sbt -Dsbt.ivy.home=/.ivy2 assembly && \
    sbt -Dsbt.global.base=/.sbt -Dsbt.boot.directory=/.sbt -Dsbt.ivy.home=/.ivy2 publishLocal

RUN mkdir firrtl2digitaljs

ADD build.sbt /firrtl2digitaljs/
ADD src /firrtl2digitaljs/src/
ADD project /firrtl2digitaljs/project/

RUN cd firrtl2digitaljs && \
    sbt -Dsbt.global.base=/.sbt -Dsbt.boot.directory=/.sbt -Dsbt.ivy.home=/.ivy2 compile && \
    sbt -Dsbt.global.base=/.sbt -Dsbt.boot.directory=/.sbt -Dsbt.ivy.home=/.ivy2 assembly && \
    cp target/scala-*/firrtl2digitaljs-assembly-*.jar /etc/firrtl2digitaljs.jar \
