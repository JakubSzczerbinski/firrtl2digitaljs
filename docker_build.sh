#!/bin/sh

CONTAINER=$(docker run -itd szczerbi/f2d_build bash)
F2D_PATH=$(docker exec ${CONTAINER} sh -c "ls /firrtl2digitaljs/target/scala-*/firrtl2digitaljs-assembly-*.jar")
docker cp ${CONTAINER}:${F2D_PATH} firrtl2digitaljs.jar
docker stop ${CONTAINER}
docker rm ${CONTAINER}
