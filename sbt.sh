#!/usr/bin/env bash

java -Xmx3g -Xms1g -XX:+TieredCompilation -XX:ReservedCodeCacheSize=256m -XX:+UseNUMA -XX:+UseParallelGC -jar sbt-launch.jar "$@"
