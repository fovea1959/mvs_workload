#!/bin/bash

pull1() {
 mkdir $1
 cd $1
 rm *.mac
 dasdpdsu ../../../dasd2/wegscd.3350 $1 ASCII
 cd ..
}

pull1 WEGSCD.BOTTLES
pull1 WEGSCD.PI
pull1 WEGSCD.RUN
pull1 WEGSCD.BUILD
