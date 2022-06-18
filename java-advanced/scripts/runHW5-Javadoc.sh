#!/bin/bash

OUTDIR=javadoc

rm -rf ${OUTDIR}

javadoc -private -version -author -sourcepath "java-solutions" -classpath "./../java-advanced-2022/artifacts/info.kgeorgiy.java.advanced.implementor.jar" -d ${OUTDIR} info.kgeorgiy.ja.sharaev.implementor
