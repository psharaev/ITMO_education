#!/bin/bash

find "./java-solutions/info/kgeorgiy/ja/sharaev/implementor/" -name "*.java" >sources.txt
javac -cp "./../java-advanced-2022/artifacts/*" @sources.txt -d out/

mkdir "artifacts"
jar -cfm ./artifacts/JarImplementor.jar ./java-solutions/info/kgeorgiy/ja/sharaev/implementor/MANIFEST.MF -C out info/kgeorgiy/ja/sharaev/implementor/

find . -name 'sources.txt' -delete