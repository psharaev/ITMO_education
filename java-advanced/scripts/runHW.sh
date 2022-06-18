#!/bin/bash

# arguments:
# NOTE: pwd is project root
#HW-1         walk Walk Walk
#HW-2         arrayset SortedSet ArraySet
#HW-3         student StudentQuery StudentDB
#HW-4         implementor interface Implementor
#HW-5         implementor jar-interface Implementor
#HW-6         concurrent advanced IterativeParallelism
#HW-7         concurrent advanced IterativeParallelism mapper
#HW-8         crawler easy WebCrawler
#HW-9-client  hello client-evil HelloUDPClient
#HW-9-server  hello server-evil HelloUDPServer
#HW-11-client hello client-evil HelloUDPNonblockingClient
#HW-11-server hello server-evil HelloUDPNonblockingServer

package="${1}"
mod="${2}"
className="${3}"
testPackage="${package}"

if [[ ! (-z "${4}") ]]; then
  testPackage="${4}"
fi

courseRepository="./../java-advanced-tests/"

find "./java-solutions/info/kgeorgiy/ja/sharaev/$package/" -name "*.java" >sources.txt
javac -cp "${courseRepository}/artifacts/*" @sources.txt -d out

java -cp "./out:${courseRepository}/artifacts/*" -p "${courseRepository}lib:${courseRepository}artifacts" -m "info.kgeorgiy.java.advanced.$testPackage" "$mod" "info.kgeorgiy.ja.sharaev.$package.$className"

find . -name 'sources.txt' -delete
