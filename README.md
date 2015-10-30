====================
The Oak HProf Parser
====================

*a silly attempt to parse heap dumps in search of SegmentId dependency graphs*

This comes as support for Oak issue [OAK-3560](https://issues.apache.org/jira/browse/OAK-3560), so read there for the output format.

Build with Scala ```2.11.x``` and [Oak](https://jackrabbit.apache.org/oak/) ```1.3.x```

[![Oak 1.3.10](https://img.shields.io/badge/Oak-1.3.10-green.svg)](https://jackrabbit.apache.org/oak/)
[![Build Status](https://travis-ci.org/stillalex/oak-hprof-parser.svg?branch=master)](https://travis-ci.org/stillalex/oak-hprof-parser)
[![License](http://img.shields.io/:license-Apache%202-red.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)

How to build
------------
Running `mvn package` from the project's root will produce a distribution archive containing everything you need

```bash
$ mvn package
$ ll target/oak-hprof-*
$ .... target/oak-hprof-0.0.1-SNAPSHOT-dist.zip
$ .... target/oak-hprof-0.0.1-SNAPSHOT.jar
```

Or look under the [releases tab](../../releases) on github.

How to run
----------
```bash
$ java -jar oak-hprof-*.jar heap.bin
```

Sample output (```segments-heap.gdf):
```
nodedef>name VARCHAR, type VARCHAR, gc INT, t INT
1ddcd2eb-2f40-4131-ab25-4bc76a87c0fc,data,1,59217755
8297d721-f862-4d2c-afb0-03502713140c,data,1,59206647
52d809fa-45d6-4f0a-a6b9-6b3a2493675c,data,1,59205553
edgedef>node1 VARCHAR, node2 VARCHAR
1ddcd2eb-2f40-4131-ab25-4bc76a87c0fc,ed3bc371-33a1-4a1e-a017-3f32e8c96a93
1ddcd2eb-2f40-4131-ab25-4bc76a87c0fc,4cea14ea-67a2-4bfa-a4a2-9cdfd8e0305d
1ddcd2eb-2f40-4131-ab25-4bc76a87c0fc,3e83ac44-595e-4655-a2da-6397218448f7
```

Credits
-------

  https://java.net/downloads/heap-snapshot/hprof-binary-format.html
  http://hg.openjdk.java.net/jdk7/jdk7/jdk/file/e7d93d1d2bf0/src/share/classes/com/sun/tools/hat/internal/parser/HprofReader.java
  https://github.com/eaftan/hprof-parser

License
-------

```
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this work except in compliance with the License.
You may obtain a copy of the License in the LICENSE file, or at:

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
