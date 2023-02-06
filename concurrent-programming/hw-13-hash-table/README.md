# Lock-Free Open-Addressing Hashtable

A sequential hash table is implemented in the file [`src/IntIntHashMap.kt`](src/IntIntHashMap.kt).
Using the idea discussed in the corresponding class, you need to make it thread-safe.
Please also check [this paper](https://arxiv.org/pdf/cs/0303011.pdf).

**Details:**

1. The general code design should stay the same.
2. Do not change the initial capacity (the `INITIAL_CAPACITY` field).
3. The current `IntIntHashMap` implementation always doubles the table size, even if the table is full of removed elements. You do not need to fix this.
4. In the class `IntIntHashMap.Core`, add a new `next: AtomicRef<Core>` field, which references the next version of the table.
5. `IntIntHashMap` supports only positive keys and values strictly lower `Int.MAX_VALUE`. Use negative numbers and `Int.MAX_VALUE` for the algorithm's needs.
6. You do not need to implement cooperative **rehash**.


To test your solution, please run:

* `./gradlew build` on Linux or MacOS
* `gradlew build` on Windows