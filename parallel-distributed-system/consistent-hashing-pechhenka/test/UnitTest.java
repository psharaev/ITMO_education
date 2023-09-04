import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Map;
import java.util.Random;
import java.util.Set;

public class UnitTest {
    @Test
    public void testAddSingleShard() {
        var random = new Random(System.currentTimeMillis());
        var cHash = new ConsistentHashImpl<Integer>();
        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(100));
        assertEquals(Map.of(), addRes);
        for (int i = 0; i < 100; i++) {
            var key = random.nextInt();
            assertEquals(shard1, cHash.getShardByKey(key));
        }
    }

    @Test
    public void testAddMultipleVnodesSingleShard() {
        var random = new Random(System.currentTimeMillis());
        var cHash = new ConsistentHashImpl<Integer>();
        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(
                shard1,
                Set.of(-100_000, 100_000, 300_000, 500_000, 900_000)
        );
        assertEquals(Map.of(), addRes);
        for (int i = 0; i < 100; i++) {
            var key = random.nextInt();
            assertEquals(shard1, cHash.getShardByKey(key));
        }
    }

    @Test
    public void testAddMultipleShardsSingleVnode() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(100));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(200));
        assertEquals(
                Map.of(
                        shard1, Set.of(new HashRange(101, 200))
                ),
                addRes
        );

        var shard3 = new Shard("shard_3");
        addRes = cHash.addShard(shard3, Set.of(150));
        assertEquals(
                Map.of(
                        shard2, Set.of(new HashRange(101, 150))
                ),
                addRes
        );

        var shard4 = new Shard("shard_4");
        addRes = cHash.addShard(shard4, Set.of(50));
        assertEquals(
                Map.of(
                        shard1, Set.of(new HashRange(201, 50))
                ),
                addRes
        );

        var shard5 = new Shard("shard_5");
        addRes = cHash.addShard(shard5, Set.of(500));
        assertEquals(
                Map.of(
                        shard4, Set.of(new HashRange(201, 500))
                ),
                addRes
        );

        assertEquals(shard4, cHash.getShardByKey(-100));
        assertEquals(shard4, cHash.getShardByKey(10));
        assertEquals(shard1, cHash.getShardByKey(75));
        assertEquals(shard1, cHash.getShardByKey(100));
        assertEquals(shard5, cHash.getShardByKey(300));
        assertEquals(shard4, cHash.getShardByKey(1_000_000));
    }

    @Test
    public void testOneAfterAnother() {
        var cHash = new ConsistentHashImpl<Integer>();
        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(100, 1000, 2000));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(200, 500, 400));
        assertEquals(
                Map.of(shard1, Set.of(new HashRange(101, 500))),
                addRes
        );

        assertEquals(shard1, cHash.getShardByKey(100));
        assertEquals(shard1, cHash.getShardByKey(-100));
        assertEquals(shard2, cHash.getShardByKey(150));
        assertEquals(shard2, cHash.getShardByKey(500));
        assertEquals(shard1, cHash.getShardByKey(501));
        assertEquals(shard1, cHash.getShardByKey(3000));
    }

    @Test
    public void testOneAfterAnotherCircleEnd() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(100, 1000, 2000));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(3000, -100, -500, 2500));
        assertEquals(
                Map.of(
                        shard1, Set.of(new HashRange(2001, -100))
                ),
                addRes
        );
    }

    @Test
    public void testMultipleRangesReplaceSameShard() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(300, 1200, 2200));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(1700, 4200, 3200));
        assertEquals(
                Map.of(
                        shard1,
                        Set.of(
                                new HashRange(1201, 1700),
                                new HashRange(2201, 4200)
                        )
                ),
                addRes
        );

        assertEquals(shard1, cHash.getShardByKey(4201));
    }

    @Test
    public void testMultipleRangesReplaceMultipleShard() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(100, 1000, 2000));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(1500, 4000, 3000));
        assertEquals(
                Map.of(
                        shard1,
                        Set.of(
                                new HashRange(2001, 4000),
                                new HashRange(1001, 1500)
                        )
                ),
                addRes
        );

        var shard3 = new Shard("shard_3");
        addRes = cHash.addShard(shard3, Set.of(5000, -100, -200, 1300, 1250));
        assertEquals(
                Map.of(
                        shard2, Set.of(new HashRange(1001, 1300)),
                        shard1, Set.of(new HashRange(4001, -100))
                ),
                addRes
        );
    }

    @Test
    public void testAddMultipleShardsMultipleVnodesStress() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(100, 1000, 2000));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(200, 3000, -100));
        assertEquals(
                Map.of(
                        shard1,
                        Set.of(
                                new HashRange(2001, -100),
                                new HashRange(101, 200)
                        )
                ),
                addRes
        );

        var shard3 = new Shard("shard_3");
        addRes = cHash.addShard(shard3, Set.of(300, -200, 400));
        assertEquals(
                Map.of(
                        shard1, Set.of(new HashRange(201, 400)),
                        shard2, Set.of(new HashRange(3001, -200))
                ),
                addRes
        );

        var shard4 = new Shard("shard_4");
        addRes = cHash.addShard(shard4, Set.of(1500, 1800, 1700));
        assertEquals(
                Map.of(
                        shard1, Set.of(new HashRange(1001, 1800))
                ),
                addRes
        );

        var shard5 = new Shard("shard_5");
        addRes = cHash.addShard(shard5, Set.of(1600, 1750, 150));
        assertEquals(
                Map.of(
                        shard2, Set.of(new HashRange(101, 150)),
                        shard4, Set.of(
                                new HashRange(1701, 1750),
                                new HashRange(1501, 1600)
                        )
                ),
                addRes
        );

        var shard6 = new Shard("shard_6");
        addRes = cHash.addShard(shard6, Set.of(4000, -300));
        assertEquals(
                Map.of(
                        shard3, Set.of(new HashRange(3001, -300))
                ),
                addRes
        );

        assertEquals(shard2, cHash.getShardByKey(-100));
        assertEquals(shard4, cHash.getShardByKey(1602));
        assertEquals(shard2, cHash.getShardByKey(-150));
        assertEquals(shard3, cHash.getShardByKey(350));
    }

    @Test
    public void testAddAndRemoveSingleShard() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(100, 1000, 2000));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(1500));
        assertEquals(
                Map.of(
                        shard1, Set.of(new HashRange(1001, 1500))
                ),
                addRes
        );

        var removeRes = cHash.removeShard(shard2);
        assertEquals(
                Map.of(
                        shard1, Set.of(new HashRange(1001, 1500))
                ),
                removeRes
        );

        var random = new Random(System.currentTimeMillis());
        for (int i = 0; i < 100; ++i) {
            var key = random.nextInt();
            assertEquals(shard1, cHash.getShardByKey(key));
        }
    }

    @Test
    public void testAddNewAndRemoveOldShard() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(200, 1100, 2100));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(1600));
        assertEquals(
                Map.of(
                        shard1, Set.of(new HashRange(1101, 1600))
                ),
                addRes
        );

        var removeRes = cHash.removeShard(shard1);
        assertEquals(
                Map.of(
                        shard2, Set.of(new HashRange(1601, 1100))
                ),
                removeRes
        );

        var random = new Random(System.currentTimeMillis());
        for (int i = 0; i < 100; ++i) {
            var key = random.nextInt();
            assertEquals(shard2, cHash.getShardByKey(key));
        }
    }

    @Test
    public void testRemoveMultipleRangesSameShard() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(100, 500, 1000));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(2000, 300));
        assertEquals(
                Map.of(
                        shard1, Set.of(
                                new HashRange(1001, 2000),
                                new HashRange(101, 300)
                        )
                ),
                addRes
        );

        var shard3 = new Shard("shard_3");
        addRes = cHash.addShard(shard3, Set.of(250, 200, 1500, 1700));
        assertEquals(
                Map.of(
                        shard2, Set.of(
                                new HashRange(1001, 1700),
                                new HashRange(101, 250)
                        )
                ),
                addRes
        );

        var removeRes = cHash.removeShard(shard3);
        assertEquals(
                Map.of(
                        shard2, Set.of(
                                new HashRange(1001, 1700),
                                new HashRange(101, 250)
                        )
                ),
                removeRes
        );
    }

    @Test
    public void testRemoveMultipleRangesSameShardRangeBeginEnd() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(200, 1000, 2000));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(3000, 1500, 500));
        assertEquals(
                Map.of(
                        shard1, Set.of(
                                new HashRange(201, 500),
                                new HashRange(1001, 1500),
                                new HashRange(2001, 3000)
                        )
                ),
                addRes
        );

        var shard3 = new Shard("shard_3");
        addRes = cHash.addShard(shard3, Set.of(4000, -100, 700));
        assertEquals(
                Map.of(
                        shard1, Set.of(
                                new HashRange(501, 700),
                                new HashRange(3001, -100)
                        )
                ),
                addRes
        );

        var removeRes = cHash.removeShard(shard3);
        assertEquals(
                removeRes,
                Map.of(
                        shard1, Set.of(
                                new HashRange(501, 700),
                                new HashRange(3001, -100)
                        )
                )
        );
    }

    @Test
    public void testRemoveMultipleRangesMultipleShards() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(200, 1000, 2000));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(3000, 4000));
        assertEquals(
                Map.of(
                        shard1, Set.of(
                                new HashRange(2001, 4000)
                        )
                ),
                addRes
        );

        var shard3 = new Shard("shard_3");
        addRes = cHash.addShard(shard3, Set.of(5000, -100, 700, 500, 2500));
        assertEquals(
                Map.of(
                        shard1, Set.of(
                                new HashRange(4001, -100),
                                new HashRange(201, 700)
                        ),
                        shard2, Set.of(new HashRange(2001, 2500))
                ),
                addRes
        );

        var removeRes = cHash.removeShard(shard3);
        assertEquals(
                Map.of(
                        shard1, Set.of(
                                new HashRange(4001, -100),
                                new HashRange(201, 700)
                        ),
                        shard2, Set.of(new HashRange(2001, 2500))
                ),
                removeRes
        );
    }

    @Test
    public void testRemoveOldShardMultipleRangesMultipleShards() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(200, 1000, 2000));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(3000, 4000));
        assertEquals(
                Map.of(
                        shard1, Set.of(new HashRange(2001, 4000))
                ),
                addRes
        );

        var shard3 = new Shard("shard_3");
        addRes = cHash.addShard(shard3, Set.of(5000, -100, 700, 500, 2500));
        assertEquals(
                Map.of(
                        shard1, Set.of(
                                new HashRange(4001, -100),
                                new HashRange(201, 700)
                        ),
                        shard2, Set.of(new HashRange(2001, 2500))
                ),
                addRes
        );

        var removeRes = cHash.removeShard(shard2);
        assertEquals(
                Map.of(
                        shard3, Set.of(new HashRange(2501, 4000))
                ),
                removeRes
        );
    }

    @Test
    public void testRemoveOldestShardMultipleRangesMultipleShards() {
        var cHash = new ConsistentHashImpl<Integer>();

        var shard1 = new Shard("shard_1");
        var addRes = cHash.addShard(shard1, Set.of(200, 1000, 2000));
        assertEquals(Map.of(), addRes);

        var shard2 = new Shard("shard_2");
        addRes = cHash.addShard(shard2, Set.of(3000, 4000));
        assertEquals(
                Map.of(
                        shard1, Set.of(new HashRange(2001, 4000))
                ),
                addRes
        );

        var shard3 = new Shard("shard_3");
        addRes = cHash.addShard(shard3, Set.of(5000, -100, 700, 500, 2500));
        assertEquals(
                Map.of(
                        shard1, Set.of(
                                new HashRange(4001, -100),
                                new HashRange(201, 700)
                        ),
                        shard2, Set.of(new HashRange(2001, 2500))
                ),
                addRes
        );

        var removeRes = cHash.removeShard(shard1);
        assertEquals(
                Map.of(
                        shard3, Set.of(
                                new HashRange(701, 2000),
                                new HashRange(-99, 200)
                        )
                ),
                removeRes
        );
    }
}
