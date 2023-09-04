import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

public class StressTest {
    private Map<Shard, Set<HashRange>> parseResult(JsonArray jsonResult) {
        var result = new HashMap<Shard, Set<HashRange>>();
        for (var curShard : jsonResult) {
            var shardObj = curShard.getAsJsonObject();
            var shardName = shardObj.getAsJsonObject("first").get("shardName").getAsString();
            var shard = new Shard(shardName);
            assert !result.containsKey(shard);

            var rangesSet = new HashSet<HashRange>();
            var ranges = shardObj.getAsJsonArray("second");
            for (var curRange : ranges) {
                var rangeObj = curRange.getAsJsonObject();
                var leftBorder = rangeObj.get("leftBorder").getAsInt();
                var rightBorder = rangeObj.get("rightBorder").getAsInt();
                var addRes = rangesSet.add(new HashRange(leftBorder, rightBorder));
                assert addRes;
            }

            var prevSet = result.put(shard, rangesSet);
            assert prevSet == null;
        }
        return result;
    }

    private void processGetShard(ConsistentHash<String> cHash, JsonObject op) {
        var key = op.get("key").getAsString();
        var result = cHash.getShardByKey(key).getName();
        var expectedResult = op.getAsJsonObject("result").get("shardName").getAsString();
        assertEquals(expectedResult, result);
    }

    private void processAddShard(ConsistentHash<String> cHash, JsonObject op) {
        var shardName = op.getAsJsonObject("newShard").get("shardName").getAsString();
        var shard = new Shard(shardName);
        var hashes = op.getAsJsonArray("vnodeHashes");
        var vnodes = new HashSet<Integer>();
        for (var x : hashes) {
            vnodes.add(x.getAsInt());
        }
        var result = cHash.addShard(shard, vnodes);
        var expectedResult = parseResult(op.getAsJsonArray("result"));
        assertEquals(expectedResult, result);
    }

    private void processRemoveShard(ConsistentHash<String> cHash, JsonObject op) {
        var shardName = op.getAsJsonObject("shard").get("shardName").getAsString();
        var shard = new Shard(shardName);
        var result = cHash.removeShard(shard);
        var expectedResult = parseResult(op.getAsJsonArray("result"));
        assertEquals(expectedResult, result);
    }

    private void doSingleTest(JsonArray operations) {
        var cHash = new ConsistentHashImpl<String>();
        for (var curOp : operations) {
            var op = curOp.getAsJsonObject();
            switch (op.get("type").getAsString()) {
                case "AddShardRequest" -> processAddShard(cHash, op);
                case "GetShardByKeyRequest" -> processGetShard(cHash, op);
                case "RemoveShardRequest" -> processRemoveShard(cHash, op);
            }
        }
    }

    @Test
    public void testStress() throws IOException {
        List<Path> filesList;
        try (var files = Files.list(Paths.get("resources"))) {
            filesList = files.toList();
        }
        for (var curPath : filesList) {
            if (!curPath.toString().endsWith(".json")) {
                continue;
            }
            String curJson;
            try (var reader = Files.newBufferedReader(curPath)) {
                curJson = reader.lines().collect(Collectors.joining("\n"));
            }
            doSingleTest(JsonParser.parseString(curJson).getAsJsonArray());
        }
    }
}
