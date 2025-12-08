#scope_file

Int3 :: struct {
    x, y, z: s32;
};

Pair :: struct {
    a, b: s32;
    dist: s64;
}

UnionFind :: struct {
    parent: []s32;
    size:   []s32;
    count:    s32;
};

PQueue :: struct {
    pairs:   []Pair;
    count:     s32;
    max_count: s32;
};

uf_init :: (count: s32) -> UnionFind {
    uf : UnionFind;
    uf.parent = NewArray(count, s32, false);
    uf.size   = NewArray(count, s32, false);
    uf.count  = count;

    for i : 0..count - 1 {
        uf.parent[i] = i;
        uf.size[i]   = 1;
    }

    return uf;
}

uf_find :: (uf: UnionFind, n: s32) -> s32 {
    root := uf.parent[n];

    while root != uf.parent[root] {
        root = uf.parent[root];
    }

    uf.parent[n] = root;
    return root;
}

uf_union :: (uf: UnionFind, a: s32, b: s32) -> s32 {
    a_root := uf_find(uf, a);
    b_root := uf_find(uf, b);

    if a_root == b_root {
        return uf.size[a_root];
    }

    if uf.size[a_root] > uf.size[b_root] {
        uf.parent[b_root] = a_root;
        uf.size[a_root]  += uf.size[b_root];
        return uf.size[a_root];
    } else {
        uf.parent[a_root] = b_root;
        uf.size[b_root]  += uf.size[a_root];
        return uf.size[b_root];
    }
}

queue_init :: (size: s32) -> PQueue {
    queue: PQueue;
    queue.pairs = NewArray(xx size, T = Pair, initialized = false);
    queue.count = 0;
    queue.max_count = size;
    return queue;
}

queue_insert :: (queue: *PQueue, value: Pair) {
    if queue.count < queue.max_count {
        idx := queue.count;
        queue.pairs[idx] = value;
        while idx > 0 && queue.pairs[(idx - 1) / 2].dist < queue.pairs[idx].dist {
            queue.pairs[idx], queue.pairs[(idx - 1) / 2] = queue.pairs[(idx - 1) / 2], queue.pairs[idx];
            idx = (idx - 1) / 2;
        }

        queue.count += 1;
        return;
    }

    if value.dist > queue.pairs[0].dist {
        return;
    }
    
    queue.pairs[0] = value;

    idx := 0;
    while true {
        left  := 2 * idx + 1;
        right := 2 * idx + 2;
        largest := idx;

        if left < queue.count && queue.pairs[left].dist > queue.pairs[largest].dist {
            largest = left;
        }

        if right < queue.count && queue.pairs[right].dist > queue.pairs[largest].dist {
            largest = right;
        }

        if largest != idx {
            queue.pairs[idx], queue.pairs[largest] = queue.pairs[largest], queue.pairs[idx];
            idx = largest;
        } else {
            break;
        }
    }
}

square_distance :: (a: Int3, b: Int3) -> s64 {
    dx := cast(s64) a.x - b.x;
    dy := cast(s64) a.y - b.y;
    dz := cast(s64) a.z - b.z;
    return dx * dx + dy * dy + dz * dz;
};

#scope_export

d08 :: () -> string, string {
    input, _ := read_entire_file("d08.txt");

    second_gaulois := 0;
    first_gaulois  := 0;

    boxes: [..]Int3;
    array_reserve(*boxes, 1_000);

    while line := next_line(*input) {
        box := array_add(*boxes);
        scan2(line, "%,%,%", *box.x, *box.y, *box.z);
    }

    // part 1: priority queue to extract the 1000 closest pairs then Kruskal's

    uf    := uf_init(xx boxes.count);
    queue := queue_init(1000);

    for i : 0..boxes.count - 1 {
        for j : i + 1..boxes.count - 1 {
            dist := square_distance(boxes[i], boxes[j]);
            queue_insert(*queue, Pair.{xx i, xx j, dist});
        }
    }

    for pair : queue.pairs {
        uf_union(uf, pair.a, pair.b);
    }

    max0, max1, max2 := 0;
    for i : 0..uf.count - 1 {
        if uf.parent[i] == i {
            size := uf.size[i];
            if size > max0 {
                max2 = max1;
                max1 = max0;
                max0 = size;
            } else if size > max1 {
                max2 = max1;
                max1 = size;
            } else if size > max2 {
                max2 = size;
            }
        }
    }

    first_gaulois = max0 * max1 * max2;

    // part 2: the first smallest pair to complete the tree is actually
    // the largest edge in the Minimum Spanning Tree. Therefore we skip
    // the pair sorting stuff and just compute the MST with Prim's

    visited := NewArray(boxes.count, bool);
    costs   := NewArray(boxes.count, s64);
    answers := NewArray(boxes.count, s64);
    for i : 0..costs.count - 1 {
        costs[i] = S64_MAX;
    }
    costs[0] = 0;

    max_cost := 0;
    for i : 0..boxes.count - 1 {
        u := 0;

        min_cost := S64_MAX;
        for v : 0..boxes.count - 1 {
            if visited[v] == false && costs[v] < min_cost {
                min_cost = costs[v];
                u = v;
            }
        }

        if min_cost > max_cost {
            max_cost = min_cost;
            second_gaulois = answers[u];
        }

        visited[u] = true;

        for v : 0..boxes.count - 1 {
            if visited[v] then continue;

            cost := square_distance(boxes[u], boxes[v]);
            if cost < costs[v] {
                costs[v]   = cost;
                answers[v] = boxes[u].x * boxes[v].x;
            }
        }
    }

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}