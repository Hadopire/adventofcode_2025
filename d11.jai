#scope_file

Node :: struct {
    conn: [..]s32;
};

hash_node_name :: (str: string) -> s64 {
    a : s64 = str[0] - #char "a";
    b : s64 = str[1] - #char "a";
    c : s64 = str[2] - #char "a";
    return a * 26 * 26 + b * 26 + c;
}

get_node_idx :: (nodes: *[..]Node, table: []s32, name: string) -> s32 {
    hash := hash_node_name(name);
    if table[hash] {
        return table[hash] - 1;
    }

    node := array_add(nodes);
    idx  := nodes.count - 1;
    table[hash] = xx (idx + 1);
    return xx idx;
}

count_path :: (nodes: []Node, start: s32, end: s32) -> s64 {
    Ctx :: struct { nodes: []Node; memo: []s64; end: s32; };
    dfs :: (ctx: Ctx, idx: s32) -> s64 {
        using ctx;

        if memo[idx] {
            return memo[idx] - 1;
        } else if idx == end {
            return 1;
        }

        count: s64;
        node  := nodes[idx];
        for conn: node.conn {
            count += dfs(ctx, conn);
        }

        memo[idx] = count + 1;
        return count;
    }

    memo := NewArray(nodes.count, s64);
    return dfs(Ctx.{ nodes, memo, end }, start);
}

#scope_export

d11 :: () -> string, string {
    input, _ := read_entire_file("d11.txt");

    first_gaulois  := 0;
    second_gaulois := 0;

    nodes:  [..]Node;
    table := NewArray(20_000, s32);
    array_reserve(*nodes, 1_000);

    while line := next_line(*input) {
        node_idx := get_node_idx(*nodes, table, line);
        node     := *nodes[node_idx];

        at := 0;
        while line[at] != #char " " {
            at += 1;
        }
        at += 1;

        while at < line.count {
            array_add(*node.conn, get_node_idx(*nodes, table, slice(line, at, 3)));
            at += 4;
        }
    }
    
    start_node := get_node_idx(*nodes, table, "you");
    end_node   := get_node_idx(*nodes, table, "out");
    svr_node   := get_node_idx(*nodes, table, "svr");
    fft_node   := get_node_idx(*nodes, table, "fft");
    dac_node   := get_node_idx(*nodes, table, "dac");

    first_gaulois = count_path(nodes, start_node, end_node);
    second_gaulois = count_path(nodes, svr_node, fft_node);
    second_gaulois *= count_path(nodes, fft_node, dac_node);
    second_gaulois *= count_path(nodes, dac_node, end_node);

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}