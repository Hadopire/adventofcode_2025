#scope_file

Int2 :: struct {
    x, y: s64;
};

Segment :: struct {
    pos: s64;
    min: s64;
    max: s64;
};

upper_bound :: (array: []Segment, n: s64) -> s64 {
    low := 0;
    high := array.count - 1;
    idx := 0;

    while (low <= high) {
        mid := low + (high - low) / 2;

        if (array[mid].pos > n) {
            idx = mid;
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }

    return idx;
}

#scope_export

d09 :: () -> string, string {
    input, _ := read_entire_file("d09.txt");

    second_gaulois := 0;
    first_gaulois  := 0;

    tiles: [..]Int2;
    v_seg: [..]Segment;
    h_seg: [..]Segment;

    while line := next_line(*input) {
        tile := array_add(*tiles);
        scan2(line, "%,%", *tile.x, *tile.y);
    }

    for i : 0..tiles.count - 1 {
        a := tiles[i];
        b := tiles[(i + 1) % tiles.count];
        if a.x == b.x {
            seg := array_add(*v_seg);
            seg.pos = a.x;
            seg.min = min(a.y, b.y) + 1;
            seg.max = max(a.y, b.y) - 1;
        } else {
            seg := array_add(*h_seg);
            seg.pos = a.y;
            seg.min = min(a.x, b.x) + 1;
            seg.max = max(a.x, b.x) - 1;
        }
    }

    insertion_sort(v_seg, (a, b) => a.pos - b.pos);
    insertion_sort(h_seg, (a, b) => a.pos - b.pos);

    for i : 0..tiles.count - 1 {
        for j : i + 1..tiles.count - 1 {
            width  := abs(tiles[i].x - tiles[j].x) + 1;
            height := abs(tiles[i].y - tiles[j].y) + 1;
            area   := width * height;
            first_gaulois = max(area, first_gaulois);

            if (area < second_gaulois) {
                continue;
            }

            min_x := min(tiles[i].x, tiles[j].x);
            min_y := min(tiles[i].y, tiles[j].y);
            max_x := max(tiles[i].x, tiles[j].x);
            max_y := max(tiles[i].y, tiles[j].y);

            is_cut := false;
            v_idx  := upper_bound(v_seg, min_x);
            while v_idx < v_seg.count - 1 && v_seg[v_idx].pos < max_x {
                seg := v_seg[v_idx];
                if min_y < seg.max && seg.min < max_y {
                    is_cut = true;
                    break;
                }
                v_idx += 1;
            }

            if is_cut {
                continue;
            }

            h_idx  := upper_bound(h_seg, min_y);
            while h_idx < h_seg.count - 1 && h_seg[h_idx].pos < max_y {
                seg := h_seg[h_idx];
                if min_x < seg.max && seg.min < max_x {
                    is_cut = true;
                    break;
                }
                h_idx += 1;
            }

            if is_cut == false {
                second_gaulois = area;
            }
        }
    }

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}