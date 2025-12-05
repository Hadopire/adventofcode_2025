#scope_file

Range :: struct {
    min: s64;
    max: s64;
};

next_line :: (str: *string) -> string {
    _, line:, <<str = split_from_left(<<str, #char "\n");
    return line;
}

ranges_overlap :: (a: Range, b: Range) -> bool {
    return a.min <= b.max && b.min <= a.max;
}

lower_bound :: (array: []Range, n: s64) -> s64 {
    low := 0;
    high := array.count - 1;
    idx := 0;

    while (low <= high) {
        mid := low + (high - low) / 2;

        if (array[mid].max >= n) {
            idx = mid;
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }

    return idx;
}

#scope_export

d05 :: () -> string, string {
    input, _ := read_entire_file("d05.txt");

    first_gaulois := 0;
    second_gaulois := 0;

    ranges: [..]Range;
    while line := next_line(*input) {
        range := array_add(*ranges);
        scan2(line, "%-%", *range.min, *range.max);
    }

    // merge ranges
    quick_sort(ranges, (a, b) => a.min - b.min);
    head := 0;
    tail := 1;
    while tail < ranges.count {
        if ranges_overlap(ranges[head], ranges[tail]) {
            ranges[head].min = min(ranges[head].min, ranges[tail].min);
            ranges[head].max = max(ranges[head].max, ranges[tail].max);
            tail += 1;
        } else {
            head += 1;
            ranges[head] = ranges[tail];
            tail += 1;
        }
    }
    ranges.count = ranges.count - tail + head + 1;

    // p1
    while line := next_line(*input) {
        value := string_to_int(line, T = s64);
        idx   := lower_bound(ranges, value);
        if (ranges[idx].min <= value && value <= ranges[idx].max) {
            first_gaulois += 1;
        }
    }

    // p2
    for range : ranges {
        second_gaulois += range.max - range.min + 1;
    }

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}