#scope_file

Grid :: struct {
    map:    []u8;
    counts: []s64;
    width:    s32;
    height:   s32;
    splits:   s32;
};

count_timelines :: (grid: *Grid, px: s64, py: s64) -> s64 {
    y := py;
    while y < grid.height - 1 {
        if grid.map[y * (grid.width + 1) + px] == #char "^" {
            break;
        }

        y += 2;
    }

    if y == grid.height {
        return 0;
    }

    memo := grid.counts[y * grid.width + px];
    if memo {
        return memo;
    }

    split     := 0;
    timelines := 0;
    if grid.map[y * (grid.width + 1) + px - 1] == #char "." {
        split     += 1;
        timelines += count_timelines(grid, px - 1, y);
    }
    if grid.map[y * (grid.width + 1) + px + 1] == #char "." {
        split     += 1;
        timelines += count_timelines(grid, px + 1, y);
    }

    if split == 2 then timelines += 1;
    grid.splits += 1;

    grid.counts[y * grid.width + px] = timelines;
    return timelines;
}

#scope_export

d07 :: () -> string, string {
    input, _ := read_entire_file("d07.txt");

    grid   : Grid;
    startx : s64;

    for c : input {
        if c == #char "\n" then break;
        if c == #char "S" then startx = it_index;
        grid.width += 1;
    }
    grid.height = xx (input.count + 1) / (grid.width + 1);
    grid.map    = xx input;
    grid.counts = NewArray(grid.width * grid.height, T = s64);

    second_gaulois := count_timelines(*grid, startx, 0) + 1;
    first_gaulois  := grid.splits;

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}