#scope_file

Int2 :: struct {
    x: s32;
    y: s32;
};

Grid :: struct {
    map    : []u8;
    counts : []s8;
    width  :  s64;
    height :  s64;
};

all_neighbours       :: Int2.[ Int2.{-1, -1}, Int2.{ 0, -1}, Int2.{ 1, -1}, Int2.{-1, 0}, Int2.{1, 0}, Int2.{-1, 1}, Int2.{0, 1}, Int2.{1, 1} ];
right_neighbours     :: Int2.[ Int2.{ 0, -1}, Int2.{ 1, -1}, Int2.{ 1,  0}, Int2.{ 0, 1}, Int2.{1, 1} ];
left_neighbours      :: Int2.[ Int2.{-1, -1}, Int2.{ 0, -1}, Int2.{-1,  0}, Int2.{-1, 1}, Int2.{0, 1} ];
top_neighbours       :: Int2.[ Int2.{-1, -1}, Int2.{ 0, -1}, Int2.{ 1, -1}, Int2.{-1, 0}, Int2.{1, 0} ];
bot_neighbours       :: Int2.[ Int2.{-1,  0}, Int2.{ 1,  0}, Int2.{-1,  1}, Int2.{ 0, 1}, Int2.{1, 1} ];
top_right_neighbours :: Int2.[ Int2.{ 0, -1}, Int2.{ 1, -1}, Int2.{ 1,  0} ];
top_left_neighbours  :: Int2.[ Int2.{-1, -1}, Int2.{ 0, -1}, Int2.{-1,  0} ];
bot_right_neighbours :: Int2.[ Int2.{ 1,  0}, Int2.{ 0,  1}, Int2.{ 1,  1} ];
bot_left_neighbours  :: Int2.[ Int2.{-1,  0}, Int2.{-1,  1}, Int2.{ 0,  1} ];

grid_init :: (input: string) -> Grid {
    grid: Grid;

    for c : input {
        if c == #char "\n" then break;
        grid.width += 1;
    }
    grid.height = (input.count + 1) / (grid.width + 1);
    grid.map    = xx input;

    counts : [..]s8;
    array_resize(*counts, grid.width * grid.height);
    grid.counts = counts;

    return grid;
}

grid_get_neighbours :: (grid: Grid, pos: Int2) -> []Int2 {
    bot   := pos.y == 0;
    top   := pos.y == grid.height - 1;
    right := pos.x == 0;
    left  := pos.x == grid.width - 1;

    if top {
        if right then return top_right_neighbours;
        if left then return top_left_neighbours;
        return top_neighbours;
    }
    if bot {
        if right then return bot_right_neighbours;
        if left then return bot_left_neighbours;
        return bot_neighbours;
    }
    if right then return right_neighbours;
    if left then return left_neighbours;

    return all_neighbours;
}

grid_get :: (grid: Grid, pos: Int2) -> *u8 {
    return *grid.map[pos.y * (grid.width + 1) + pos.x]; 
}

grid_get_count :: (grid: Grid, pos: Int2) -> *s8 {
    return *grid.counts[pos.y * (grid.width) + pos.x];
}

grid_is_paper :: (grid: Grid, pos: Int2) -> bool {
    return <<grid_get(grid, pos) == #char "@";
}

grid_count_paper_around :: (grid: Grid, pos: Int2) -> s8 {
    count: s8   = 0;
    neighbours := grid_get_neighbours(grid, pos);
    for n : neighbours {
        p := Int2.{pos.x+n.x, pos.y+n.y};
        if grid_is_paper(grid, p) {
            count += 1;
        }
    }

    return count;
}

grid_full_scan :: (grid: Grid) -> [..]Int2 {
    array : [..]Int2;
    array_reserve(*array, grid.height * grid.width);

    for y : 0..grid.height-1 {
        for x: 0..grid.width-1 {
            pos := Int2.{xx x, xx y};
            if grid_is_paper(grid, pos) {
                count := grid_count_paper_around(grid, pos);
                <<grid_get_count(grid, pos) = count;
                
                if count < 4 {
                    array_add(*array, pos);
                }
            }
        }
    }

    return array;
}

#scope_export

d04 :: () -> string, string {
    input, _ := read_entire_file("d04.txt");
    
    grid  := grid_init(input);  
    stack := grid_full_scan(grid);

    first_gaulois  := stack.count;
    second_gaulois := 0;

    while stack.count {
        pos := stack[stack.count-1];
        stack.count -= 1;

        second_gaulois += 1;
        <<grid_get(grid, pos) = #char ".";

        neighbours := grid_get_neighbours(grid, pos);
        for n : neighbours {
            p     := Int2.{pos.x+n.x, pos.y+n.y};
            count := grid_get_count(grid, p);
            
            if grid_is_paper(grid, p) {
                <<count -= 1;
                if <<count == 3 {
                    array_add(*stack, p);
                }
            }
        }
    }

    return sprint("%", first_gaulois), sprint("%", second_gaulois); 
}