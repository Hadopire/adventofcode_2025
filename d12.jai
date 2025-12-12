#scope_file

Region :: struct {
    width:     s32;
    height:    s32;
    shapes: [6]s32;
};

#scope_export

d12 :: () -> string, string {
    input, _ := read_entire_file("d12.txt");

    first_gaulois  := 0;
    second_gaulois := 0;

    shapes: [6]s32;

    for * shape: shapes {
        _, shape_str:, input = split_from_left(input, "\n\n");
        advance(*shape_str, 3);
        for c : shape_str {
            if c == #char "#" then <<shape += 1;
        }
    }

    while line := next_line(*input) {
        w  : s32 = (line[0]  - #char "0") * 10 + line[1]  - #char "0";
        h  : s32 = (line[3]  - #char "0") * 10 + line[4]  - #char "0";
        s0 : s32 = (line[7]  - #char "0") * 10 + line[8]  - #char "0";
        s1 : s32 = (line[10] - #char "0") * 10 + line[11] - #char "0";
        s2 : s32 = (line[13] - #char "0") * 10 + line[14] - #char "0";
        s3 : s32 = (line[16] - #char "0") * 10 + line[17] - #char "0";
        s4 : s32 = (line[19] - #char "0") * 10 + line[20] - #char "0";
        s5 : s32 = (line[22] - #char "0") * 10 + line[23] - #char "0";

        area     := w * h;
        coverage := s0 * shapes[0] + s1 * shapes[1] + s2 * shapes[2] + s3 * shapes[3] + s4 * shapes[4] + s5 * shapes[5];
        if area < coverage {
            continue;
        }

        sc := s0 + s1 + s2 + s3 + s4 + s5;
        ub := (sc * 3) / w * (sc * 3) / h;
        if ub < area {
            first_gaulois += 1;
        }
    }


    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}