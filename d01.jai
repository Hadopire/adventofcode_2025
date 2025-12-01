mod :: (a: s32, b: s32) -> s32 {
    return (a%b+b)%b;
}

d01 :: () -> string, string {
    input, _ := read_entire_file("d01.txt");
    dial : s32 = 50;
    first_gaulois := 0;
    second_gaulois := 0;

    while input.count != 0 {
        c, value: s32;

        p := dial;
        c = input[0];
        input = advance(input, 1);
        value, _, input = string_to_int(input, T = s32);
        if c == #char "L" {
            if dial then p = 100-dial;
            dial = mod(dial-value, 100);
        } else if c == #char "R" {
            dial = (dial+value)%100;
        }

        if input.count then input = advance(input, 1);
        if dial == 0 then first_gaulois += 1;
        second_gaulois += (p + value)/100;
    }

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}