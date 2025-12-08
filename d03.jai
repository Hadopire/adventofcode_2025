#scope_file

highest_digit :: (str: string) -> s64, s64 {
    highest_idx := 0;
    highest := 0;
    for c : str {
        if c > highest {
            highest = c;
            highest_idx = it_index; 
        }
    }

    return highest - #char "0", highest_idx;
}

joltage :: (l: string, n: s64) -> s64 {
    line := l;
    joltage := 0;
    for #v2 < remaining : 0..n-1 {
        digit, idx := highest_digit(slice(line, 0, line.count - remaining));
        line = slice(line, idx + 1, line.count);
        joltage = joltage * 10 + digit;
    }

    return joltage;
}

#scope_export

d03 :: () -> string, string {
    input, _ := read_entire_file("d03.txt");

    first_gaulois := 0;
    second_gaulois := 0;

    while line := next_line(*input) {
        first_gaulois += joltage(line, 2);
        second_gaulois += joltage(line, 12);
    } 
    
    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}