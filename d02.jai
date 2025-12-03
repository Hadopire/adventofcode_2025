count_digits :: (n: s64) -> s64 {
    if n < 10 then return 1;
    if n < 100 then return 2;
    if n < 1_000 then return 3;
    if n < 10_000 then return 4;
    if n < 100_000 then return 5;
    if n < 1_000_000 then return 6;
    if n < 10_000_000 then return 7;
    if n < 100_000_000 then return 8;
    if n < 1_000_000_000 then return 9;
    return 10;
}

pow10 :: (exp: s64) -> s64 {
    if exp == 0 then return 1;
    if exp == 1 then return 10;
    if exp == 2 then return 100;
    if exp == 3 then return 1_000;
    if exp == 4 then return 10_000;
    if exp == 5 then return 100_000;
    if exp == 6 then return 1_000_000;
    if exp == 7 then return 10_000_000;
    if exp == 8 then return 100_000_000;
    if exp == 9 then return 1_000_000_000;
    return 10_000_000_000;
}

sum_of_repeating_pattern :: (min_value: s64, max_value: s64, length: s64, repeat: s64) -> s64 {
    base := pow10(length-1);
    m := 1;
    for 1..repeat-1 {
        m = m * base * 10 + 1;
    }

    a1 := max(base, (min_value+m-1) / m);
    an := min(base*10-1, max_value / m);
    n := an - a1 + 1;

    return ((n * (a1 + an)) / 2) * m;
}

d02 :: () -> string, string {
    input, _ := read_entire_file("d02.txt");

    first_gaulois := 0;
    second_gaulois := 0;

    while (input.count) {
        start, end: s64;

        start, _, input = string_to_int(input, T=s64);
        input = advance(input, 1);
        end, _, input = string_to_int(input, T=s64);
        if input.count then input = advance(input, 1);

        d_start := count_digits(start);
        d_end := count_digits(end);

        // part 1
        for length : d_start..d_end {
            if length % 2 != 0 then continue;
            pattern := length / 2;
            repeat := 2;
            first_gaulois += sum_of_repeating_pattern(start, end, pattern, repeat);
        }

        // part 2
        for length : d_start..d_end {
            sums : [5]s64;
            for pattern: 1..length/2 {
                if length%pattern != 0 then continue;
                repeat := length / pattern;
                sum := sum_of_repeating_pattern(start, end, pattern, repeat);
                for factor: 1..pattern-1 {
                    if pattern % factor != 0 then continue;
                    sum -= sums[factor-1];
                }
                sums[pattern-1] = sum;
                second_gaulois += sum;
            }
        }

    }

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}