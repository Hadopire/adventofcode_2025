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

lower_bound :: (array: []s64, n: s64) -> s64 {
    lo := 0;
    hi := array.count - 1;
    idx := array.count;

    while lo <= hi {
        mid := lo + (hi-lo)/2;
        if array[mid] >= n {
            idx = mid;
            hi = mid - 1;
        } else {
            lo = mid + 1;
        }
    }

    return idx;
}

d02 :: () -> string, string {
    input, _ := read_entire_file("d02.txt");

    first_gaulois := 0;
    second_gaulois := 0;

    table: Table(s64, bool);
    numbers: [..] s64;
    init(*table, 200_000);
    array_reserve(*numbers, 200_000);

    for b : 0..4 {
        base := pow10(b);
        m := base * 10 + 1;
        while m*base < 10_000_000_000 {
            for n : base..base*10-1 {
                value := m * n;
                _, new := find_or_add(*table, value);
                if new {
                    array_add(*numbers, value);
                }
            }
            m = m * 10 * base + 1;
        }
    }

    quick_sort(numbers, (a, b) => a-b); 

    while (input.count) {
        start, end: s64;

        start, _, input = string_to_int(input, T=s64);
        input = advance(input, 1);
        end, _, input = string_to_int(input, T=s64);
        if input.count then input = advance(input, 1);

        d_start := count_digits(start);
        d_end := count_digits(end);
        for digit : d_start..d_end {
            if digit % 2 != 0 then continue;

            base := pow10(digit/2-1);
            for n : base..base*10-1 {
                value := (base * 10 + 1) * n;
                if end <= value then break;
                if start <= value then first_gaulois += value;
            }
        }

        idx := lower_bound(numbers, start);
        while numbers[idx] <= end {
            second_gaulois += numbers[idx];
            idx += 1;
        }
    }

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}