#import "Basic";
#import "File";
#import "Math";
#import "Hash_Table";
#import "Sort";
#import "IntroSort";
#import "String";

next_line :: (str: *string) -> string {
    _, line:, <<str = split_from_left(<<str, #char "\n");
    return line;
}

split_lines :: (s: string) -> [] string {
    return split(s, cast(u8) #char "\n");
}

#load "d01.jai";
#load "d02.jai";
#load "d03.jai";
#load "d04.jai";
#load "d05.jai";
#load "d06.jai";
#load "d07.jai";

main :: () {
    days := (()->(string,string)).[
        d01,
        d02,
        d03,
        d04,
        d05,
        d06,
        d07,
    ];

    iter :: 50;
    total : float64 = 0.0;

    context.allocator = temp;
    for day_fn : days {
        day_n := it_index + 1;
        acc_time : float64 = 0;
        a : string;
        b : string;

        for 1..iter {
            before := current_time_monotonic();
            a, b = day_fn();
            after := current_time_monotonic();

            if it == 1 {
                print("d% part_1: % - part_2: % - ", formatInt(day_n, minimum_digits = 2), a, b);
            }

            us, _ := to_microseconds(after - before);
            ms := us / 1000.0;
            acc_time += ms;

            reset_temporary_storage();
        }

        day_time := acc_time / iter;
        print("%ms\n", day_time);

        total += day_time;
    }

    print("total: %ms\n", total);
}