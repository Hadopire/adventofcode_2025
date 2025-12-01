#import "Basic";
#import "File";
#import "Math";

#load "d01.jai";

main :: () {
    days := (()->(string,string)).[
        d01
    ];

    iter :: 20;

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

        print("%ms\n", acc_time / iter);
    }
}