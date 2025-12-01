#import "Basic";

#load "d01.jai";

main :: () {
    days := (()->(string,string)).[
        solve_d01
    ];

    iter :: 20;

    for day : days {
        acc_time : float64 = 0;
        a : string;
        b : string;

        for 1..iter {
            before := current_time_monotonic();
            a, b = day();
            after := current_time_monotonic();

            ms, _ := to_milliseconds(after - before);
            acc_time += ms;
        }
        print("d% part_1: % - part_2: % - %ms\n", formatInt(it_index+1, minimum_digits = 2), a, b, acc_time / iter);
    }
}