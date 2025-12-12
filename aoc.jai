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

Queue :: struct ($T: Type) {
    allocator: Allocator;
    
    array: []T;
    count: s64;
    head:  s64;
    tail:  s64;
}

queue_grow :: (queue: *Queue($T)) {
    new_size  := queue.array.count * 2;

    if new_size == 0 {
        queue.allocator = context.allocator;
        queue.array = NewArray(32, T, false);
        return;
    }

    push_allocator(queue.allocator);
    new_array := NewArray(new_size, T, false);
    
    for i : 0..queue.array.count - 1 {
        new_array[i] = queue.array[(queue.head + i) % queue.array.count];
    }
    
    queue.head = 0;
    queue.tail = queue.array.count;
    free(queue.array.data);
    queue.array = new_array;
}

queue_push :: (queue: *Queue($T), value: T) {
    if queue.count == queue.array.count {
        queue_grow(queue);
    }

    queue.array[queue.tail] = value;
    queue.tail = (queue.tail + 1) % queue.array.count;
    queue.count += 1;
}

queue_pop :: (queue: *Queue($T)) -> T {
    assert(queue.count != 0);

    value := queue.array[queue.head];
    queue.head = (queue.head + 1) % queue.array.count;
    queue.count -= 1;
    return value;
}

queue_clear :: (queue: *Queue($T)) {
    queue.count = 0;
    queue.head = 0;
    queue.tail = 0;
}

#load "d01.jai";
#load "d02.jai";
#load "d03.jai";
#load "d04.jai";
#load "d05.jai";
#load "d06.jai";
#load "d07.jai";
#load "d08.jai";
#load "d09.jai";
#load "d10.jai";
#load "d11.jai";
#load "d12.jai";

main :: () {
    days := (()->(string,string)).[
        d01,
        d02,
        d03,
        d04,
        d05,
        d06,
        d07,
        d08,
        d09,
        d10,
        d11,
        d12,
    ];

    iter :: 100;
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