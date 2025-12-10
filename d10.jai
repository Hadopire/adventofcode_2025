#scope_file

Machine :: struct {
    lights:    u32;
    buttons: []u32;
    jolts:   []u32;
};

LightState :: struct {
    state:       u32;
    press_count: u32;
};

parse :: (input: string) -> []Machine {
    machines: [..]Machine;

    while line := next_line(*input) {
        machine := array_add(*machines);

        s := split(line, cast(u8) #char " ");

        lights    := s[0];
        bit_count := 0;
        advance(*lights); 
        while lights[0] != #char "]" {
            if lights[0] == #char "#" {
                machine.lights = (machine.lights << 1) | 1;
            } else {
                machine.lights <<= 1;
            }
            bit_count += 1;
            advance(*lights);
        }

        buttons: [..]u32;
        for i : 1..s.count - 2 {
            button     : u32 = 0;
            button_str := s[i];
            advance(*button_str);
            while button_str.count {
                bit := cast(u32) (button_str[0] - #char "0");
                bit  = xx (bit_count - 1) - bit;
                button |= cast(u32) (1 << bit);
                advance(*button_str, min(button_str.count, 2));
            }
            array_add(*buttons, button);
        }
        machine.buttons = buttons;

        jolts: [..]u32;
        joltage := s[s.count - 1];
        advance(*joltage);
        while joltage.count {
            jolt := parse_int(*joltage, T=u32, ",}");
            array_add(*jolts, jolt);
            advance(*joltage);
        }
        machine.jolts = jolts;
    }

    return machines;
}

#scope_export

d10 :: () -> string, string {
    input, _ := read_entire_file("d10.txt");

    first_gaulois  := 0;
    second_gaulois := 0;

    machines := parse(input);

    queue: Queue(LightState);
    for machine : machines {
        visited: [1024]bool;
        queue_clear(*queue);
        
        queue_push(*queue, LightState.{ 0, 0 });
        while queue.count {
            item        := queue_pop(*queue);
            press_count := item.press_count + 1;

            for button : machine.buttons {
                new_state := item.state ^ button;

                if visited[new_state] {
                    continue;
                }

                if new_state == machine.lights {
                    first_gaulois += press_count;
                    continue machine;
                }

                visited[new_state] = true;
                queue_push(*queue, LightState.{ new_state, press_count });
            }

        }
    }

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}