#scope_file

Machine :: struct {
    lights:    s32;
    buttons: []s32;
    jolts:   []s32;
};

LightState :: struct {
    state:       s32;
    press_count: s32;
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

        buttons: [..]s32;
        for i : 1..s.count - 2 {
            button     : s32 = 0;
            button_str := s[i];
            advance(*button_str);
            while button_str.count {
                shift := cast(s32) (button_str[0] - #char "0");
                shift  = xx (bit_count - 1) - shift;
                button |= cast(s32) (1 << shift);
                advance(*button_str, min(button_str.count, 2));
            }
            array_add(*buttons, button);
        }
        machine.buttons = buttons;

        jolts: [..]s32;
        joltage := s[s.count - 1];
        advance(*joltage);
        while joltage.count {
            jolt := parse_int(*joltage, T=s32, ",}");
            array_add(*jolts, jolt);
            advance(*joltage);
        }
        machine.jolts = jolts;
    }

    return machines;
}

epsilon: float64: 0.000000001;

Coef :: struct {
    var: *Variable;
    c:    float64;
};

Variable :: struct {
    coefs:     []Coef;
    constant: float64;
    free:        bool;
    max:      float64;
    min:      float64;
};

Matrix :: struct {
    data: []float64;
    row:    s32;
    col:    s32;
};

Int2 :: struct {
    x,y: s32;
};

matrix_init :: (row: s32, col: s32) -> Matrix {
    m: Matrix;
    m.row = row;
    m.col = col;
    m.data = NewArray(row * col, float64);
    return m;
}

matrix_swap_rows :: (m: Matrix, r1: s32, r2: s32) {
    for c: 0..m.col - 1 {
        m[r1][c], m[r2][c] = m[r2][c], m[r1][c];
    }
}

operator [] :: (m: Matrix, index: int) -> []float64 {
    assert(index < m.row);
    return array_view(m.data, index * m.col);
}

gaussian_elimination :: (m: Matrix) -> []Int2, []s32 {
    pivots: [..]Int2;
    free:   [..]s32;

    // forward elimination
    row : s32 = 0;
    col : s32 = 0;
    while row < m.row - 1 && col < m.col - 1 {
        max_idx := row;
        max := abs(m[max_idx][col]);
        for i : row + 1..m.row - 1 {
            if abs(m[i][col]) > max {
                max = abs(m[i][col]);
                max_idx = i;
            }
        }

        if max_idx != row {
            matrix_swap_rows(m, row, max_idx);
        }

        if cmp(m[row][col], 0) {
            array_add(*free, col);
            col += 1;
            continue;
        }

        for i : row + 1..m.row - 1 {
            factor := m[i][col] / m[row][col];
            for j : col + 1..m.col - 1 {
                m[i][j] -= m[row][j] * factor;
            }
            m[i][col] = 0;
        }

        array_add(*pivots, Int2.{col, row});

        row += 1;
        col += 1;
    }

    // remaining free variables and pivot bookkeeping
    row = m.row - 1;
    while col < m.col - 1 && cmp(m[row][col], 0) {
        array_add(*free, col);
        col += 1;
    }
    if col < m.col - 1 {
        array_add(*pivots, Int2.{col, row});
        col += 1;
    }
    while col < m.col - 1 {
        array_add(*free, col);
        col += 1;
    }

    // back substitution
    for #v2 < 0..pivots.count - 1 {
        row = pivots[it].y;
        col = pivots[it].x;

        if cmp(m[row][col], 1) == false {
            factor := 1 / m[row][col];
            for j: col..m.col - 1 {
                m[row][j] *= factor;
            }
        }

        i := 0;
        while i < row {
            factor := m[i][col] / m[row][col];
            for j : col + 1..m.col - 1 {
                m[i][j] -= m[row][j] * factor;
            }
            m[i][col] = 0;
            i += 1;
        }
    }

    return pivots, free;
}

round :: (n: float64) -> float64 {
    f := floor(n);
    c := ceil(n);

    if abs(n - f) < 0.5 {
        return f;
    }
    return c;
}

cmp :: (a: float64, b: float64) -> bool {
    return abs(a-b) < epsilon;
}

eval :: (var: Variable) -> float64 {
    if var.free {
        return var.constant;
    }

    sum := var.constant;
    for coef: var.coefs {
        sum += coef.c * coef.var.constant;
    }
    return sum;
}

solve :: (variables: []Variable) -> s64, bool {
    sum: float64;
    for var: variables {
        r := eval(var);
        if r < -epsilon || abs(r - round(r)) > epsilon {
            return 0, false;
        }

        sum += r;
    }
    return xx round(sum), true;
}

solve :: (variables: []Variable, free: []s32, idx: s32) -> s64 {
    var := *variables[free[idx]];
    res := S64_MAX;
    
    if idx < free.count - 1 {
        i: float64 = 0;
        while i <= var.max {
            var.constant = i;
            res = min(res, solve(variables, free, idx + 1));
            i += 1.0;            
        }
        return res;
    }

    i: float64 = 0;
    while i <= var.max {
        var.constant = i;

        r, ok := solve(variables);
        if ok {
            if r >= res {
                break;
            } else {
                res = r;
            }
        }

        i += 1.0;
    }

    return res;
}

minimum_button_press :: (machine: Machine) -> s64 {
    variables := NewArray(machine.buttons.count, T=Variable);
    aug := matrix_init(xx machine.jolts.count, xx (machine.buttons.count + 1));
    for jolt : machine.jolts {
        shift := machine.jolts.count - 1 - it_index;
        r     := it_index;
        for button : machine.buttons {
            if (button >> shift) & 1 {
                aug[r][it_index] = xx 1;
                variables[it_index].max = xx jolt;
            }
        }
        aug[r][aug.col - 1] = xx jolt;
    }

    pivots, free := gaussian_elimination(aug);
    for pivot: pivots {
        var   := *variables[pivot.x];
        coefs : [..]Coef;

        for f: free {
            if cmp(aug[pivot.y][f], 0) == false {
                array_add(*coefs, Coef.{ *variables[f], aug[pivot.y][f] * -1 });
            }
        }

        var.coefs = coefs;
        var.constant = aug[pivot.y][aug.col - 1];
    }

    if free.count == 0 {
        return solve(variables);
    }

    for f: free {
        variables[f].free = true;
    }

    tightened := true;
    while tightened {
        tightened = false;
        for constraint: variables {
            if constraint.free then continue;

            for * x: constraint.coefs {
                x_idx := it_index;

                lb := -constraint.constant;
                ub := constraint.max-constraint.constant;
                for xi: constraint.coefs {
                    if it_index == x_idx continue;
                    lb -= ifx xi.c > 0 then xi.var.max * xi.c else xi.var.min * xi.c;
                    ub -= ifx xi.c > 0 then xi.var.min * xi.c else xi.var.max * xi.c;
                }

                if x.c < 0 {
                    ub, lb = lb, ub;
                }
                ub =  ceil(ub / x.c);
                lb = floor(lb / x.c);
                
                if x.var.max > ub {
                    x.var.max = ub;
                    tightened = true;
                }

                if x.var.min < lb {
                    x.var.min = lb;
                    tightened = true;
                }
            }
        }
    }

    return solve(variables, free, xx 0);
}

#scope_export

d10 :: () -> string, string {
    input, _ := read_entire_file("d10.txt");

    first_gaulois  := 0;
    second_gaulois := 0;

    machines := parse(input);

    queue: Queue(LightState);
    for machine : machines {
        // p1
        visited: [1024]bool;
        queue_clear(*queue);
        queue_push(*queue, LightState.{ 0, 0 });
        while bfs := queue.count {
            item        := queue_pop(*queue);
            press_count := item.press_count + 1;

            for button : machine.buttons {
                new_state := item.state ^ button;

                if visited[new_state] {
                    continue;
                }

                if new_state == machine.lights {
                    first_gaulois += press_count;
                    break bfs;
                }

                visited[new_state] = true;
                queue_push(*queue, LightState.{ new_state, press_count });
            }
        }

        // p2
        second_gaulois += minimum_button_press(machine);
    }

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}