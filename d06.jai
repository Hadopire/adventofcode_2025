d06 :: () -> string, string {
    input, _ := read_entire_file("d06.txt");
    
    first_gaulois := 0;
    second_gaulois := 0;

    lines     := split_lines(input);
    numbers   := array_view(lines, 0, lines.count - 1);
    operators := lines[lines.count - 1];
    at        := 0;
    while operators.count {
        result := ifx operators[0] == #char "*" then 1 else 0;
        for line : numbers {
            value := 0;
            
            n := eat_spaces(slice(line, at, line.count));
            while n && n[0] != #char " " {
                value = value * 10 + n[0] - #char "0";
                n = advance(n);
            }
            
            if operators[0] == {
            case #char "*"; result *= value;
            case #char "+"; result += value;
            }
        }

        first_gaulois += result;
        operators = eat_spaces(advance(operators));
        at = lines[lines.count - 1].count - operators.count - 1;
    }

    operators  = lines[lines.count - 1];
    result    := ifx operators[0] == #char "*" then 1 else 0;
    for i : 0..lines[0].count-1 {
        value := 0;
        for line : numbers {
            if line[i] != #char " " {
                value = value * 10 + line[i] - #char "0";
            }
        }

        if value == 0 {
            second_gaulois += result;
            operators       = eat_spaces(advance(operators));
            result = ifx operators[0] == #char "*" then 1 else 0;

            continue;
        }

        if operators[0] == {
        case #char "*"; result *= value;
        case #char "+"; result += value;
        }
    }
    second_gaulois += result;

    return sprint("%", first_gaulois), sprint("%", second_gaulois);
}