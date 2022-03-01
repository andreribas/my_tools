

type symbol_class = Uppercase | Lowercase | Number | Symbol
type symbol_class_tuple = char * symbol_class

let string_to_list_of_chars string =
    string
    |> String.to_seq
    |> List.of_seq

let list_of_chars_to_string list_of_chars =
    list_of_chars
    |> List.to_seq
    |> String.of_seq

let char_to_string char =
    char
    |> String.make 1

let to_symbol_class symbol =
    match symbol with
        'a' .. 'z' -> Lowercase |
        'A' .. 'Z' -> Uppercase |
        '0' .. '9' -> Number |
        _ -> Symbol

let green = "\027[32m"
let blue = "\027[34m"
let magenta = "\027[35m"
let cyan = "\027[36m"
let reset_color = "\027[m"

let to_ascii_colored_strings (symbol, symbol_class) =
    match symbol_class with
        Lowercase -> cyan ^ char_to_string symbol ^ reset_color |
        Uppercase -> blue ^ char_to_string symbol ^ reset_color |
        Number -> green ^ char_to_string symbol ^ reset_color |
        Symbol -> magenta ^ char_to_string symbol ^ reset_color
;;

let to_symbol_class_tuple symbol: symbol_class_tuple =
    (symbol, to_symbol_class symbol);;

let colorize text = 
    text
    |> string_to_list_of_chars
    |> List.map to_symbol_class_tuple
    |> List.map to_ascii_colored_strings
    |> List.fold_left (^) ""
    (* |> print_endline *)
;;

if Array.length Sys.argv == 2 then
    Sys.argv.(1)
    |> colorize
    |> print_endline
else
    let acc = ref [] in
        try
            while true do
                acc := read_line () :: !acc;
            done
        with
            End_of_file -> String.concat "\n" !acc |> colorize |> print_endline
;;

