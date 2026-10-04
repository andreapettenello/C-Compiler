type token = 
    |Identifier of string
    |Constant of int
    |IntKeyword
    |VoidKeyword
    |ReturnKeyword
    |OpenParenthesis
    |CloseParenthesis
    |OpenBrace
    |CloseBrace
    |Semicolon

let constant_pattern = Str.regexp {|[0-9]+\b|}
let int_keyword_pattern = Str.regexp {|int\b|}
let void_keyword_pattern = Str.regexp {|void\b|}
let return_keyword_pattern = Str.regexp {|return\b|}
let identifier_pattern = Str.regexp {|[a-zA-Z_][a-zA-Z0-9_]*\b|}
let open_parenthesis_pattern = Str.regexp {|(|}
let close_parenthesis_pattern = Str.regexp {|)|}
let open_brace_pattern = Str.regexp {|{|}
let close_brace_pattern = Str.regexp {|}|}
let semicolon_pattern = Str.regexp {|;|}

let rules = 
    [ (constant_pattern, fun text -> Constant (int_of_string text));
      (int_keyword_pattern, fun _ -> IntKeyword);
      (void_keyword_pattern, fun _ -> VoidKeyword);
      (return_keyword_pattern, fun _ -> ReturnKeyword);
      (identifier_pattern, fun text -> Identifier text);
      (open_parenthesis_pattern, fun _ -> OpenParenthesis);
      (close_parenthesis_pattern, fun _ -> CloseParenthesis);
      (open_brace_pattern, fun _ -> OpenBrace);
      (close_brace_pattern, fun _ -> CloseBrace);
      (semicolon_pattern, fun _ -> Semicolon)
    ]

let is_whitespace (character: char): bool = 
    match character with
        | ' ' | '\t' | '\n' | '\r' | '\011' | '\012' -> true
        | _ -> false

let rec choose_best candidates best = 
    match candidates with 
        |[] -> best
        |current_candidate::remaining_candidates -> (match current_candidate with
                                                        |None -> choose_best remaining_candidates best
                                                        |Some (matched_string, _) -> (match best with
                                                            |None -> choose_best remaining_candidates current_candidate
                                                            |Some (best_text, _) ->
                                                                if String.length matched_string > String.length best_text then
                                                                    choose_best remaining_candidates current_candidate
                                                                else 
                                                                    choose_best remaining_candidates best))
let recognize_token (text: string): (token * int, string) result = 
        let possible_tokens = List.map
             (fun (pattern, make_token) ->
                    match Str.string_match pattern text 0 with
                        |true -> Some (Str.matched_string text, make_token)
                        |false -> None)
             rules in match choose_best possible_tokens None with
                |None -> Error "Error: no matching token found"
                |Some (matched_string, make_token) -> Ok (make_token matched_string, String.length matched_string)
    
let rec tokenize (text: string) : (token list, string) result =
    if text = "" then
        Ok []
    else if is_whitespace text.[0] then
        let remaining_text = String.sub text 1 (String.length text - 1) in
                tokenize remaining_text
    else
        match recognize_token text with
            |Ok (token, token_length) -> let remaining_text = String.sub text token_length (String.length text - token_length) in
                                            (match tokenize remaining_text with
                                                |Ok token_list -> Ok (token::token_list)
                                                |Error message -> Error message)
            |Error message -> Error message
                    


(* Testing *)
let string_of_token token =
    match token with
    | Identifier name -> Printf.sprintf "Identifier %S" name
    | Constant value -> Printf.sprintf "Constant %d" value
    | IntKeyword -> "IntKeyword"
    | VoidKeyword -> "VoidKeyword"
    | ReturnKeyword -> "ReturnKeyword"
    | OpenParenthesis -> "OpenParenthesis"
    | CloseParenthesis -> "CloseParenthesis"
    | OpenBrace -> "OpenBrace"
    | CloseBrace -> "CloseBrace"
    | Semicolon -> "Semicolon"

let print_tokenization_result result =
    match result with
    | Ok tokens ->
        let token_names = List.map string_of_token tokens in
        Printf.printf "Ok [%s]\n" (String.concat "; " token_names)
    | Error message ->
        Printf.printf "Error %S\n" message


