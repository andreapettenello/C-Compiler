let usage_msg = "Usage: compiler_driver [--lex| --parse| --codegen| -S] </path/to/program.c>"

type mode =
    | Full
    | Lex_only
    | Parse_only
    | Codegen_only
    | Assembly_only

let input_files = ref []
let selected_mode = ref Full 

let anon_fun filename = input_files := filename :: !input_files

let speclist =
    [
        ("--lex", Arg.Unit (fun () -> selected_mode := Lex_only), "Stop after lexing");
        ("--parse", Arg.Unit (fun () -> selected_mode := Parse_only), "Stop after parsing");
        ("--codegen", Arg.Unit (fun () -> selected_mode := Codegen_only), "Stop after assembly generation, before emitting code");
        ("-S", Arg.Unit (fun () -> selected_mode := Assembly_only), "Emit assembly, without assembling or linking");
    ]


let () = Arg.parse speclist anon_fun usage_msg

let c_to_preprocessor_suffix filename =
    let filename_without_suffix = Filename.chop_suffix filename ".c" in
    filename_without_suffix ^ ".i"

let c_to_assembly_suffix filename =
    let filename_without_suffix = Filename.chop_suffix filename ".c" in
    filename_without_suffix ^ ".s"

let run_preprocessor filename =
    let filename_ending_in_i = c_to_preprocessor_suffix filename in
        Sys.command ("gcc -E -P " ^ Filename.quote filename ^ " -o " ^ Filename.quote filename_ending_in_i)

let run_compiler mode filename =
    match mode with
        |Full|Assembly_only -> let assembly_file_channel = open_out (c_to_assembly_suffix filename) in
            output_string assembly_file_channel {|.text
                                            .globl main
                                            main:
                                                movl $2, %eax
                                                ret

                                            .section .note.GNU-stack,"",@progbits
                                                                                |};
            close_out assembly_file_channel;
            Ok ()
        |Lex_only|Parse_only|Codegen_only -> Ok ()

let run_assembler_and_linker filename = 
    let filename_without_suffix = Filename.chop_suffix filename ".s" in
        Sys.command ("gcc " ^ Filename.quote filename ^ " -o " ^ Filename.quote filename_without_suffix)

let () =
    match !input_files with
     |[filename] ->( match run_preprocessor filename with
            |0 ->
                  (match run_compiler !selected_mode filename with 
                        |Ok () -> Sys.remove(c_to_preprocessor_suffix filename);
                                  ( match !selected_mode with
                                        |Full -> (match run_assembler_and_linker (c_to_assembly_suffix filename) with
                                            |0 -> Sys.remove(c_to_assembly_suffix filename); 
                                                  exit 0
                                            |_ -> prerr_endline "Error: assembler/linker failed"; exit 1)
                                        |Lex_only|Parse_only|Codegen_only|Assembly_only -> exit 0)
                                                                                           
                        |Error message -> prerr_endline message; exit 1)
             |_ -> prerr_endline "Error: preprocessor failed"; exit 1)

     |_ -> prerr_endline usage_msg; exit 1


