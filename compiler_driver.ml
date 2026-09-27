let usage_msg = "Usage: compiler_driver <file.c>"

let verbose = ref false
let input_files = ref []
let output_file = ref ""

let anon_fun filename = input_files := filename :: !input_files

let speclist =
    [
        ("-verbose", Arg.Set verbose, "Output debug information");
        ("-o", Arg.Set_string output_file, "Set output file name");
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

let run_compiler filename =
    let assembly_file_channel = open_out (c_to_assembly_suffix filename) in
        let output_string assembly_file_channel "Nothing";
        close_out assembly_file_channel

let run_assembler_and_linker filename = 
    let filename_without_suffix = Filename.chop_suffix filename ".s" in
        Sys.command ("gcc " ^ Filename.quote filename ^ " -o " ^ Filename.quote filename_without_suffix)

let () =
    match !input_files with
     |[filename] ->
             (match run_preprocessor filename with
                    |0 -> run_compiler filename;
                          Sys.remove (c_to_preprocessor_suffix filename);
                          run_assembler_and_linker_filename;
                          Sys.remove (c_to_assembly_suffix filename)
                    |_ -> prerr_endline "Error: preprocessor command failed"; exit 1)
     |_ ->
            prerr_endline usage_msg;
            exit 1



