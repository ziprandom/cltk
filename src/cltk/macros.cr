# writes the contents of block to code_file_name, runs it
# and inserts the output into the file
macro insert_output_of(code_file_name = ".___generated___code.cr", debug = false, &block)
  {%
   # get the absolute directory of the file from which the macro was
   # called
    called_from_path_abs = block.filename.gsub(/\/[^\/]+$/, "\/")
    # create the path for the intermediate code
    intermediate_program_path = called_from_path_abs + code_file_name
    ## Write Block to file
    system(
      "cat << 'EOF' > #{intermediate_program_path}\n" +
      block.body.stringify + "\nEOF"
    )
    result = run(intermediate_program_path)
    if debug
      pp result
    else
      system("rm #{intermediate_program_path}")
    end
     %}
  {{result}}
end
