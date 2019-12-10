#!/usr/bin/env bash -l
POSITIONAL=()
while [[ $# -gt 0 ]]
do
    key="$1"
    
    case $key in
        -h|--help)
            echo "render.sh -h [-o|--output-dir] [input-file]"
            exit
            ;;
        -o|--output-dir)
            output_dir="$2"
            shift 2
            ;;
        *)    
            POSITIONAL+=("$1") # save it in an array for later
            shift # past argument
            ;;
    esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

filename=$(basename -- "$1")
filename="${filename%.*}"

R -q -e "rmarkdown::render(\"$1\", output_dir=\"$output_dir\")"

open $output_dir/$filename.html
