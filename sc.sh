#!/bin/bash

# Convert directory files to ChatGPT-friendly prompt with filenames
# Usage: ./script.sh [directory] [file-extension]

directory="${1:-.}"       # Use specified directory or current directory
extension="${2:-*}"       # Use specified extension or all files

# Header for clarity
echo "### File Contents of: $directory/ ###"
echo "-------------------------------------"

# Process files
find "$directory" -maxdepth 1 -type f -name "*.$extension" -print0 | while IFS= read -r -d $'\0' file; do
    if [[ "$(file -b --mime-type "$file")" == text/* ]]; then
        echo -e "\n\n=== FILE: $(basename "$file") ==="
        echo "-------------------------------------"
        cat "$file"
    fi
done

echo -e "\n\n### End of File Contents ###"