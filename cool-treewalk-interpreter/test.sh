#!/bin/bash

# @author   Trey Rubino
# @date     07/01/2025

LEXER="lexer/build/lexer"
PARSER="parser/build/parser"
CHECKER="checker/build/checker"
INTERP="interpreter/build/interp"

COOL_FILE="tests/test.cl"
ERR_FILE="tests/error.log"

CLEANUP_FILES=(
    "${COOL_FILE}-lex"
    "${COOL_FILE}-ast"
    "${COOL_FILE}-type"
)

build_stage() 
{
    local stage_dir=$1
    echo -n "" > "$ERR_FILE"

    if ! (cd "$stage_dir" && make clean > /dev/null 2>&1 && make > /dev/null 2>&1); then
        echo "ERROR: build failed in $stage_dir. Check $ERR_FILE"
        exit 1
    fi
}

run_stage() 
{
    local binary=$1
    local input=$2
    local output=$3
    local name=$(basename "$binary")
    local Name="$(tr '[:lower:]' '[:upper:]' <<< "${name:0:1}")${name:1}"

    if ! "./$binary" "$input" "$output" >> "$ERR_FILE" 2>&1; then
        echo "ERROR: $Name: Check $ERR_FILE"
        exit 1
    fi

    if [[ ! -f "$output" ]]; then
        echo "ERROR: $Name: Output '$output' not found"
        exit 1
    fi
}

usage()
{
    echo "Usage: $0 -l|--lex     # Lexer only"
    echo "       $0 -p|--parse   # Lexer + Parser"
    echo "       $0 -c|--check   # Lexer + Parser + Checker"
    echo "       $0 -i|--interp  # Full pipeline"
    exit 1 
}

clean() 
{
    cd "$(dirname "$0")" || exit 1
    : > "$ERR_FILE"
    for f in "${CLEANUP_FILES[@]}"; do
        [[ -f "$f" ]] && rm -f "$f"
    done
}

clean

if [[ $# -ne 1 ]]; then
    usage
    exit 1
fi

case "$1" in
    -l|--lex)
        build_stage "lexer"
        run_stage "$LEXER" "$COOL_FILE" "${COOL_FILE}-lex"
        ;;

    -p|--parse)
        build_stage "lexer"
        run_stage "$LEXER" "$COOL_FILE" "${COOL_FILE}-lex"
        build_stage "parser"
        run_stage "$PARSER" "${COOL_FILE}-lex" "${COOL_FILE}-ast"
        ;;

    -c|--check)
        build_stage "lexer"
        run_stage "$LEXER" "$COOL_FILE" "${COOL_FILE}-lex"
        build_stage "parser"
        run_stage "$PARSER" "${COOL_FILE}-lex" "${COOL_FILE}-ast"
        build_stage "checker"
        run_stage "$CHECKER" "${COOL_FILE}-ast" "${COOL_FILE}-type"
        ;;

    -i|--interp)
        build_stage "lexer"
        run_stage "$LEXER" "$COOL_FILE" "${COOL_FILE}-lex"
        build_stage "parser"
        run_stage "$PARSER" "${COOL_FILE}-lex" "${COOL_FILE}-ast"
        build_stage "checker"
        run_stage "$CHECKER" "${COOL_FILE}-ast" "${COOL_FILE}-type"
        build_stage "interpreter"
        run_stage "$INTERP" "${COOL_FILE}-type" "tests/error.log"
        ;;

    *)
        usage
        ;;
esac
