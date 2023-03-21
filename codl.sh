#!/bin/bash

declare -A nodes childNodes nodeTypes identifier values
declare -a focus
focus=("")
shopt -s extglob

refocus() {
  local -i level difference depth
  local -a nodeArgs
  local node nodeType address parent
  level=$1
  nodeId="$2"
  file="$3"
  nodeType="$4"
  nodeArgs=("${@:4}")
  depth=${#focus[@]}
  difference=$((depth-level))
  
  while [ $difference -gt 2 ]
  do
    unset 'focus[-1]'
    depth=${#focus[@]}
    difference=$((depth-level))
  done
  
  if [ $difference = 2 ]
  then
    unset 'focus[-1]'
    difference=$((difference-1))
  fi
  if [ $difference = 1 ]
  then
    focus+=("$nodeId")
  fi
  
  printf -v address ".%s" "${focus[@]}"
  parent="${address%.*}"
  address="${address:2}"
  parent="${parent:2}"
  nodeTypes["$file:$address"]="${nodeType}"
  values["$file:$address"]+=" ${nodeArgs[@]:1}"
  childNodes["$file:$parent"]+=" $file:$address"
}

parseLine() {
  local -i level abort
  local -a atoms
  local line IFS id file
  file="$1"
  line="$2"
  level=0
  abort=0

  while [ $abort = 0 ]
  do
    case "$line" in
      *( )#*)
        abort=1
        ;;
      '  '*)
        level=$((level+1))
        line="${line:2}"
        ;;
      *)
        read -ra atoms <<< "$line"
        id="${atoms[${identifier[${atoms[0]}]}]}"
        nonEmpty "$id" "Unrecognized atom: '${atoms[0]}'"
        refocus $level "$id" "$file" "${atoms[0]}" "${atoms[@]:1}"
        abort=1
        ;;
    esac
  done
}

parseCodl() {
  local file
  file="$1"
  while IFS=$'\n' read -r line
  do parseLine "$file" "$line"
  done < "$file"
}

children() {
  local filter node
  local -a nodes
  node="$1"
  filter="$2"
  read -ra nodes <<< "${childNodes[$node]}"
  for node in "${nodes[@]}"
  do
    if [ "$filter" = "" ]
    then printf "%s\n" "$node"
    elif [ "$filter" = "${nodeTypes[$node]}" ]
    then printf "%s\n" "$node"
    fi
  done
}

nonEmpty() {
  if [ "$1" = "" ]
  then
    printf "%s\n" "$2"
    exit 1
  fi
}

values() {
  local address
  local -a data
  address="$1"
  nonEmpty "$address" "Cannot access values for empty node"
  read -ra data <<< "${values[$address]}"
  for datum in "${data[@]}"
  do printf "%s\n" "$datum"
  done
}
