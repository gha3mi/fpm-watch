# bash completion for fpm-watch

_fpm_watch() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  local i cmd_index=-1
  local prefix="" values
  COMPREPLY=()

  i=1
  while (( i < ${#COMP_WORDS[@]} )); do
    case "${COMP_WORDS[i]}" in
      --watch-poll|--watch-debounce|--watch-rescan|--watch-ignore|--watch-include|--watch-restart-delay|--watch-restart-max|--watch-self|--watch-log-format|--watch-fpm)
        ((i+=2)); continue ;;
      --watch-verbose)
        if (( i+1 < ${#COMP_WORDS[@]} )) && [[ "${COMP_WORDS[i+1]}" =~ ^[0-2]$ ]]; then
          ((i+=2))
        else
          ((i+=1))
        fi
        continue ;;
      --watch-verbose=*|--watch-*=*)
        ((i+=1)); continue ;;
    esac

    case "${COMP_WORDS[i]}" in
      build|test|run) cmd_index=$i; break ;;
    esac
    ((i+=1))
  done

  if (( cmd_index != -1 && COMP_CWORD > cmd_index )); then
    [[ "$cur" == -* ]] && COMPREPLY=( $(compgen -W '--' -- "$cur") )
    return 0
  fi

  if [[ "$cur" == --watch-log-format=* ]]; then
    prefix="--watch-log-format="
    cur="${cur#*=}"
  fi
  if [[ -n "$prefix" || ( "$COMP_CWORD" -gt 0 && "${COMP_WORDS[COMP_CWORD-1]}" == --watch-log-format ) ]]; then
    for values in $(compgen -W 'text json logfmt' -- "$cur"); do
      COMPREPLY+=("$prefix$values")
    done
    return 0
  fi

  local cmds="build test run"
  local opts="--help -h
--watch-help
--watch-quiet -q
--watch-log-format
--watch-verbose -v
--watch-very-verbose -vv
--watch-debug
--watch-deps
--watch-no-deps
--watch-poll
--watch-debounce
--watch-rescan
--watch-no-rescan
--watch-run-on-start
--watch-no-run-on-start
--watch-silent-fpm
--watch-fpm
--watch-output-markers
--watch-print-files
--watch-ignore
--watch-include
--watch-auto-restart
--watch-restart-delay
--watch-restart-max
--watch-self
--watch-once"

  if [[ -z "$cur" ]]; then
    COMPREPLY=( $(compgen -W "$cmds $opts" -- "") )
  elif [[ "$cur" == -* ]]; then
    COMPREPLY=( $(compgen -W "$opts" -- "$cur") )
  else
    COMPREPLY=( $(compgen -W "$cmds" -- "$cur") )
  fi
}

complete -o default -F _fpm_watch fpm-watch
