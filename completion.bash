# bash completion for fpm-watch

_fpm_watch() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  local i cmd_index=-1

  i=1
  while (( i < ${#COMP_WORDS[@]} )); do
    case "${COMP_WORDS[i]}" in
      --watch-poll|--watch-debounce|--watch-rescan|--watch-ignore|--watch-include|--watch-feature|--watch-restart-delay|--watch-restart-max|--watch-self)
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

  local cmds="build test run"
  local opts="--help -h
--watch-help
--watch-quiet -q
--watch-verbose -v
--watch-very-verbose -vv
--watch-debug
--watch-deps
--watch-no-deps
--watch-low-cpu
--watch-no-low-cpu
--watch-poll
--watch-debounce
--watch-rescan
--watch-no-rescan
--watch-run-on-start
--watch-no-run-on-start
--watch-silent-fpm
--watch-print-files
--watch-ignore
--watch-include
--watch-feature
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