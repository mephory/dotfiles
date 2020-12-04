FZF_TMUX_OPTS="-p"

# Auto-completion
# ---------------
if [[ -f /usr/share/fzf/completion.zsh ]]; then
  source /usr/share/fzf/completion.zsh
fi

# Key bindings
# ------------
if [[ $- == *i* ]]; then

# CTRL-T - Paste the selected file path(s) into the command line
__fsel() {
  local cmd="${FZF_CTRL_T_COMMAND:-"command find -L . \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | sed 1d | cut -b3-"}"
  eval "$cmd" | $(__fzfcmd) -m | while read item; do
    printf '%q ' "$item"
  done
  echo
}

__fzfcmd() {
  [ ${FZF_TMUX:-1} -eq 1 ] && echo "fzf-tmux ${FZF_TMUX_OPTS:--d${FZF_TMUX_HEIGHT:-40%}}" || echo "fzf"
}

fzf-file-widget() {
  LBUFFER="${LBUFFER}$(__fsel)"
  zle redisplay
}
zle     -N   fzf-file-widget
# bindkey '^T' fzf-file-widget

bindkey -r '^]'

fzf-info-widget() {
echo "
a - audio input
b - git branch
f - file
t - tabs\
"
zle redisplay
}
zle -N fzf-info-widget

fzf-git-branch-widget() {
    B=$(git branch --format '%(refname:lstrip=2)' | $(__fzfcmd))
    LBUFFER="${LBUFFER}${B}"
    zle redisplay
}
zle -N fzf-git-branch-widget

fzf-audio-input-widget() {
    B=$(pacmd list-sources | awk -vRS='>' -vFS='<' '/name:/ { print $2 }' | $(__fzfcmd))
    LBUFFER="${LBUFFER}${B}"
    zle redisplay
}
zle -N fzf-audio-input-widget

fzf-audio-output-widget() {
    B=$(pacmd list-sinks | awk -vRS='>' -vFS='<' '/name:/ { print $2 }' | $(__fzfcmd))
    LBUFFER="${LBUFFER}${B}"
    zle redisplay
}
zle -N fzf-audio-output-widget

fzf-tab-widget() {
    B=$(lstab | urls | $(__fzfcmd))
    LBUFFER="${LBUFFER}${B}"
    zle redisplay
}
zle -N fzf-tab-widget

bindkey '^]^]' fzf-info-widget
bindkey '^]b' fzf-git-branch-widget
bindkey '^]a' fzf-audio-input-widget
bindkey '^]o' fzf-audio-output-widget
bindkey '^]f' fzf-file-widget
bindkey '^]t' fzf-tab-widget

# ALT-C - cd into the selected directory
# fzf-cd-widget() {
#   local cmd="${FZF_ALT_C_COMMAND:-"command find -L . \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
#     -o -type d -print 2> /dev/null | sed 1d | cut -b3-"}"
#   cd "${$(eval "$cmd" | $(__fzfcmd) +m):-.}"
#   zle reset-prompt
# }
# zle     -N    fzf-cd-widget
# bindkey '\ec' fzf-cd-widget

# CTRL-R - Paste the selected command from history into the command line
# fzf-history-widget() {
#   local selected num
#   selected=( $(fc -l 1 | $(__fzfcmd) +s --tac +m -n2..,.. --tiebreak=index --toggle-sort=ctrl-r -q "${LBUFFER//$/\\$}") )
#   if [ -n "$selected" ]; then
#     num=$selected[1]
#     if [ -n "$num" ]; then
#       zle vi-fetch-history -n $num
#     fi
#   fi
#   zle redisplay
# }
# zle     -N   fzf-history-widget
# bindkey '^R' fzf-history-widget

fi
