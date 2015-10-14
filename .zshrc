# setopts
setopt prompt_subst
setopt auto_pushd
setopt pushd_ignore_dups
setopt extended_glob


# settings and environment
export HISTFILE=~/.history
export HISTSIZE=1000000
export SAVEHIST=1000000

export PATH="$PATH:$HOME/.gem/ruby/2.2.0/bin"
export PATH="$PATH:$HOME/.cabal/bin"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/bin/git-plugins"
export PATH="$PATH:$HOME/bin/tmux-plugins"

for f in ~/.zsh-env/*(@,.N); do
    source $f;
done


# hash dirs
hash -d acc="$HOME/data/account"
hash -d vid="$HOME/data/video"
hash -d img="$HOME/data/images"
hash -d cam="$HOME/data/images/webcam"


# autocompletion settings
autoload -Uz compinit
compinit
zstyle ':completion:*' completer _complete _ignored
zstyle ':completion:*' expand prefix
zstyle :compinstall filename '/home/mephory/.zshrc'
fpath=(~/.zsh-completions $fpath)


# prompt
function zle-line-init zle-keymap-select {
    if [[ $KEYMAP = "vicmd" ]]; then
        PCOLOR="%{$fg[yellow]%}"
    else
        PCOLOR="%{$reset_color%}"
    fi;
    PROMPT="%{$fg[yellow]%}%n %{$fg[blue]%}%~ %{$fg[magenta]%}$(gitprompt)%(1j.%{$fg[green]|%j| .)$PCOLOR$%{$reset_color%} "
    zle reset-prompt
}

autoload -Uz colors
colors
zle -N zle-line-init
zle -N zle-keymap-select
export KEYTIMEOUT=1


# key bindings
bindkey -v
bindkey '^[[H'  beginning-of-line 
bindkey '\e[1~' beginning-of-line
bindkey '^a'  beginning-of-line 
bindkey '^e' end-of-line
bindkey '^P' up-history
bindkey '^B' backward-char
bindkey '^H' backward-delete-char
bindkey '^W' backward-kill-word
bindkey '^U' kill-whole-line
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
bindkey '^o' insert-last-word
bindkey -M viins '^x' vi-cmd-mode
bindkey -M vicmd '^x' vi-insert
bindkey -M vicmd '^e' edit-command-line

autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey '^k' copy-earlier-word

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line


# quote stuff that looks like URLs automatically
autoload -U url-quote-magic
zstyle ':urlglobber' url-other-schema ftp git gopher http https magnet
zstyle ':url-quote-magic:*' url-metas '*?[]^(|)~#='  # dropped { }
zle -N self-insert url-quote-magic


# colorize auto-completion like ls
export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}


# Complete from tmux pane.
_tmux_pane_words() {
    setopt glob_complete
    local expl
    local -a w
    if [[ -z "$TMUX_PANE" ]]; then
    _message "not running inside tmux!"
    return 1
    fi
    w=( ${(u)=$(tmux capture-pane \; show-buffer \; delete-buffer)} )
    _wanted values expl 'words from current tmux pane' compadd -a w
}

zle -C tmux-pane-words-prefix   complete-word _generic
zle -C tmux-pane-words-anywhere complete-word _generic
bindkey '^ ' tmux-pane-words-anywhere
bindkey '^t' tmux-pane-words-prefix

zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' completer _tmux_pane_words
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' ignore-line current
zstyle ':completion:tmux-pane-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'

source ~/.alias


# print the todo when opening a shell once in a while
if [[ ! -f /tmp/todoread ]]; then
    todo
    touch /tmp/todoread
    ((sleep 3600 && rm /tmp/todoread) &)
fi
