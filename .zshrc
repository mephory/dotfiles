# setopts
setopt prompt_subst
setopt auto_pushd
setopt pushd_ignore_dups
setopt extended_glob
setopt histignorespace
setopt incappendhistory


# settings and environment
export HISTFILE=~/.history
export HISTSIZE=1000000
export SAVEHIST=1000000

export TERM="xterm-256color"

export PATH="$PATH:$HOME/.gem/ruby/2.2.0/bin"
export PATH="$PATH:$HOME/.gem/ruby/2.3.0/bin"
export PATH="$PATH:$HOME/.gem/ruby/2.4.0/bin"
export PATH="$PATH:$HOME/.gem/ruby/2.5.0/bin"
export PATH="$PATH:$HOME/.gem/ruby/2.6.0/bin"
export PATH="$PATH:$HOME/.gem/ruby/2.7.0/bin"
export PATH="$PATH:$HOME/.cabal/bin"
export PATH="$PATH:$HOME/.npm-global/bin"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/bin/git-plugins"
export PATH="$PATH:$HOME/bin/tmux-plugins"
export PATH="$PATH:$HOME/bin/polybar"
export PATH="$PATH:$HOME/bin/wisp"
export PATH="$PATH:$HOME/bin/rofi"

export GOPATH="$HOME/.go"
export PATH="$PATH:$GOPATH/bin"

export FZF_COMPLETION_TRIGGER="~~"

for f in ~/.zsh-env/*(@,.N); do
    source $f;
done

# hash dirs
hash -d acc="$HOME/data/account"
hash -d vid="$HOME/data/video"
hash -d img="$HOME/data/images"
hash -d cam="$HOME/data/images/webcam"
hash -d ext="/mnt/external"
hash -d eimg="/mnt/external/image"
hash -d evid="/mnt/external/video"


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
    PROMPT="%(#.%{$fg[red]%}.%{$fg[yellow]%})%n %{$fg[blue]%}%~ %{$fg[magenta]%}$(gitprompt)%(1j.%{$fg[green]%}[%j] .)$PCOLOR$%{$reset_color%} "
    zle reset-prompt
}

# kill word but have / as word separator
function backward-kill-dirword {
    local WORDCHARS=${WORDCHARS/\/}
    zle backward-kill-word
}
zle -N backward-kill-dirword

function insert-newest-file-glob {
    LBUFFER="${LBUFFER}*(om[1])"
    zle expand-word
}
zle -N insert-newest-file-glob

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
bindkey '^N' insert-newest-file-glob
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
bindkey '^y' yank
bindkey '^o' insert-last-word
bindkey '^f' forward-word
bindkey '^b' backward-word
bindkey '^\' backward-kill-dirword
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
export LS_COLORS='no=00:fi=00:di=34:ow=34;40:ln=35:pi=30;44:so=35;44:do=35;44:bd=33;44:cd=37;44:or=05;37;41:mi=05;37;41:ex=01;31:*.cmd=01;31:*.exe=01;31:*.com=01;31:*.bat=01;31:*.reg=01;31:*.app=01;31:*.txt=32:*.org=32:*.md=32:*.mkd=32:*.h=32:*.c=32:*.C=32:*.cc=32:*.cpp=32:*.cxx=32:*.objc=32:*.cl=32:*.sh=32:*.bash=32:*.csh=32:*.zsh=32:*.el=32:*.vim=32:*.java=32:*.pl=32:*.pm=32:*.py=32:*.rb=32:*.hs=32:*.php=32:*.htm=32:*.html=32:*.shtml=32:*.erb=32:*.haml=32:*.xml=32:*.rdf=32:*.css=32:*.sass=32:*.scss=32:*.less=32:*.js=32:*.coffee=32:*.man=32:*.0=32:*.1=32:*.2=32:*.3=32:*.4=32:*.5=32:*.6=32:*.7=32:*.8=32:*.9=32:*.l=32:*.n=32:*.p=32:*.pod=32:*.tex=32:*.go=32:*.sql=32:*.csv=32:*.bmp=33:*.cgm=33:*.dl=33:*.dvi=33:*.emf=33:*.eps=33:*.gif=33:*.jpeg=33:*.jpg=33:*.JPG=33:*.mng=33:*.pbm=33:*.pcx=33:*.pdf=33:*.pgm=33:*.png=33:*.PNG=33:*.ppm=33:*.pps=33:*.ppsx=33:*.ps=33:*.svg=33:*.svgz=33:*.tga=33:*.tif=33:*.tiff=33:*.xbm=33:*.xcf=33:*.xpm=33:*.xwd=33:*.xwd=33:*.yuv=33:*.aac=33:*.au=33:*.flac=33:*.m4a=33:*.mid=33:*.midi=33:*.mka=33:*.mp3=33:*.mpa=33:*.mpeg=33:*.mpg=33:*.ogg=33:*.opus=33:*.ra=33:*.wav=33:*.anx=33:*.asf=33:*.avi=33:*.axv=33:*.flc=33:*.fli=33:*.flv=33:*.gl=33:*.m2v=33:*.m4v=33:*.mkv=33:*.mov=33:*.MOV=33:*.mp4=33:*.mp4v=33:*.mpeg=33:*.mpg=33:*.nuv=33:*.ogm=33:*.ogv=33:*.ogx=33:*.qt=33:*.rm=33:*.rmvb=33:*.swf=33:*.vob=33:*.webm=33:*.wmv=33:*.doc=31:*.docx=31:*.rtf=31:*.odt=31:*.dot=31:*.dotx=31:*.ott=31:*.xls=31:*.xlsx=31:*.ods=31:*.ots=31:*.ppt=31:*.pptx=31:*.odp=31:*.otp=31:*.fla=31:*.psd=31:*.7z=1;35:*.apk=1;35:*.arj=1;35:*.bin=1;35:*.bz=1;35:*.bz2=1;35:*.cab=1;35:*.deb=1;35:*.dmg=1;35:*.gem=1;35:*.gz=1;35:*.iso=1;35:*.jar=1;35:*.msi=1;35:*.rar=1;35:*.rpm=1;35:*.tar=1;35:*.tbz=1;35:*.tbz2=1;35:*.tgz=1;35:*.tx=1;35:*.war=1;35:*.xpi=1;35:*.xz=1;35:*.z=1;35:*.Z=1;35:*.zip=1;35:*.ANSI-30-black=30:*.ANSI-01;30-brblack=01;30:*.ANSI-31-red=31:*.ANSI-01;31-brred=01;31:*.ANSI-32-green=32:*.ANSI-01;32-brgreen=01;32:*.ANSI-33-yellow=33:*.ANSI-01;33-bryellow=01;33:*.ANSI-34-blue=34:*.ANSI-01;34-brblue=01;34:*.ANSI-35-magenta=35:*.ANSI-01;35-brmagenta=01;35:*.ANSI-36-cyan=36:*.ANSI-01;36-brcyan=01;36:*.ANSI-37-white=37:*.ANSI-01;37-brwhite=01;37:*.log=01;32:*~=01;32:*#=01;32:*.bak=01;33:*.BAK=01;33:*.old=01;33:*.OLD=01;33:*.org_archive=01;33:*.off=01;33:*.OFF=01;33:*.dist=01;33:*.DIST=01;33:*.orig=01;33:*.ORIG=01;33:*.swp=01;33:*.swo=01;33:*,v=01;33:*.gpg=34:*.gpg=34:*.pgp=34:*.asc=34:*.3des=34:*.aes=34:*.enc=34:*.sqlite=34:';
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
# bindkey '^t' tmux-pane-words-prefix


# Move cursor after the first word
move-after-first-word() {
    zle beginning-of-line
    zle forward-word
}

zle -N move-after-first-word
bindkey '^g' move-after-first-word

open-manpage() {
    tokens=(${(z)LBUFFER[(ws:|:)-1]})
    cmd=${tokens[1]}
    man ${cmd}
    zle redisplay
}
zle -N open-manpage
# bindkey '^T' open-manpage


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

# eval "$(rbenv init -)"

# export PYENV_ROOT="$HOME/.pyenv"
# export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init -)"
