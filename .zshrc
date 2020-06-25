#a Caching

zstyle ':completion:*' use-cache true
zstyle ':completion:*' cache-path ~/.zshcache
zstyle ':completion:*' add-space true

# Completers loaded
zstyle ':completion:*' completer _list _expand _complete _ignored _match _correct _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort name
zstyle ':completion:*' glob 1

# zstyle ':completion:*' group-name ''
zstyle ':completion:*' ignore-parents parent pwd .. directory
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-grouped
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=** r:|=**' 'm:{[:lower:]}={[:upper:]} l:|=* r:|=*'
zstyle ':completion:*' match-original both
zstyle ':completion:*' max-errors 2 numeric
zstyle ':completion:*' menu yes select
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' prompt 'Errors found %e'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' substitute 1
zstyle ':completion:*' use-compctl true
zstyle ':completion:*' verbose true
zstyle ':completion:*' word true
zstyle :compinstall filename '$HOME/.zshrc'

alias kudt='sudo shutdown -P now'
autoload -Uz compinit
compinit


# Color ls output

# eval `dircolors ~/.dir_colors`

alias ls='ls --color=auto'



# End of lines added by compinstall
# Lines configured by zsh-newuser-install

# I like to use ZSH as calculator
zmodload zsh/mathfunc

HISTFILE=~/.histfile
HISTSIZE=6000
SAVEHIST=6000
setopt appendhistory autocd extendedglob nomatch notify hist_expire_dups_first extended_history share_history inc_append_history hist_find_no_dups auto_pushd pushd_silent pushd_ignore_dups bang_hist multios
unsetopt beep
bindkey -v

# End of lines configured by zsh-newuser-install


# Dynamic changing prompt
# and customized prompt
autoload -Uz myprompt
myprompt

function chpwd {
    dynamic_prompt
}


autoload -Uz buffermanip
buffermanip

# Load last used dirs
autoload -Uz cache_dirs
cache_dirs

restore_dirs

# Partitioned periodic functions
# Runs functions interleaved

autoload -U periodicity
periodicity

# Periodic functions
# this one runs 5 in 10
# and updates the prompt

function 5_in_10 {
    cache_dirs
    dynamic_prompt
}

function periodic {
    periodicity
}

PERIOD=1
# Load the not found command handler, it defaults to /etc/zsh_command_not_found
autoload -k command_handling
command_handling

# Load my aliases
autoload -U aliases
aliases

# Load keybindings tweaks

autoload -U keybindings
keybindings

# Compile often used scripts
autoload -U zrecompile

autoload -Uz colorify
autoload -Uz keys
keys
keys 

autoload -Uz git-extras
git-extras

# Recompile changed scripts as neccesary

zrecompile -p \
        -R ~/.zshrc -- \
        -R ~/scripts/functions/aliases -- \
        -R ~/scripts/functions/branching -- \
        -R ~/scripts/functions/command_handling -- \
        -R ~/scripts/functions/myprompt -- \
        -R ~/scripts/functions/periodicity -- \
        -R ~/scripts/functions/spinner -- \
        -R ~/scripts/functions/keys -- \
        -R ~/scripts/functions/git-extras -- \
        -R ~/scripts/functions/keybindings

function run_server {
        autoload -U chatserver
        chatserver
        rm /tmp/test-socket
        start_server "test"
}

function run_client {
        autoload -U chatserver
        chatserver
        start_client "test"
}
function sman {
        param=$1
        sections=(${(s:.:)param})
        man $sections[1] $sections[2]
}

# Load some stuff which enables concatenative programming

autoload -Uz functional
functional
# Tmux integration extensions
autoload -Uz tmux-extras
tmux-extras


autoload -Uz macro

# Preexec hooks
function preexec {
        macro $1 $2 $3
	tmux-preexec "$@"
}


autoload -Uz channel
channel

autoload -Uz keydb
keydb

# Antigen bundles

function find_finder(){
  local qq
  while true; do  
    fd --type file | fzf-tmux -- --preview "bat {}" | read qq; 
    if [[ "$?" == 0 ]]; then 
      nvim $qq 
    else 
      break
    fi 
  done 

}

zle -N find_finder

bindkey '^P' find_finder

function find_branch(){
  local qq 
  git branch | fzf-tmux | read qq; 
  git checkout $qq
  git pull
}

zle -N find_branch
bindkey '^B' find_branch 

export FZF_FINDER_BINDKEY="[^e"
source ~/sources/configuration/antigen/bin/antigen.zsh
antigen use oh-my-zsh
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zaw
antigen bundle rupa/z
antigen bundle changyuheng/fz
antigen bundle Czocher/gpg-crypt

antigen bundle laurenkt/zsh-vimto
antigen bundle command-not-found
antigen bundle Tarrasch/zsh-bd
antigen bundle extract
antigen bundle vagrant
antigen bundle debian
antigen bundle git
antigen bundle voronkovich/gitignore.plugin.zsh
antigen bundle pip
antigen bundle scala 
antigen bundle sbt 
antigen bundle perl 
antigen bundle nmap 
antigen bundle aws
antigen bundle zsh-users/zsh-completions
antigen theme candy


antigen apply


eval "$(gpg-agent --daemon)"

# OPAM configuration
. /home/eklerks/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

export NVM_DIR="/home/eklerks/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

alias dquilt="quilt --quiltrc=${HOME}/.quiltrc-dpkg"
alias ll='ls -lctra'
alias -s pdf=okular 

if [[ -z "$TOPIC" ]]; then 
        export TOPIC="main"
fi 


# Push most used directories
#


. /home/eklerks/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

 export EDITOR="nvim"
 export GOROOT=/usr/local/go 
 export GOPATH=/home/eklerks/go

PATH="/home/eklerks/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/eklerks/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/eklerks/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/eklerks/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/eklerks/perl5"; export PERL_MM_OPT;

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/eklerks/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/eklerks/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/eklerks/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/eklerks/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

