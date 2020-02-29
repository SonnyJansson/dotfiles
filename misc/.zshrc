export TERM="xterm-256color"
export PATH=~/.cabal/bin:~/.local/bin:$PATH

#Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/zyphicx/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"

PROMPT='%F{196}[%F{106}%n@%F{033}%m %~%F{196}]%F{255}%(!.#.$) %F{reset_color}'

#source ~/.zsh-themes/powerlevel9k/powerlevel9k.zsh-theme
#POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir rbenv vcs)
#POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(root_indicator background_jobs history time)
#POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs history time)

#export WORKON_HOME=$HOME/.virtualenvs
#source /usr/local/bin/virtualenvwrapper.sh

alias less='less -Q'
alias man='man -P "less -Q"'
