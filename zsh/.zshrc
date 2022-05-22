export PATH="/opt/homebrew/opt/openssl@1.1/bin:$PATH"
export PATH="/opt/homebrew/opt/luajit-openresty/bin:$PATH"
fpath=($fpath "/Users/eugene/.zfunctions")

source "$(brew --prefix)/etc/profile.d/z.sh"


# cd ~/dotfiles && sudo chmod +x install && ./install
# brew bundle dump
# brew bundle install


#
# Command aliases
#

alias :sp='test -n "$TMUX" && tmux split-window'
alias :vs='test -n "$TMUX" && tmux split-window -h'
alias cd..='cd ..'
alias claer='clear'
alias cler='clear'
alias cl='clear'
alias e=exit
alias topten='history | commands | sort -rn | head -n 100'
alias contents='ag --nobreak --nonumbers --noheading . | fzf'
alias zshrc='nvim ~/.zshrc' # Quick access to the ~/.zshrc file
alias tree='exa --tree --long'
alias ls='exa --long --git'

#
# Git Aliases
#

alias ga='git add'
alias gc='git commit'
alias gca='git commit -a'
alias gcam='git commit -a -m'
alias gcm='git commit -m'
alias glog="git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gs='git status -sb'
alias gl='git pull --ff-only'
alias gp='git push --all'

#
# System management Aliases
#
alias findd="find . -type d -iname" # find a directory
alias findf="find . -type f -iname" # find a file
alias ip="curl icanhazip.com"       # get current public IP
alias more='more -R'                # give more colors
alias process="ps aux | grep -i"
alias scanlocal='nmap -sP 192.168.1.0/24'
alias dfd='df -h -x squashfs -x tmpfs -x devtmpfs'
alias editenv='$EDITOR ~/.env.zsh && source ~/.env.zsh && echo "Reloaded environment variables"'
alias editbrew='$EDITOR ~/github/lmullen/dotfiles/homebrew/Schaff.Brewfile'
alias reload="source $HOME/.zshrc"

unalias z 2> /dev/null

#
# Options
#

setopt hist_ignore_all_dups # remove older duplicate entries from history
setopt hist_reduce_blanks # remove superfluous blanks from history items
setopt inc_append_history # save history entries as soon as they are entered
setopt share_history # share history between different instances of the shell
setopt correct_all # autocorrect commands
setopt auto_list # automatically list choices on ambiguous completion
setopt always_to_end # move cursor to end if word had one match

zstyle ':completion:*' group-name '' # group results by category

setopt  autocd autopushd \ pushdignoredups

#
# Functions
#


# Tab title
# sets the tab title to current dir
precmd() {
  echo -ne "\e]1;${PWD##*/}\a"
}

function shell () {
  ps | grep `echo $$` | awk '{ print $4 }'
}

function z() {
  [ $# -gt 0 ] && _z "$*" && return
  cd "$(_z -l 2>&1 | fzf --height 40% --nth 2.. --reverse --inline-info +s --tac --query "${*##-* }" | sed 's/^[0-9,.]* *//')"
}

# fuzzy grep open via ag with line number
vg() {
  local file
  local line

  read -r file line <<<"$(ag --nobreak --noheading $@ | fzf -0 -1 | awk -F: '{print $1, $2}')"

  if [[ -n $file ]]
  then
     vim $file +$line
  fi
}

# fd - "find directory"
# This one differs from the above, by only showing the sub directories and not
#  showing the directories within those.
function fdir() {
  DIR=`find * -maxdepth 0 -type d -print 2> /dev/null | fzf-tmux` \
    && cd "$DIR"
}

# fda - including hidden directories
function fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# fh - repeat history
function fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}


# fco_preview - checkout git branch/tag, with a preview showing the commits between the tag/branch and HEAD
function preview() {
  local tags branches target
  tags=$(
git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
  branches=$(
git branch --all | grep -v HEAD |
sed "s/.* //" | sed "s#remotes/[^/]*/##" |
sort -u | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
  target=$(
(echo "$tags"; echo "$branches") |
    fzf --no-hscroll --no-multi --delimiter="\t" -n 2 \
        --ansi --preview="git log -200 --pretty=format:%s $(echo {+2..} |  sed 's/$/../' )" ) || return
  git checkout $(echo "$target" | awk '{print $2}')
}

# fshow - git commit browser
function fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

# Install (one or multiple) selected application(s)
# using "brew search" as source input
# mnemonic [B]rew [I]nstall [P]lugin
function bip() {
  local inst=$(brew search | fzf -m)

  if [[ $inst ]]; then
    for prog in $(echo $inst);
    do; brew install $prog; done;
  fi
}

# Update (one or multiple) selected application(s)
# mnemonic [B]rew [U]pdate [P]lugin
function bup() {
  local upd=$(brew leaves | fzf -m)

  if [[ $upd ]]; then
    for prog in $(echo $upd);
    do; brew upgrade $prog; done;
  fi
}

# Delete (one or multiple) selected application(s)
# mnemonic [B]rew [C]lean [P]lugin (e.g. uninstall)
function bdp() {
  local uninst=$(brew leaves | fzf -m)

  if [[ $uninst ]]; then
    for prog in $(echo $uninst);
    do; brew uninstall $prog; done;
  fi
}


#
# History
#

export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE

#
# hooks
#

autoload -U add-zsh-hook

typeset -F SECONDS

function record-start-time() {
  emulate -L zsh
  ZSH_START_TIME=${ZSH_START_TIME:-$SECONDS}
}

add-zsh-hook preexec record-start-time

function report-start-time() {
  emulate -L zsh
  if [ $ZSH_START_TIME ]; then
    local DELTA=$(($SECONDS - $ZSH_START_TIME))
    local DAYS=$((~~($DELTA / 86400)))
    local HOURS=$((~~(($DELTA - $DAYS * 86400) / 3600)))
    local MINUTES=$((~~(($DELTA - $DAYS * 86400 - $HOURS * 3600) / 60)))
    local SECS=$(($DELTA - $DAYS * 86400 - $HOURS * 3600 - $MINUTES * 60))
    local ELAPSED=''
    test "$DAYS" != '0' && ELAPSED="${DAYS}d"
    test "$HOURS" != '0' && ELAPSED="${ELAPSED}${HOURS}h"
    test "$MINUTES" != '0' && ELAPSED="${ELAPSED}${MINUTES}m"
    if [ "$ELAPSED" = '' ]; then
      SECS="$(print -f "%.2f" $SECS)s"
    elif [ "$DAYS" != '0' ]; then
      SECS=''
    else
      SECS="$((~~$SECS))s"
    fi
    ELAPSED="${ELAPSED}${SECS}"
    local ITALIC_ON=$'\e[3m'
    local ITALIC_OFF=$'\e[23m'
    export RPROMPT="%F{cyan}%{$ITALIC_ON%}${ELAPSED}%{$ITALIC_OFF%}%f $RPROMPT_BASE"
    unset ZSH_START_TIME
  else
    export RPROMPT="$RPROMPT_BASE"
  fi
}

add-zsh-hook precmd report-start-time

function auto-ls-after-cd() {
  emulate -L zsh
  # Only in response to a user-initiated `cd`, not indirectly (eg. via another
  # function).
  if [ "$ZSH_EVAL_CONTEXT" = "toplevel:shfunc" ]; then
    ls -a
  fi
}

add-zsh-hook chpwd auto-ls-after-cd

PROMPT='%(?.%F{green}λ.%F{red}?%?)%f %B%F{240}%1~%f%b %# '

export PNPM_HOME="/Users/eugene/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# function commands() {
#   awk '{a[$2]++}END{for(i in a){print a[i] " " i}}'
# }

#function cdir() {
#    if [[ "$#" != 0 ]]; then
#        builtin cd "$@";
#        return
#    fi
#    while true; do
#        local lsd=$(echo ".." && ls -p | grep '/$' | sed 's;/$;;')
#        local dir="$(printf '%s\n' "${lsd[@]}" |
#            fzf --reverse --preview '
#                __cd_nxt="$(echo {})";
#                __cd_path="$(echo $(pwd)/${__cd_nxt} | sed "s;//;/;")";
#                echo $__cd_path;
#                echo;
#                ls -p -FG "${__cd_path}";
#        ')"
#        [[ ${#dir} != 0 ]] || return 0
#        builtin cd "$dir" &> /dev/null
#    done
#}

# # Bounce the Dock icon, if iTerm does not have focus.
# function bounce() {
#   if [ -n "$TMUX" ]; then
#     print -Pn "\ePtmux;\e\e]1337;RequestAttention=1\a\e\\"
#   else
#     print -Pn "\e]1337;RequestAttention=1\a"
#   fi
# }

# function set-tab-and-window-title() {
#   emulate -L zsh
#   local CMD="${1:gs/$/\\$}"
#   print -Pn "\e]0;$CMD:q\a"
# }

# function update-window-title-precmd() {
#   emulate -L zsh
#   set-tab-and-window-title `history | tail -1 | cut -b8-`
# }
# add-zsh-hook precmd update-window-title-precmd

# function update-window-title-preexec() {
#   emulate -L zsh
#   setopt extended_glob

#   # skip ENV=settings, sudo, ssh; show first distinctive word of command;
#   # mostly stolen from:
#   #   https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/termsupport.zsh
#   set-tab-and-window-title ${2[(wr)^(*=*|mosh|ssh|sudo)]}
# }

# add-zsh-hook preexec update-window-title-preexec

# add-zsh-hook precmd bounce
