# case insensitive completion
# set completion-ignore-case on
bind 'set completion-ignore-case on'

# search through your history using the up and down arrows
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# words which have more than one possible completion cause the matches to be listed immediately
bind 'set show-all-if-ambiguous on'

# stop appending repeated identical commands to bash_history
# export HISTCONTROL=ignoredups
export HISTCONTROL=ignoreboth:erasedups

# use xterm-256color
export TERM=xterm-256color

# export no proxy for adp
export no_proxy=adp.com,aws.adp

# path to z shell
. ~/z/z.sh

# tmux session on terminal start
# connects to an existing tmux session or create a new one
case $- in
    *i*)
    if command -v tmux>/dev/null; then
        if [[ ! $TERM =~ screen ]] && [[ -z $TMUX ]]; then
          if tmux ls 2> /dev/null | grep -q -v attached; then
            exec tmux attach -t $(tmux ls 2> /dev/null | grep -v attached | head -1 | cut -d : -f 1)
          else
            exec tmux
          fi
        fi
    fi
    ;;
esac

# path to emscripten toolchain
# . ~/sk/projects/emsdk/emsdk_env.sh --build=Release

# starts emacs server when using emacsclient
# export ALTERNATE_EDITOR=""

# load nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# show dot files in finder
alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES;
killall Finder /System/Library/CoreServices/Finder.app'

# hide dot files in finder
alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'

# fancy prompt with git
function git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* (*\([^)]*\))*/\1/'
}

function markup_git_branch {
  if [[ -n $@ ]]; then
    if [[ -z $(git status --porcelain 2> /dev/null | tail -n1) ]]; then
      echo -e "\001\033[32m\002($@)\001\033[0m\002"
    else
      echo -e "\001\033[31m\002($@)\001\033[0m\002"
    fi
  fi
}

PS1="\n┌─[\`if [ \$? = 0 ]; then echo \[\e[32m\]✔\[\e[0m\]; else echo \[\e[31m\]✘\[\e[0m\]; fi\`]───[\[\e[01;49;39m\]\w\[\e[00m\]]───[\[\e[1;49;34m\]\W\[\e[0m\]]───\$(markup_git_branch \$(git_branch))───[\[\e[1;49;39m\]\$(ls -a | wc -l | tr -d ' ') F ◉  \$(ls -lah | grep -m 1 total | sed 's/total //') B\[\e[0m\]]\n└───▶ "
# PS1="\n┌─[\`if [ \$? = 0 ]; then echo \[\e[32m\]✔\[\e[0m\]; else echo \[\e[31m\]✘\[\e[0m\]; fi\`]───[\[\e[01;49;39m\]\w\[\e[00m\]]───[\[\e[1;49;34m\]\W\[\e[0m\]]───\$(markup_git_branch \$(git_branch))───[\[\e[1;49;39m\]\$(ls | wc -l) items, \$(ls -lah | grep -m 1 total | sed 's/total //')\[\e[0m\]]\n└───▶ "
# ⸦⸧

# use visible bell
set bell-style visible

# automatically cd into the directories if the command is a path
shopt -s autocd

# most commonly used aliases
# export EDITOR='open -a /Applications/Emacs.app'
export EDITOR='subl'

alias em='open -a /Applications/Emacs.app'

alias emdebug='/Applications/Emacs.app/Contents/MacOS/Emacs --debug-init'
alias emacsd='/Users/kotamrs/.emacs.d'
alias emacsinstall='brew install emacs --with-cocoa --with-gnutls --with-librsvg --with-modules'

alias npmrc='$EDITOR ~/.npmrc'
alias tmuxconf='$EDITOR ~/.tmux.conf'
alias bashprofile='$EDITOR ~/.bash_profile'
alias bashrc='$EDITOR ~/.bashrc'

# export SHELL_SESSION_HISTORY=0
alias bashhistory='$EDITOR ~/.bash_history'
alias zprofile='$EDITOR ~/.zprofile'
alias gitconfig='$EDITOR ~/.gitconfig'
alias gitignore='$EDITOR ~/.gitignore'

alias npg='npm list --depth=0 -g'
alias npmu='npm outdated -g --depth=0'
alias npmup='npm update -g'
alias brg='brew list --versions'
alias brewup='brew update && brew upgrade && brew cleanup; brew doctor'


# use tab to complete from the list
bind TAB:menu-complete

# Git Aliases copied from prezto
# https://github.com/sorin-ionescu/prezto/blob/master/modules/git/alias.zsh

# Git
alias g='git'

# Branch (b)
alias gb='git branch'
alias gba='git branch --all --verbose'
alias gbc='git checkout -b'
alias gbd='git branch --delete'
alias gbD='git branch --delete --force'
alias gbl='git branch --verbose'
alias gbL='git branch --all --verbose'
alias gbm='git branch --move'
alias gbM='git branch --move --force'
alias gbr='git branch --move'
alias gbR='git branch --move --force'
alias gbs='git show-branch'
alias gbS='git show-branch --all'
alias gbv='git branch --verbose'
alias gbV='git branch --verbose --verbose'
alias gbx='git branch --delete'
alias gbX='git branch --delete --force'

# Commit (c)
alias gc='git commit --verbose'
alias gca='git commit --verbose --all'
alias gcm='git commit --message'
alias gcS='git commit -S --verbose'
alias gcSa='git commit -S --verbose --all'
alias gcSm='git commit -S --message'
alias gcam='git commit --all --message'
alias gco='git checkout'
alias gcO='git checkout --patch'
alias gcf='git commit --amend --reuse-message HEAD'
alias gcSf='git commit -S --amend --reuse-message HEAD'
alias gcF='git commit --verbose --amend'
alias gcSF='git commit -S --verbose --amend'
alias gcp='git cherry-pick --ff'
alias gcP='git cherry-pick --no-commit'
alias gcr='git revert'
alias gcR='git reset "HEAD^"'
alias gcs='git show'
alias gcl='git-commit-lost'

# Conflict (C)
alias gCl='git --no-pager diff --name-only --diff-filter=U'
alias gCa='git add $(gCl)'
alias gCe='git mergetool $(gCl)'
alias gCo='git checkout --ours --'
alias gCO='gCo $(gCl)'
alias gCt='git checkout --theirs --'
alias gCT='gCt $(gCl)'

# Data (d)
alias gd='git ls-files'
alias gdc='git ls-files --cached'
alias gdx='git ls-files --deleted'
alias gdm='git ls-files --modified'
alias gdu='git ls-files --other --exclude-standard'
alias gdk='git ls-files --killed'
alias gdi='git status --porcelain --short --ignored | sed -n "s/^!! //p"'

# Fetch (f)
alias gf='git fetch'
alias gfa='git fetch --all'
alias gfc='git clone'
alias gfm='git pull'
alias gfr='git pull --rebase'

# Flow (F)
alias gFi='git flow init'
alias gFf='git flow feature'
alias gFb='git flow bugfix'
alias gFl='git flow release'
alias gFh='git flow hotfix'
alias gFs='git flow support'

alias gFfl='git flow feature list'
alias gFfs='git flow feature start'
alias gFff='git flow feature finish'
alias gFfp='git flow feature publish'
alias gFft='git flow feature track'
alias gFfd='git flow feature diff'
alias gFfr='git flow feature rebase'
alias gFfc='git flow feature checkout'
alias gFfm='git flow feature pull'
alias gFfx='git flow feature delete'

alias gFbl='git flow bugfix list'
alias gFbs='git flow bugfix start'
alias gFbf='git flow bugfix finish'
alias gFbp='git flow bugfix publish'
alias gFbt='git flow bugfix track'
alias gFbd='git flow bugfix diff'
alias gFbr='git flow bugfix rebase'
alias gFbc='git flow bugfix checkout'
alias gFbm='git flow bugfix pull'
alias gFbx='git flow bugfix delete'

alias gFll='git flow release list'
alias gFls='git flow release start'
alias gFlf='git flow release finish'
alias gFlp='git flow release publish'
alias gFlt='git flow release track'
alias gFld='git flow release diff'
alias gFlr='git flow release rebase'
alias gFlc='git flow release checkout'
alias gFlm='git flow release pull'
alias gFlx='git flow release delete'

alias gFhl='git flow hotfix list'
alias gFhs='git flow hotfix start'
alias gFhf='git flow hotfix finish'
alias gFhp='git flow hotfix publish'
alias gFht='git flow hotfix track'
alias gFhd='git flow hotfix diff'
alias gFhr='git flow hotfix rebase'
alias gFhc='git flow hotfix checkout'
alias gFhm='git flow hotfix pull'
alias gFhx='git flow hotfix delete'

alias gFsl='git flow support list'
alias gFss='git flow support start'
alias gFsf='git flow support finish'
alias gFsp='git flow support publish'
alias gFst='git flow support track'
alias gFsd='git flow support diff'
alias gFsr='git flow support rebase'
alias gFsc='git flow support checkout'
alias gFsm='git flow support pull'
alias gFsx='git flow support delete'

# Grep (g)
alias gg='git grep'
alias ggi='git grep --ignore-case'
alias ggl='git grep --files-with-matches'
alias ggL='git grep --files-without-matches'
alias ggv='git grep --invert-match'
alias ggw='git grep --word-regexp'

# Index (i)
alias gia='git add'
alias giA='git add --patch'
alias giu='git add --update'
alias gid='git diff --no-ext-diff --cached'
alias giD='git diff --no-ext-diff --cached --word-diff'
alias gii='git update-index --assume-unchanged'
alias giI='git update-index --no-assume-unchanged'
alias gir='git reset'
alias giR='git reset --patch'
alias gix='git rm -r --cached'
alias giX='git rm -rf --cached'

# Log (l)
alias gl='git log --topo-order --pretty'
alias gls='git log --topo-order --stat --pretty'
alias gld='git log --topo-order --stat --patch --full-diff --pretty'
alias glo='git log --topo-order --pretty'
alias glg='git log --topo-order --all --graph --pretty'
alias glb='git log --topo-order --pretty'
alias glc='git shortlog --summary --numbered'

# Merge (m)
alias gm='git merge'
alias gmC='git merge --no-commit'
alias gmF='git merge --no-ff'
alias gma='git merge --abort'
alias gmt='git mergetool'

# Push (p)

alias gp='git push'
alias gpf='git push --force-with-lease'
alias gpF='git push --force'
alias gpa='git push --all'
alias gpA='git push --all && git push --tags'
alias gpt='git push --tags'
alias gpc='git push --set-upstream origin "$(git_branch)"'
alias gpp='git pull origin "$(git_branch)" && git push origin "$(git_branch)"'

# Rebase (r)
alias gr='git rebase'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias gri='git rebase --interactive'
alias grs='git rebase --skip'

# Remote (R)
alias gR='git remote'
alias gRl='git remote --verbose'
alias gRa='git remote add'
alias gRx='git remote rm'
alias gRm='git remote rename'
alias gRu='git remote update'
alias gRp='git remote prune'
alias gRs='git remote show'
alias gRb='git config --get remote.origin.url'

# Stash (s)
alias gs='git stash'
alias gsa='git stash apply'
alias gsx='git stash drop'
alias gsX='git-stash-clear-interactive'
alias gsl='git stash list'
alias gsL='git-stash-dropped'
alias gsd='git stash show --patch --stat'
alias gsp='git stash pop'
alias gsr='git-stash-recover'
alias gss='git stash save --include-untracked'
alias gsS='git stash save --patch --no-keep-index'
alias gsw='git stash save --include-untracked --keep-index'

# Submodule (S)
alias gS='git submodule'
alias gSa='git submodule add'
alias gSf='git submodule foreach'
alias gSi='git submodule init'
alias gSI='git submodule update --init --recursive'
alias gSl='git submodule status'
alias gSm='git-submodule-move'
alias gSs='git submodule sync'
alias gSu='git submodule foreach git pull origin master'
alias gSx='git-submodule-remove'

# Working Copy (w)
alias gws='git status --ignore-submodules --short'
alias gwS='git status --ignore-submodules'
alias gwd='git diff --no-ext-diff'
alias gwD='git diff --no-ext-diff --word-diff'
alias gwr='git reset --soft'
alias gwR='git reset --hard'
alias gwc='git clean -n'
alias gwC='git clean -f'
alias gwx='git rm -r'
alias gwX='git rm -rf'
alias gwn=git_branch
