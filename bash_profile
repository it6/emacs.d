# use git auto completion

if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash

  # Add git completion to aliases
  __git_complete g _git_main
  __git_complete gco _git_checkout
  __git_complete gm _git_merge
  __git_complete gb _git_branch
fi


# use bashrc
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
