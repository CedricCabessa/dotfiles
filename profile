if [ -f ~/.cargo/env ]; then
   . ~/.cargo/env
fi
export PATH=/snap/bin:$HOME/.local/bin:$HOME/bin:~/go/bin:/usr/local/go/bin:$HOME/node/bin:${HOME}/.krew/bin${PATH:+:$PATH}
export LD_LIBRARY_PATH=~/.local/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

#export PYENV_ROOT="$HOME/.pyenv"
#command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
#eval "$(pyenv init -)"

if [ -f ~/.private ]; then
    . ~/.private
fi

# >>> coursier install directory >>>
export PATH="$PATH:/home/ccabessa/.local/share/coursier/bin"
# <<< coursier install directory <<<
