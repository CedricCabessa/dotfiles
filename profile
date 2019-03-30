if [ -f ~/.cargo/env ]; then
   . ~/.cargo/env
else
    export PATH="$HOME/.cargo/bin:$PATH"
fi
export PATH=$HOME/.local/bin:$HOME/bin:~/go/bin${PATH:+:$PATH}
export LD_LIBRARY_PATH=~/.local/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
