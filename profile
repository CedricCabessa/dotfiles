if [ -f ~/.cargo/env ]; then
   . ~/.cargo/env
else
    export PATH="$HOME/.cargo/bin:$PATH"
fi
export PATH=/snap/bin:$HOME/.local/bin:$HOME/bin:~/go/bin:/usr/local/go/bin${PATH:+:$PATH}
export LD_LIBRARY_PATH=~/.local/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
