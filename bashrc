# bashrc
# Author: ced@ryick.net - http://ced.ryick.net
# License: WTFPL


if [[ $- != *i* || $TERM == "dumb" ]] ; then
	# Test for an interactive shell.  There is no need to set anything
	# past this point for scp and rcp, and it's important to refrain from
	# outputting anything in those cases.
	return
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

##
# if pwd (\w) is too long, replace first characters by $trunc_symbol
# @see: http://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/x783.html
##
prompt_pwd()
{
	# How many characters of the $PWD should be kept
	local pwdmaxlen=25
	# Indicate that there has been dir truncation
	local trunc_symbol="--"

	local dir=${PWD##*/}
	pwdmaxlen=$(( ( pwdmaxlen < ${#dir} ) ? ${#dir} : pwdmaxlen ))
	NEW_PWD=${PWD/#$HOME/\~}
	local pwdoffset=$(( ${#NEW_PWD} - pwdmaxlen ))
	if [ ${pwdoffset} -gt "0" ]
	then
		NEW_PWD=${NEW_PWD:$pwdoffset:$pwdmaxlen}
		NEW_PWD=${trunc_symbol}/${NEW_PWD#*/}
	fi
	echo $NEW_PWD
}


##
# @see PROMPT_COMMAND
##
prompt()
{
	#save $? before it is overrided
	local error=$?

        # https://unix.stackexchange.com/a/18443
        history -n; history -w; history -c; history -r;

	if [[ $error != 0 ]]; then
		if [[ $UID != 0 ]]; then
			PS1="${error} \[\e[37;41m\]\h \[\e[01;37m\]$(prompt_pwd) $ \[\e[0m\]"
		else
			PS1="${error} \[\e[41;36m\]\h \[\e[37;41m\]\w # \[\e[0m\]"
		fi
	else
		if [[ $UID != 0 ]]; then
			#\[\e[s\] save cursor
			#\[\e[u\] restor cursor
			#\[\e[1;\$((COLUMNS-4))f\] write at row 1 col max-4
			#\[\e[K\] clear to endofline
			PS1="${prefix}\[\e[01;36m\]\h \[\e[01;37m\]$(prompt_pwd) $ \[\e[0m\]"
		else
			#not too much fancy stuff for root
			PS1="\[\e[01;31m\]\h \[\e[37;1m\]\w # \[\e[0m\]"
		fi
	fi
}

case $TERM in
	xterm*|rxvt*|Eterm|screen*|linux)
		PROMPT_COMMAND="prompt${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
		;;
	*)
		PS1='\h:\w '
		;;
esac



# contextual grep.
# search for a pattern in current dir only in code file (EXT)
# display matched pattern with filename and 3 lines of context.
# usage: $ search om_kill_process
search()
{
	local MOTIF
	local EXT
	local EXTARGS

	MOTIF="$1"

	EXT=(".hpp" ".h" ".c" ".sh" ".cpp" ".py" ".pl" ".java" ".lzx" ".php" ".js")
	EXECGREP="-exec /bin/grep -H -i -n -R -B3 -A3 --color \"$MOTIF\" '{}' \;"
	EXTARG="-name '*${EXT[0]}' $EXECGREP"

	for i in ${EXT[@]:1}
	do
		EXTARG="$EXTARG -o -name '*$i' $EXECGREP"
	done
	CLI="/usr/bin/find . $EXTARG"
	eval "$CLI"
}



# because /etc/services is not enough
# usage: $ iana 1935
iana()
{
        wget -q -O /dev/stdout http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.txt | grep "$@"
}



# simple find -i, usefull to find a class in source tree.
# usage: $ ifind debug
ifind()
{
	find . -iname "*$@*"
}


# source customized dircolors if exist
if [[ -f ~/.dircolors ]]
then
	eval "`dircolors ~/.dircolors`"
fi



# colorize ls
export LS_OPTIONS="--color=auto"
# shortcut ls
alias ls="ls $LS_OPTIONS"
alias ll="ls $LS_OPTIONS -lh"
alias la="ls $LS_OPTIONS -la"
alias tiga="tig --all"
# colorize matched pattern
export GREP_COLOR="01;31"
alias grep="grep --color"
# history
export HISTSIZE=-1
export HISTCONTROL=erasedups:ignorespace
shopt -s histappend
export PATH=~/.local/bin:~/go/bin${PATH:+:$PATH}
export MANPATH=~/.local/man${MANPATH:+:$MANPATH}:
export LD_LIBRARY_PATH=~/.local/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
#sunrise
export ECHANGELOG_USER="Cédric Cabessa (ced_c) <ced@ryick.net>"
# what are my process ?
alias psme="ps aux | grep $(whoami)"
#quilt
export QUILT_PATCHES=../patches
export QUILT_PC=../.pc

# for those who don't use svn:ignore
alias svnstat="svn stat | grep -v ^?"
# use git, you'll not need this !
svnlog()
{
	svn log -v $@ | less
}

svndiff()
{
	svn diff $@ | less
}

notify()
{
	local me="ced"
	local token="xxxx"
	local title=$1
	shift

	notify-send $title "$*"
	#curl -s --data "*${title}*:$*" "https://genymobile.slack.com/services/hooks/slackbot?token=$token&channel=%40$me" > /dev/null
}

M()
{
	local cpus=$(grep ^processor /proc/cpuinfo | wc -l)
	local args="-j$(( $cpus * 3/2 )) $@"

	if type colormake > /dev/null 2>&1; then
		if colormake $args; then
			notify [$(prompt_pwd)] compilation succeed
		else
			notify [$(prompt_pwd)] compilation failed
		fi
	else
		make $args
	fi
}

# For root
# With great power comes great responsibility.
if [[ $UID != 0 ]]
then
	alias rm="rm -i"
	alias cp="cp -i"
	alias mv="mv -i"
fi


# term mode (root or noX), launch emacs daemon.
# in X mode emacs server is launched with emacs
if [[ -z $DISPLAY || $UID == 0 ]] && ! pgrep -u $(whoami) -f "emacs --daemon" > /dev/null
then
	emacs --daemon 2>/dev/null
fi


if [[ -n $DISPLAY && $UID != 0 ]]
then
	#X11 mode and normal user: if emacs is not yet launch, launch it
	#e is used for quick editing (don't hang on term)
	export EDITOR="emacsclient -a emacs"
	e()
	{
		emacsclient -n -a emacs $@
	}
else
	#assume emacs daemon is launched
	export EDITOR="emacsclient -a vim"
	e()
	{
		$EDITOR $@
	}
fi

export VISUAL=$EDITOR

# resume (or create) a tmux session if I come by SSH.
if [ -n "$SSH_CONNECTION" ] && [ -z "$TMUX_EXIST" ] && type tmux >/dev/null 2>&1; then
	export TMUX_EXIST=1
	tmux attach -d || tmux
	exit
fi

irssinotify()
{
	if ! ssh-add -l >/dev/null 2>&1; then
		ssh-add
	fi

	while true
	do
		ssh -o ServerAliveInterval=60 cedc@ced.ryick.net 'tail -f -n0 ~/.irssi/fnotify' |
		while read heading msg; do
			msg=$(echo "${msg}" | sed 's/<\s*\|\s*>//g')
			notify "${heading}" "${msg}"
		done
		sleep 60
	done
}

export USE_CCACHE=1
export CCACHE_DIR=~/.ccache
export TERMINAL="rxvt256c -bg black -fg green -sl 20000 -fn 'xft:DejaVuSansMono:pixelsize=12'"

dkr() {
    docker exec -i -t $1 /bin/bash
}

if [[ -f /usr/share/fzf/shell/key-bindings.bash ]]; then
   . /usr/share/fzf/shell/key-bindings.bash
fi