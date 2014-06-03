# bashrc
# Author: ced@ryick.net - http://ced.ryick.net
# License: WTFPL


if [[ $- != *i* || $TERM == "dumb" ]] ; then
	# Test for an interactive shell.  There is no need to set anything
	# past this point for scp and rcp, and it's important to refrain from
	# outputting anything in those cases.
	return
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
# return sys info: load average (5min), mem available, swap used, svn and git
# branch for the current folder.
##
prompt_info()
{
	local load5=$(awk '{print $2}' /proc/loadavg)
	local memf=$(($(grep ^MemFree /proc/meminfo |awk '{print $2}')/1024))
	local swaptotal=$(($(grep ^SwapTotal /proc/meminfo |awk '{print $2}')/1024))
	local swapfree=$(($(grep ^SwapFree /proc/meminfo |awk '{print $2}')/1024))
	local swap=$(($swaptotal - $swapfree))

	local svnmsg=""
	if [[ -d .svn ]]
	then
		local branch=$(LANG=C svn info 2>/dev/null | grep ^URL: | grep  /branches/ | sed 's%.*/branches/\([[:alnum:]_\-\.]*\)\(/.*\)\{0,1\}%\1%g')
		branch=${branch:-"trunk"}
		svnmsg="svn:$branch"
	fi

	local gitmsg=$(git branch --no-color 2> /dev/null | grep ^\* | awk '{print $2}')
	if [[ "_$gitmsg" != "_" ]]
	then
		gitmsg="git:$gitmsg"
	fi

	echo -e "l5:$load5     mem:$memf     swap:$swap     $svnmsg$gitmsg"
}


##
# @see PROMPT_COMMAND
# @see PROMPT_COMMAND_PREFIX
##
prompt()
{
	#save $? before it is overrided
	local error=$?

	#write history
	history -a
	#reload history
	history -n
	local prefix="${SCHROOT_CHROOT_NAME}"
	if [[ -n ${prefix} ]]; then
		prefix="${prefix}-"
	fi
	prefix="${prefix}${PROMPT_COMMAND_PREFIX}"

	if [[ $error != 0 ]]; then
		if [[ $UID != 0 ]]; then
			#PS1="\[\e[s\]\[\e[1;1f\]\[\e[44m\]\[\e[01;37m\]\[$(prompt_info)\]\[\e[K\]\[\e[u\]\[\e[37;41m\]\h \[\e[01;37m\]$(prompt_pwd) $ \[\e[0m\]"
			PS1="${error} ${prefix}\[\e[37;41m\]\h \[\e[01;37m\]$(prompt_pwd) $ \[\e[0m\]"
		else
			PS1="${error} ${prefix}\[\e[41;36m\]\h \[\e[37;41m\]\w # \[\e[0m\]"
		fi
	else
		if [[ $UID != 0 ]]; then
			#\[\e[s\] save cursor
			#\[\e[u\] restor cursor
			#\[\e[1;\$((COLUMNS-4))f\] write at row 1 col max-4
			#\[\e[K\] clear to endofline
			#PS1="\[\e[s\]\[\e[1;1f\]\[\e[44m\]\[\e[01;37m\]\[$(prompt_info)\]\[\e[K\]\[\e[u\]\[\e[01;36m\]\h \[\e[01;37m\]$(prompt_pwd) $ \[\e[0m\]"
			PS1="${prefix}\[\e[01;36m\]\h \[\e[01;37m\]$(prompt_pwd) $ \[\e[0m\]"
		else
			#not too much fancy stuff for root
			PS1="\[\e[01;31m\]\h \[\e[37;1m\]\w # \[\e[0m\]"
		fi
	fi
}
#do not write on the status line (first line)
#echo ""

case $TERM in
	xterm*|rxvt*|Eterm|screen*|linux)
		PROMPT_COMMAND=prompt
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
export HISTSIZE=50000
shopt -s histappend
# home made binary go to ~/fakeroot
export PATH=/usr/java/latest/bin:~/fakeroot/bin:$PATH
export MANPATH=~/fakeroot/share/man:$MANPATH
export LD_LIBRARY_PATH=~/fakeroot/lib:~/fakeroot/usr/lib:$LD_LIBRARY_PATH
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

droid()
{
	if [[ -f build/envsetup.sh ]]; then
		. build/envsetup.sh
	else
		echo "little lost robot :-(" >&2
		return 1
	fi
	export USE_CCACHE=1
	if compgen -A function | grep brunch; then
		# cyanogenmod
		brunch $@
	else
		# aosp
		lunch $@
	fi
	# android prompt is borked
	PROMPT_COMMAND_PREFIX="${TARGET_PRODUCT}-${TARGET_BUILD_VARIANT} "
	PROMPT_COMMAND=prompt
}

M()
{
	local cpus=$(grep ^processor /proc/cpuinfo | wc -l)
	local makecmd="make -j $(( $cpus * 6/4 )) $*"

	if type colormake > /dev/null 2>&1; then
		if eval $makecmd  2>&1; then
			notify-send [$PWD] succeed
		else
			notify-send [$PWD] failed
		fi | colormake
	else
		eval $makecmd
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

# resume (or create) a screen session if I come by SSH.
if [ -n "$SSH_CONNECTION" ] && [ -z "$SCREEN_EXIST" ] && type screen 2>/dev/null; then
	export SCREEN_EXIST=1
	screen -RD
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
			notify-send "${heading}" "${msg}"
		done
		sleep 60
	done
}

export USE_CCACHE=1
export CCACHE_DIR=~/.ccache
export TERMINAL="rxvt256c -bg black -fg green -sl 20000 -fn 'xft:DejaVuSansMono:pixelsize=12'"
