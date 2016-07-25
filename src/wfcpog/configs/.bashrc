# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# gerneral variables
export PATH=$PATH:"/usr/lib/jvm/java-1.5.0-sun-1.5.0.08/bin/"
export CLASSPATH=$CLASSPATH:"/opt/tinyos-2.x/support/sdk/java/tinyos.jar"

# JAVA variables
export JAVA_HOME="/usr/lib/jvm/java-1.5.0-sun-1.5.0.08/"

# CRUISECONTROL variables
export CC_HOME="/home/joe/software/cruisecontrol"
export CC_PRO="$CC_HOME/cruisecontrol-bin-2.7.1/projects"
export CC_WSN="$CC_PRO/manu_wsntest"

# TINYOS variables
export TOSROOT="/opt/tinyos-2.x"
export TOSDIR="$TOSROOT/tos"
export MAKERULES="$TOSROOT/support/make/Makerules"
export PYTHONPATH="/opt/tinyos-2.x/support/sdk/python/"

# DNS variables
export DSNUSER="btnode"
export DSNPASS="btadmin"
export DSNSRV="192.168.110.13"
export DSNPORT="8887"
export TINY_APP_PATH="$CC_WSN/"
#export TEST_TOPOLOGY="inss_topo"
export DSNROOTNODE="00:04:3f:00:01:ae"
export TOSSIMROOTNODE="30"
export TCL_LIBRARY="/usr/lib/tcl8.4"
export TK_LIBRARY="/usr/lib/tk8.4"

# SU4SML variables
export SU4SML_HOME="$HOME/ethz/masterthesis/wf-check_and_po-generation/su4sml"
export WFCPO_HOME="$SU4SML_HOME/src/wfcpo-gen"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
xterm-color)
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    ;;
*)
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    ;;
esac

# Comment in the above and uncomment this below for a color prompt
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable color support of ls and also add handy aliases
#if [ "$TERM" != "dumb" ]; then
#    eval "`dircolors -b`"
#    alias ls='ls --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
# fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Set up TinyOS environment according to
# http://5secondfuse.com/tinyos/install.html
if [ -f ~/.bash_tinyos ]; then
    . ~/.bash_tinyos
fi
