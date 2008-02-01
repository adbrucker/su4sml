if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
fi
alias pg='ps ax | grep '
alias kp='sudo kill -9 '
alias nb='nano ~/.bashrc'
alias fr='sudo find / -name '
alias vpn='sudo vpnc ethz'
alias cs='cd $SU4SML_HOME'
alias cw='cd $WFCPO_HOME'


alias su4sml='cd $HOME/ethz/masterthesis/wf-check_and_po-generation/su4sml'
alias wfcpo='cd $HOME/ethz/masterthesis/wf-check_and_po-generation/su4sml/src/wfcpo-gen'
alias exam='cd $HOME/ethz/masterthesis/wf-check_and_po-generation/examples'

