(pv -n "$1" | tar xzf - -C . ) \
    2>&1 | dialog --gauge "Extracting file..." 6 50

clear
