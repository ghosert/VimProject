# Tiling Window Manager

## Windows

[komorebi](https://github.com/LGUG2Z/komorebi)

### Install/Uninstall Guide

https://lgug2z.github.io/komorebi/installation.html

1. I used scoop to install, winget doesn't work for me
2. 'scoop install git' also required.
3. 'scoop install vim' I installed for editing

### Tips

1. config file: ~/komorebi.json
2. shortcut file: ~/.config/whkdrc
3. I created powershell rc file like .zshrc to start komorebi whenever start powershell
    ~\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1

    if (-not (Get-Process -Name "komorebi" -ErrorAction SilentlyContinue)) {
        Start-Process "komorebic" -ArgumentList "start --whkd"
    }
    ssh jiawzhang@gmk-ubuntu

