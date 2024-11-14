# Tiling Window Manager

## Windows

[komorebi](https://github.com/LGUG2Z/komorebi)

### Install/Uninstall Guide

https://lgug2z.github.io/komorebi/installation.html

1. I used scoop to install, winget doesn't work for me
2. 'scoop install git' also required.
3. 'scoop install vim' I installed for editing
4. run 'komorebic quickstart' to load default configs ~/komorebi.json ~/.config/whkdrc
5. run 'komorebic start --whkd' to start with whkd
6. to start komorebi with autohotkey(if whkd doesn't work for powershell 7.0+):
```
  a) Install autohotkey manually
  b) set windows system path with autohotkey installation path
  c) copy ahk script from https://lgug2z.github.io/komorebi/common-workflows/autohotkey.html?h=autohotkey to ~/komorebi.ahk
  d) 'komorebic start --ahk', you can then 'scoop uninstall whkd' and remove ~/.config/whkdrc
```

### Tips

1. config file: ~/komorebi.json
2. shortcut file: ~/.config/whkdrc or ~/komorebi.ahk
3. I created powershell rc file like .zshrc to start komorebi whenever start powershell
```bash
    ~\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1

    if (-not (Get-Process -Name "komorebi" -ErrorAction SilentlyContinue)) {
        Start-Process "komorebic" -ArgumentList "start --ahk --bar"
    }
    ssh jiawzhang@gmk-ubuntu
```

