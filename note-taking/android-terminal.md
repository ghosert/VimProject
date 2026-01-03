# Android Terminal Setup & Tips

## 1. SSH from Android Tablet to Another Machine

### Setup Android Client

**Termux (CLI)**
1.  Install **Termux** (F-Droid recommended, download from github).
2.  Update and install SSH:
    ```bash
    pkg update && pkg upgrade
    pkg install openssh
    ```
3.  Connect:
    ```bash
    ssh username@192.168.1.50
    ```
4. pkg install vim

5. pkg install git

---

## 2. Termux Configuration Tips

### Update/Change Font

1. git clone https://github.com/adi1090x/termux-style
2. cd termux-style
3. ./install
After installation, just type termux-style to get an interactive menu to change both fonts and color schemes.


### Hide/Show Shortcut Bar (Extra Keys)

1. Open the properties file:
    vi ~/.termux/termux.properties
2. Add or modify this line (if it's already there, make sure it's not commented out with a #):
    extra-keys = []
3. Save and exit (Press Ctrl+O, Enter, then Ctrl+X).
4. Apply the changes:
   termux-reload-settings

### Restore Font Size (in Tmux or Terminal)
Font size is controlled by the terminal, not Tmux.
*   **Resize:** `Ctrl` + `+` / `Ctrl` + `-`
