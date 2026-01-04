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

```bash
cp ~/docker/HackNerdFont-Regular.ttf ~/.termux/font.ttf
termux-reload-settings
```

### Grant storage permission if needed

```bash
# Grant storage permission if needed
termux-setup-storage

# Copy and rename (replace 'MyFont.ttf' with your file)
cp /sdcard/Download/HackNerdFont-Regular.ttf ~/.termux/font.ttf
```

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

---

# Fixing SSH Disconnections in Termux on Android

When switching to other apps like Chrome, Android often kills background processes or sleeps network sockets. Follow these steps to prevent your SSH sessions from dropping.

## 1. Acquire Wakelock
This is the most direct way to tell Termux to keep running in the background.
- Pull down the Android **Notification Center**.
- Expand the **Termux notification**.
- Tap **"Acquire wakelock"**.

## 2. Disable Battery Optimizations
Prevent Android's power management from throttling Termux.
- Go to Android **Settings** > **Apps** > **Termux**.
- Tap **Battery** or **Battery Optimization**.
- Select **"Unrestricted"** or **"Don't optimize"**.

## 3. Configure SSH Keepalive
Force the SSH client to send heartbeats to keep the connection active.
- Create or edit the SSH config file in Termux: `nano ~/.ssh/config`
- Add the following configuration:
  ```text
  Host *
      ServerAliveInterval 60
      ServerAliveCountMax 3
  ```
