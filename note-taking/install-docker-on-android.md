# Install docker on android

Refer to this article for the most: https://medium.com/@kumargaurav.pandey/docker-on-mobile-that-too-without-root-how-7b0848833c42

## Step 0: Get SSH access

1. Install app Termux from google app store to the android device

2. Open Termux and run

```bash
pkg update && pkg upgrade
pkg install openssh git curl wget
sshd # run sshd to start ssh server, NOTICE: this 'sshd' should be run every time when you kill Termux app and restart Termux.
passwd # this will set a new password to be used when ssh to android
```

3. SSH to android

Get android device IP address: 192.168.31.245 for example and run below from remote machine

```bash
ssh -p 8022 192.168.31.132
```

## If you want to just install Ubuntu inside Termux

```bash
pkg install proot-distro
pd install ubuntu
pd login ubuntu
```

4. Native Linux and docker on Pixel 6a

    a. Follow this to unlock the bootloader and root Pixel 6a

       https://www.xda-developers.com/how-to-unlock-bootloader-root-magisk-google-pixel-6a/

       Inside magisk app, import unlock_kvm_magisk_v2.zip as a module, this will be used to stable limbo_tensor app below looks like.

    b. Install and follow readme carefully with this app

       https://github.com/wasdwasd0105/limbo_tensor/releases (limbo-kvm-release-0.5.0.apk)
       https://github.com/wasdwasd0105/limbo_tensor 

       Before starting this limbo_tensor app"

       enable kvm option to get native performance, also enable UEFI to boot linux from iso file, set cpu number = 2 looks more stable
       set forward network like "tcp:2222:22", Network: User, NIC Card: e1000, so that after installing openssh, you can "ssh -p 2222 192.168.31.132" instead of vnc
       Boot linux with CD-ROM loaded with iso first to install the system to QCOW2 hard disk
       Remove CD-ROM and boot from hard disk

    c. Connect to native linux and docker

       Use an VNC app or VNC viewer desktop to connect 192.168.31.132:5900
       After install ssh inside linux: ssh -p 2222 192.168.31.132 without VNC
       Go with docker
       Install other linux distro like Ubuntu desktop by repeating steps above for limbo_tensor app on a separate QCOW2 hard disk

    d. Other resources:

       https://xdaforums.com/t/root-virtual-machine-with-kvm-acceleration-for-tensor-chips.4501665/
       https://www.esper.io/blog/android-dessert-bites-13-virtualization-on-pixel-6-379185
       https://www.worldofgh0st.com/native-linux-on-pixel-6-w-pkvm/
       https://xdaforums.com/t/trying-to-run-protected-virtual-machine.4485689/
       https://chromium.googlesource.com/chromiumos/platform/crosvm/+/master/README.md
       https://gist.github.com/FreddieOliveira/efe850df7ff3951cb62d74bd770dce27#41-kernel-patches
       https://medium.com/@kumargaurav.pandey/docker-on-mobile-that-too-without-root-how-7b0848833c42


## Make android, wifi, Termux awake when screen is off

1. Start Termux app with "wake lock", this can be set inside notifications after running Termux.
2. Allow Termux to use unrestricted wifi data, unrestricted battery usage
3. Enable wifi hotspot and disable "turn off hotspot automatically" (otherwise, ssh is slow when android screen is off, restarting wifi hotspot also helps most time)
4. keep limbo_tensor open on the screen looks like also make linux/docker system stable? especially for neovim editing, i saw neovim crash sometimes.
