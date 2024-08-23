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

4. Native docker on Pixel 6a

    a. Follow this to unlock the bootloader and root Pixel 6a

       https://www.xda-developers.com/how-to-unlock-bootloader-root-magisk-google-pixel-6a/

    b. Install and follow readme carefully with this app

       https://github.com/wasdwasd0105/limbo_tensor/releases (limbo-kvm-release-0.5.0.apk)
       https://github.com/wasdwasd0105/limbo_tensor 

    c. Other resources:

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
3. Enable wifi hotspot and disable "turn off hotspot automatically" (otherwise, ssh is slow when android screen is off)
