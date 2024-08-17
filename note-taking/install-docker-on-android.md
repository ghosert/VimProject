# Install docker on android

Refer to this article for the most: https://medium.com/@kumargaurav.pandey/docker-on-mobile-that-too-without-root-how-7b0848833c42

## Step 0: Get SSH access

1. Install app Termux from google app store to the android device

2. Open Termux and run

```bash
pkg update && pkg upgrade
pkg install openssh git curl wget
sshd # run sshd to start ssh server
passwd # this will set a new password to be used when ssh to android
```

3. SSH to android

Get android device IP address: 192.168.31.245 for example and run below from remote machine

```bash
ssh -p 8022 192.168.31.245
```

## If you want to just install Ubuntu inside Termux

```bash
pkg install proot-distro
pd install ubuntu
pd login ubuntu
```

## Make android, wifi, Termux awake when screen is off

1. Start Termux app with "wake lock", this can be set inside notifications after running Termux.
2. Allow Termux to use unrestricted wifi data, unrestricted battery usage
3. Enable wifi hotspot and disable "turn off hotspot automatically" (otherwise, ssh is slow when android screen is off)
