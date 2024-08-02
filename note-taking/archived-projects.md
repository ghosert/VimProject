# Archived Projects

## Build Note taking system in neovim

### 2024-05-19

- [x] Move zybuluo TODOs to here.
- [x] mimic remember the milk or build my own note taking system
- [x] Make neovim support markdown
- [x] based on markdown, articles can have a link to jump, check [[neovim-tips#Tips for Marksman]]
- [x] Reduce the file size of ~/VimProject
- [~] Build a note taking system by neovim(display pic by: https://github.com/3rd/image.nvim)


## First Vlog

### 2024-05-31

- [x] Write Blog first: [[抛弃gui基于文本和命令行最大化编程效率]]
- [x] Learn how to use Windows Carnac or OBS? 
- [x] Check saved 2 youtube videos on how to make videos.
- [x] Convert Blog to Vlog
- [x] Upload Vlog to Bilibili, Youtube
- [x] Upload Blog to www.zybuluo.com, zhihu.com


## Improve docker dev env

### 2024-06-05

- [x] Search TODO in ~/.config/nvim/init.lua
- [x] Setup neovim for Java(linux, mac): https://www.youtube.com/watch?v=TryxysOh-fI
- [x] Setup neovim for Python

## Setup new gmk-m6

### 2024-07-10

- [~] setup Proxmox VE on gmk-m6
- [~] video card pass through for windows in PVE and make sure you can run Ubuntu simultaneously at least for server version.
- [~] install some other systems onto this PVE
- [x] Install Ubuntu 24.04 on gmk-m6
- [x] move my docker and dev env to gmk-m6
- [x] Installed switch games into Ubuntu 24.04

## Dockerize zybuluo on Ubuntu 22.04 [BD: 2024-06-07 CD: 2024-07-18]

- [x] check all TODO under ~/docker
- [x] move more stuff from ~/productproject/ to ~/docker/zybuluo/
- [x] verify all features and make sure it works no issue
- [~] combine app & mysql docker files to one composite file.
- [x] dockerize the entirement runtime env to docker based on Ubuntu 22.04

## Optimize zybuluo production [BD: 2024-07-19 CD: 2024-08-01]

- [x] deploy docker to amazon production
    - [x] build an image for amazon production and pull it from reserved amazon instance
    - [x] check how to call amazon rds from docker container
    - [x] run docker image and expose 443 to host from container
- [x] git clone again productproject from github in production.
- [x] database in mysql docker takes 130GB now, the data is coming from production. if we need that much data, can we re-use the small dataset in home-pc [we have bigger disk now and also we have solution for small dataset]
- [x] clean up amazon prod home folder

