# Archived Next Actions

## 2024-05-19:

- [~] Find a black theme for windows.
- [x] Installed zoxidie(z)


## 2024-05-22:

- [x] Write Blog first: [[抛弃gui基于文本和命令行最大化编程效率]]

## 2024-05-25

- [x] Learn how to use Windows Carnac or OBS? 
- [x] Check saved 2 youtube videos on how to make videos.
- [x] Convert Blog to Vlog

## 2024-05-26

- [x] Learn leap.nvim
- [x] Complete python virtualenv setup in docker
- [x] Complete todo inside ~/docker/docker.sh
- [x] apply the same python setup to host

## 2024-05-27

- [x] pyright, black, isort plugins are not supporting python2.7
- [x] refine python virtual env for docker

## 2024-05-29

- [x] try coc-python in neovim see if it supports both python2.7 and python3: the answer is yes
- [x] mini-home-pc in docker env ./config is root:root, check laptop, if the same thing happens and if we need to update it to jiawzhang:jiawzhang in docker.sh
- [x] restore host python setup after figure out coc-python above
- [x] python debug feature in neovim

## 2024-05-30

- [x] Upload Vlog to Bilibili, Youtube

## 2024-05-31

- [x] coc.lua breaks lua auto completion, resolved by adding enable/disable coc when editing python
- [x] coc-python <c-space> has conflicts with nvim-cmp, resolved by change it to <c-j> in coc.lua
- [x] 'go to definition or reference' in coc-python reports error when applying on new changes without saving, resolved by saving before applying

## 2024-06-01

- [x] Install java in docker
- [x] replace leap.nvim with flash.nvim to avoid highlighting issue in docker env and more features.
- [x] Install jdk in host pc
- [x] add java sample project

## 2024-06-03

- [x] Setup neovim for java in linux: https://www.youtube.com/watch?v=TryxysOh-fI
- [x] Finishing watching tech video on how to setup python and java in neovim

## 2024-06-04

- [x] Make neovim java works in MacOS and laptop
- [x] keynav.rc install for laptop pc
    ```
    # XXX: add by jiawzhang
    f warp,click 1,end
    d warp,doubleclick 1,end
    r warp,click 3,end
    ```
- [x] Install youtube blocker plugin in edge browser: "ADGUARD"


## 2024-06-05

- [x] completed most TODOs in neovim init.lua

## 2024-06-07

- [x] Move productproject from bitbucket to github


## 2024-06-09

- [x] dockerized mysql

## 2024-06-10

- [x] git clone again productproject from github on mac(done) and homepc(done) and minipc(done), retiring bitbucket.org

## 2024-06-11

- [x] make nginx/uwsgi work in docker

## 2024-06-12

- [x] setup rabbitmq and celery into docker

## 2024-06-13

- [x] add pdf support

## 2024-06-22

- [x] Make docker with Ubuntu22.04 work for zybuluo

## 2024-06-23

- [x] how to ssh to wsl2 from another machine [ssh-to-wsl-solution](https://bonguides.com/how-to-ssh-access-to-a-wsl-distro-from-a-remote-computer/)

## 2024-06-25

- [x] clean up bad notes and uploaded file which caused huge traffic and cost

## 2024-06-28

- [x] verify and clean up mysql problem

## 2024-07-02

- [x] refine ubuntu_install_guide.sh

## 2024-07-04

- [x] handle [[zybuluo-improvement#Fix Plan for Zybuluo]] and unify them in a single doc somewhere.

## 2024-07-09

- [x] Setup dev env for gmk Ubuntu mini pc and Switch games on gmk Ubuntu

## 2024-07-10

- [x] Install orbstack on macbook to replace docker desktop, proved better than Docker Desktop

## 2024-07-16

- [x] Investigate titling windows manager in Win11 [Komorebi](https://github.com/LGUG2Z/komorebi).

## 2024-07-17

- [x] try i3wm titled window manager on ubuntu desktop in gmk-ubuntu
- [x] Update neovim plugins to the latest version causing issue, but I have ~/.config/nvim/lazy-lock.json to make sure everything is good. Update init.lua to make lazy latest versions work again.

## 2024-07-18

- [x] [[archived-projects#Dockerize zybuluo on Ubuntu 22.04 %5BBD: 2024-06-07 CD: 2024-07-18%5D]]

## 2024-07-21

- [x] switch key binding from whkd for ahk for komorebic

## 2024-07-27

- [x] Make zybuluo docker work in prod
- [x] publish a vlog to explain zybuluo docker

## 2024-07-31

- [x] introduce client.zybuluo.com in qiniu to block spike download traffic on cmd markdown apps.
- [x] production docker --stop and --start doesn't work, have to use --remove and --run

## 2024-08-01

- [x] [[archived-projects#Optimize zybuluo production %5BBD: 2024-07-19 CD: 2024-08-01%5D]]

## 2024-08-03

- [x] reinstall Ubuntu24.04 to resolve the terminal input delay issue. not sure why it happened, maybe after i use mysqldump and mysql to export and import data for more than 7 hours, some disk issue inside, format and reinstall ubuntu to resolve this issue.

## 2024-08-04

- [x] Complete to support flowchart.js 1.18.0

## 2024-08-05

- [x] Deployed flowchart.js v1.18.0 to prod along with its mdeditor html

## 2024-08-27

- [x] try to rebuld v0.10.0 neovim on MacOS and see if it resolves the crash issue.

## 2024-08-29

- [x] Make pixel 6a work for linux and android
- [x] Write blog [[天命人没苦硬吃在手机上安装完全体-linux-和-docker]]

## 2024-08-31

- [x] update mermaid to latest version from 6.0.0 to 10.9.1

## 2024-09-07

- [x] Make pdf download work for latest mermaid charts, change docker file and codes to support `puppeteer` to convert html to pdf server side

## 2024-09-11

- [x] Host mermaid.min.js to zybuluo instead of cdn, cdn may be blocked in China

## 2024-09-12

- [x] add version to mermaid.min.js to avoid cache in client
- [x] fix mermaid graph issue in published note
- [x] fix wrongly counting characters on notes containing mermaid graph 

## 2024-09-13

- [x] upgrade mermaid from v10.9.1 to v11.0.2 to support more diagrams
- [x] fix all the bugs introduced by new mermaid integration

## 2024-09-19

- [x] [[zybuluo-improvement#Make the latest mermaid work on zybuluo]]

## 2024-09-30

- [x] Make zybuluo work on MacOS arm64

## 2024-10-10

- [x] mermaid rendering is overlapped on MacOS chrome/safari browser, test with <Cmd Markdown 15th release> doc

## 2024-10-19

- [x] Move everything from my Ubuntu Desktop to docker container

## 2024-10-30

- [x] Investigate new neovim plugin for the feature of Cursor AI IDE (avante.nvim)
    - [x] customize more shortcuts for avante.nvim like chatgpt.nvim
    - [x] how to quickly switch models between local and remote

## 2024-11-07

- [x] Learn aider and write down skills/tips, write some in [[neovim-tips#Tips for AI Plugins]]
    - [x] Watch youtube to learn
    - [x] use local model instead of remote claude-3.5-sonnect
    - [x] how to quickly switch models between local and remote

## 2024-11-29

- [x] complete setup vim-dadbod-ui to access database like mysql/azuresql in neovim

## 2024-11-30

- [x] complete setup rest.nvim to replace postman in neovim

## 2024-12-01

- [x] Make rest.nvim work in my flow for rest http calls like postman in neovim
- [x] Make vim-dadbod-ui work in my flow for database client

## 2024-12-10

- [x] Sell egpu docker on ebay.com

## 2024-12-13

- [x] Make lazygit work in my work flow

## 2025-09-16

- [x] Upgrade nvim avante LLM to claude sonnect 4 from 3.5

## 2025-11-06

- [x] practice again neovim, practice [mini.nvim](~/.config/nvim/init.lua) and [[neovim-tips]]
