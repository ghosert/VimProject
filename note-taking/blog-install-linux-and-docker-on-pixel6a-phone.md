# 天命人没苦硬吃，在手机上安装完全体 Linux 和 Docker

Tags: blog

---

## 身外之物的两种权利

人拥有了一样东西，也就拥有了两种权利，一种是使用它的权利，另一种是改变它用途的权利。某些时候我更喜欢第二种权利，因为 LINUS 这么想的时候，Linux 就诞生了。
两年前，女儿的 Pixel 6a 手机刚买没多久就碎屏，给她换屏换手机又不要，没苦硬吃愣是又用了一年多，终于在今年夏令营的时候划太不动了，买了新的，家里又多了一台电子垃圾。好在我向来注重内在美，左看看右悄悄，除了颜值拉胯，这处理器、内存不比树莓派强么？遂决定研究一下怎么给这手机整个 Linux 系统。讲真，家里除了没有泼天的富贵，有的是泼天的各种机器，但是人类有时就是好奇心驱动，能不能干成很重要，至于干成了有什么用不那么重要。于是我晚上做梦都在想：一台 ARM 手机，接上电源，关闭屏幕，24 小时以极低的功耗，用 6GB 内存，8 核心，128GB存储，默默运行自己的服务器代码，关闭 GUI 桌面, 打开 SSH，连接 WIFI 随时候命我的远程访问，用 NEOVIM 在纯终端里打开整套开发环境，真香。

### 碎屏 PIXEL 6A 
<img src="https://static.zybuluo.com/ghosert/lw3xxeu0o2uce8rwxu4ggrnz/20240824_145727.jpg" width="600"/>

## Linux 残体和完全体

印象中十几年前我就在手机上折腾过 Ubuntu Desktop，一开始觉得这事不难，谷歌、CHATGPT、COPILOT 一顿问，果然基本上只需要装一些 APP 例如：UserLand 就可以模拟一个 Linux，支持从远程 SSH，这种方法虽然容易操作，但底层实现原理都是基于 chroot，这是一个残体 Linux，很多系统级的权限或者标准 Linux 文件系统，用户空间没有开放，所以跑不了 Docker，现在的 Linux，跑不了 Docker，那和废物也差不多了，不香，我要完全体的 Linux 可以在上面跑我所有数据库、应用服务器和开发环境。

### 安装 TERMUX/SSHD
<img src="https://static.zybuluo.com/ghosert/34yicld420b35k429itwio7g/20240815_140527.jpg" width="600"/>



## 不知天高地厚的开始

网络上第一篇靠谱的文章说是可以不用 ROOT 机器，借用 QEMU 虚拟机技术在安卓上跑原生 LINUX，但是眼瞅着 qemu-system-x86_64 这个命令，明显是在 ARM 上模拟 X86 指令，还是在虚拟机里，这是得多慢？但是多慢也得试试。在手机上装上 TERMUX/SSHD 算是趟入了从主力设备远程进手机，用键盘操作的不归之路。然后再和 CHATGPT 一顿聊，基本流程就是自己琢磨个 QEMU 的参数，在 TERMINAL 里试，报错后贴到 CHATGPT 里问啥意思，CHATGPT 回答怎么调整参数或者去哪里下载缺少的BIOS、FIRMWARE，然后再去 TERMINAL 里试命令，反复调整，奋战了一两个晚上终于跑通了。手机上用 QEMU 模拟出一个精简的 ALPINE LINUX 操作系统，在这个系统上也能把 Docker 跑通，可惜就是跑在虚拟机上，启动 LINUX 需要几分钟，跑个 DOCKER BUILD 给我整了一晚上，眼见是不堪大用，没啥实用性。

原文链接：https://medium.com/@kumargaurav.pandey/docker-on-mobile-that-too-without-root-how-7b0848833c42

成功在 QEMU X86 64 上龟速启动 ALPINE LINUX
<img src="https://static.zybuluo.com/ghosert/mcy2cbrow61z7bkof5fbobq0/20240820_193749.jpg" width="600"/>

## 亲自编译手机操作系统

网路上第二篇靠谱的文章说：可以直接在 安卓上运行 DOCKER，但是需要做这几个步骤：

1. 先把机器 ROOT 了，再解锁 BOOTLOADER，获得手机的至高权限
2. 修改手机操作系统的 KERNEL 编译选项，使得 DOCKER 可以跑起来，重新编译手机操作系统再把新系统刷到手机
3. 再来就是 DOCKER 也要重新编译若干组件

一般人看到这个地狱级的操作估计也就退了，但我不是一般人。依图索骥去 HACK 手机，在谷歌官网阅读无数 PIXEL KERNEL 的编译资料和操作守则，下了几百个G 的 PIXEL 源代码到本地，修改编译选项，开整。编译操作系统确实费力，我的主力性能机藏着 AMD 6600H 的芯，榨干了 32GB 的内存，CPU像高血压似得飙到99%，在楼下夜深人静的夜无能咆哮了一个多小时，终于炼丹成功。手忙脚乱的把新系统刷进手机，无端白屏，再无反应，默！我反思了一下，一定是谷歌文档的锅。绝对不追究他们的责任，找下一个靠谱的文章，就是又白瞎了两个晚上。

原文链接：https://gist.github.com/FreddieOliveira/efe850df7ff3951cb62d74bd770dce27#41-kernel-patches

### 解锁手机
<img src="https://static.zybuluo.com/ghosert/5rfam79qq0898qhxnq4bcuh9/20240817_142728.jpg" width="600"/>

## 亲自编译 LINUX KERNEL，事态逐渐失控

来到第三篇靠谱的文章说：GOOGLE PIXEL 手机支持一种 crosvm 技术，它是基于 Linux KVM 专门在 CHROME OS 上运行 Linux、Android 的虚拟化技术，有接近宿主机的性能表现，而且有人在 PIXEL 6A 真能跑起来原生 Linux。千辛万苦找来了网上别人编译好的LINUX KERNEL，启动 crosvm 外加一堆参数，果然可以进系统，问题是这是一个只读系统，性能不是问题，问题是不能安装更新软件，一样跑不了 DOCKER。又是白瞎了一个晚上，一怒之下，跑去LINUX KERNEL的官网，自己开始下载源码，编译 KERNEL，事到如今，我女儿只不过摔了个手机，何至于此我要去编译 LINUX KERNEL，事态已经完全失控。

结局依然很默，自己的编译的 KERNEL 也跑不起来 CROSVM，CHATGPT 和 COPILOT 被我问到吐了也答不上来。反思了一下，这也不是人工智能的锅，这种什么在手机上跑 DOCKER 的无理要求，可能领先了这个时代整整一个身位。

原文链接：https://www.esper.io/blog/android-dessert-bites-13-virtualization-on-pixel-6-379185

### 我能想到的和我做不到的，是不是有人已经做到了

努力白瞎了五六个晚上之后，短暂修整了一下自己，我复盘了一下这个事情

1. 这是一个没必要的事情
2. 作为一个服务器端开发者和早年的桌面开发者，虽然我可以各种折腾各种 Linux 开源软件和操作系统，但是Linux和安卓开发在我能力范围之外
3. 基于第一二点，我是不是应该放弃这个事情
4. 对于第三点的答案是，如果我搞不定这个事情，我可能真的会去学安卓系统的KERNEL开发

背脊已经发凉了，我被自己的执念吓到，所以在第六个晚上我终于做到了，有最后一张图为证，全世界能做成这个事情的手机型号不会超过十个，PIXEL 6A 算一个，但是这张纸完全不够写，就不详细说了，今天的我：

一台 ARM 手机，接上电源，关闭屏幕，24 小时以极低的功耗，用 6GB 内存，8 核心，128GB存储，默默运行自己的服务器代码，关闭 GUI 桌面, 打开 SSH，连接 WIFI 随时候命我的远程访问，用 NEOVIM 在纯终端里打开整套开发环境，真香。

全文完。
<img src="https://static.zybuluo.com/ghosert/bf8mpx2v17w67k3wi7e8qgeb/Screenshot%202024-08-23%20141958.png" width="600"/>

  [1]: https://static.zybuluo.com/ghosert/lw3xxeu0o2uce8rwxu4ggrnz/20240824_145727.jpg
  [2]: https://static.zybuluo.com/ghosert/34yicld420b35k429itwio7g/20240815_140527.jpg
  [3]: https://static.zybuluo.com/ghosert/mcy2cbrow61z7bkof5fbobq0/20240820_193749.jpg
  [4]: https://static.zybuluo.com/ghosert/5rfam79qq0898qhxnq4bcuh9/20240817_142728.jpg
  [5]: https://static.zybuluo.com/ghosert/bf8mpx2v17w67k3wi7e8qgeb/Screenshot%202024-08-23%20141958.png
