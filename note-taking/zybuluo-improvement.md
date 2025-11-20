# Zybuluo Improvement

## NOTE: Check ~/docker/zybuluo/ for setup 

## TODO Tasks

- [ ] [[#Integrate AI to Cmd Markdown]]
- [ ] Upgrade zybuluo AI model to the latest version
- [ ] check all the zybuluo changes at zybuluo.com my notes.
- [ ] Use local LLM to detect bad notes/users and delete them in both local and prod database
- [ ] select count(1) from user_notes limit 100 and date range is slow in prod. this impacts /admin/user_notes page, timeout issue. check how to enhance timeout and make sql faster
- [ ] block too many registered users from the same ip to post too many notes
- [ ] update appcache by making mdeditor js/css changes to update ssl validation date, read details in SETUP.markdown
- [ ] copy paste a vim guide for cmd markdown
- [ ] Don't allow the customer to refresh the read count forever
- [ ] check if celery keeps adding queues by running "sudo rabbitmqctl list_queues"
- [ ] window.applicationCache has been deprecated and zybuluo.com is showing js error due to this.
- [ ] Send emails to all the customer ask them to come back.
- [ ] buy baidu keywords and spread zybuluo anywhere
- [ ] Move Cmd Guide to about menu? Or refine about menu?
- [ ] add Cmd Markdown formulate to official link (https://ericp.cn/cmd), ask permission from the original author.
- [ ] How to show cmd desktop client and its version in google analytics?
- [ ] Upgrade latest ACE editor
- [ ] Integrate ChatGPT with Cmd Markdown
- [ ] Integrate Markmap into Cmd Markdown
- [ ] Integrate https://github.com/microsoft/markitdown
- [ ] sync ./note-taking to zybuluo.com
- [ ] production-uwsgi.ini and production-logging.ini are duplicated for most content. after that, search "production.ini" and "development.ini" in git repo to make sure they have been renamed to something like "production-uwsgi.ini", also for development-uwsgi.ini and development-logging.ini
- [ ] There is a visible bar showing in preview area in chrome and edge, need to remove it.
- [ ] Upgrade new softwares based on docs on my local Ubuntu 20.04 and make sure https://locahost/ is able to start and reduce the deploy.py in my git repo for productproject
- [ ] Upgrade python2 to python3 and other libs like SQLAlchemy and Pyramid.
- [ ] When updating the version of NW.js make sure it works in offline mode, this issue is reported here: https://github.com/nwjs/nw.js/issues/3361
- [ ] Mobile app for zybuluo.com
- [x] [[#Integrate Chatgpt to detect and block bad note when publishing]]
- [x] [[#Make the latest mermaid work on zybuluo]]
- [x] clear up password auto-fill when publishing note
- [x] Move everything from my Ubuntu Desktop to docker container
- [x] Support latest Mermaid syntax like https://mermaid.live
- [x] Check help doc link, Mermaid link is invalid for example
- [x] Complete to support flowchart.js 1.18.0
- [x] Move productproject from bitbucket to github
- [x] Retire ~/docker/ubuntu_install_guide.sh
- [x] Retire ~/productproject/SETUP.markdown
- [x] dockerize the entirement runtime env to docker based on Ubuntu 22.04, handle [[#Fix Plan for Zybuluo]] below and unify them in a single doc somewhere.
- [x] git clone again productproject from github in production.
- [x] not allowing qiniu referer empty blocks the traffic to download installation files, but it also makess pictures blocked in downloaded pdf, resolve this by introduce client.zybuluo.com which does not allow empty referer anymore.
- [x] database in mysql docker takes 130GB now, the data is coming from production. if we need that much data, can we re-use the small dataset in home-pc

## Integrate AI to Cmd Markdown

### BD: 2024-12-22

- [ ] release steps for new tables:
    - [ ] alter `users table` to add new columns: check ~/docker/nvim_db_ui/mysql-dev/maintenance.ddl
    - [ ]source ~/devenv/bin/activate
    - [ ] cd ~/productproject/ZuoYeProject
    - [ ] python setup.py develop (optional: if your virtualenv is not up to date, run this)
    - [ ] initialize_ZuoYeProject_db development-uwsgi.ini (optional: for creating new table `user-ai-tokens` table here) or replace it with production-uwsgi.ini for production
    - [ ] for local: backup mysql-data.tar.gz one more time to include new tables created above.

- [ ] A workable AI popup and available dropdown when pressing space in ACE editor
- [ ] Integrate with gpt-4o-mini for questions asked from AI popup in ACE editor, insert answer to current cursor position in the ACE editor
- [ ] Complete ai-dropdown menu items
    - [ ] Complete Writing
    - [ ] Complete Summary
    - [ ] Complete Diagram
    - [ ] Complete Table
    - [ ] Complete Coding
    - [ ] Complete Formula
    - [ ] Complete Draft Email
    - [ ] Complete Draft Outline
    - [ ] Complete Draft Meeting
    - [ ] Complete Draft anything
    - [ ] Complete Act as Translator
    - [ ] Complete Act as Code Analysis
    - [ ] Complete Act as Linux Termminal
    - [ ] Complete Act as Math Teacher
    - [ ] Complete Act as anything
    - [ ] Complete Recent
- [ ] A sub system to check question history and count tokens for each question, set threshold for AI calls
- [ ] A new chat-mode page, and result can be easily added to ace editor


## Fix Plan for Zybuluo

```
Problem: For MySql 8.0.36, SQLAlchemy 0.7.9 need to be updated like below:

$ vi ~/devenv/local/lib/python2.7/site-packages/SQLAlchemy-0.7.9-py2.7-linux-x86_64.egg/sqlalchemy/dialects/mysql/base.py

Search @@tx_isolation, replace it with @@transaction_isolation

def get_isolation_level(self, connection):
    cursor = connection.cursor()
    # jiawzhang update this for mysql 8
    # cursor.execute('SELECT @@tx_isolation')
    cursor.execute('SELECT @@transaction_isolation')
    val = cursor.fetchone()[0]
    cursor.close()
return val.upper().replace("-", " ")
```

## Cost of Aliyun

```
280 RMB per month
数据盘：普通云盘/dev/xvdb40GB
实例：1核 4GB共享标准型 s1系列 I
I/O 优化实例：非 I/O 优化实例
系统盘：普通云盘/dev/xvda20GB
带宽：5Mbps按固定带宽
CPU：1核
可用区：华东 1 可用区 G
操作系统：Ubuntu 12.04 64位Linux64位
内存：4GB
地域：华东 1
网络类型：专有网络
```

``` 
420 RMB per month
RDS规格：1 核 2GB（通用型）
数据库类型：MySQL
数据库版本号：5.5
公网流量：按实际使用流量每日扣费
系列：高可用版
地域：华东 1（杭州）
存储空间：200GB
存储类型：本地SSD盘
```

## How to restore from aliyun mysql
```
https://help.aliyun.com/zh/rds/apsaradb-rds-for-mysql/restore-the-data-of-an-apsaradb-rds-for-mysql-instance-from-a-physical-backup-file-to-a-self-managed-mysql-database

***.tar.gz is extracted to mysql_bkdata folder
mysql_newdata is a new folder to store backup data from aliyun.(added this folder /home/jiawzhang/Downloads/zybuluo_mysql/mysql_newdata to /etc/mysql/my.cnf)


helpful link while migration:
https://askubuntu.com/questions/758898/mysql-wont-start-after-changing-the-datadir-14-04-mysql-5-7/795710#795710
https://stackoverflow.com/questions/6288103/native-table-performance-schema-has-the-wrong-structure
https://stackoverflow.com/questions/16556497/mysql-how-to-reset-or-change-the-mysql-root-password
https://docs.rackspace.com/docs/reset-a-mysql-root-password

```

## Done Tasks

### Integrate Chatgpt to detect and block bad note when publishing

#### BD: 2024-11-22 CD: 2024-12-27

- [x] Integrate Chatgpt API
- [x] build a prompt file system to store all kinds of prompt including the one to detect and block the bad note
- [x] When publishing note, call Chatgpt API with prompt to detect and block the bad note, also mark the note and in some way warn me to review
- [x] A uesr notes management system to review and approve bad notes manually

### Make the latest mermaid work on zybuluo

#### BD: 2024-08-30 CD: 2024-09-19

- [x] update mermaid to latest version from 6.0.0 to 11.2.0, work for web page.
- [x] Make pdf download work for latest mermaid charts, change docker file and codes to support `puppeteer` to convert html to pdf server side
- [x] search `TODO:` in ~/productproject/
- [x] Release notes on the 15th update for Cmd Markdown
- [x] update ~/productproject/ZuoYeProject/zuoyeproject/static/editor/md-help.markdown
- [x] mermaid js is not owned by zybuluo if we need to own it. what's the speed of the CDN for mermaid? (it's now owned by zybuluo)
- [x] remove pdfkit and wkhtmltopdf,since we are now using puppeteer to replace it to convert html to pdf
- [x] update ~/productproject/ZuoYeProject/zuoyeproject/static/editor/welcome-cmd.markdown
- [x] make a new mdeditor.html after mermaid and flowchart changes
- [x] deploy mermaid changes to prod

### Restore Zybuluo

#### 2024-03-05

- [x] install mysql client based on 8.0.36 and update doc
- [x] Upgrade local Ubuntu to 20.04
- [x] restore prod mysql data to local mysql and upgrade local mysql to 8.0.36
- [x] Upgrade EC2 to t3a.medium and convert on demand instance to reserved instance(t3a.medium) to save cost (\$243 per year)
- [x] Your PostgreSQL 11 and MySQL 5.7 databases will be automatically enrolled into RDS Extended Support on February 29, 2024. To avoid the increase in charges due to RDS Extended Support, we recommend upgrading your databases to a newer major engine version before February 29, 2024.
- [x] Upgrade RDS mysql 5.7.44 to 8.0.36 and t3.small to t3.medium (requires code changes) and purchase reserved instance(\$670 per year)
- [x] make sure 'grunt -version' is same for both local and ec2 server, so that *.min.js have the same version after run grunt in mdeditor.html.
- [x] Make desktop version work
- [x] replace support@zybuluo.com and sales@zybuluo.com with ghosert@gmail.com
- [x] make fullscreen work when client starts up
- [x] clicking on link with target=_blank to open default browser, especially for download file. (https://github.com/nwjs/nw.js/issues/6506)
- [x] loading pictures
- [x] uploading pictures
- [x] add new google analitics
- [x] export PDF
- [x] email to find back password
- [x] chinese character is not working? check create ddl in SETUP.markdown
- [x] make sure no errors in backend logs.
- [x] make https work
- [x] make fontawesome-webfont.woff work
