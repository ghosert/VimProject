# Zybuluo Improvement

## TODO Tasks

- [ ] Move everything from my Ubuntu Desktop to docker container
- [ ] database in mysql docker takes 130GB now, the data is coming from production. if we need that much data, can we re-use the small dataset in home-pc
- [ ] dockerize the entirement runtime env to docker based on Ubuntu 22.04, handle [[#Fix Plan for Zybuluo]] below and unify them in a single doc somewhere.
- [ ] git clone again productproject from github in production.
- [ ] Check help doc link, Mermaid link is invalid for example
- [ ] Feature enhancement to detect bad note realtime and block too many registered users from the same ip to post too many notes
- [ ] clear up password auto-fill when publishing note
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
- [ ] Integrate ChatGPT with Cmd Markdown
- [ ] Integrate Markmap into Cmd Markdown
- [ ] sync ./note-taking to zybuluo.com
- [ ] production-uwsgi.ini and production-logging.ini are duplicated for most content. after that, search "production.ini" and "development.ini" in git repo to make sure they have been renamed to something like "production-uwsgi.ini"
- [ ] There is a visible bar showing in preview area in chrome and edge, need to remove it.
- [ ] Upgrade new softwares based on docs on my local Ubuntu 20.04 and make sure https://locahost/ is able to start and reduce the deploy.py in my git repo for productproject
- [ ] Upgrade python2 to python3 and other libs like SQLAlchemy and Pyramid.
- [ ] When updating the version of NW.js make sure it works in offline mode, this issue is reported here: https://github.com/nwjs/nw.js/issues/3361
- [x] Move productproject from bitbucket to github


## Fix Plan for Zybuluo

Open ~/productproject/SETUP.markdown ~/productproject/ubuntu_install_guide.sh with this doc to restore zybuluo.com runtime environment.
~/docker/zybuluo/devenv.tar.gz contains everything for virtualenv

```
Problem:
ImportError: cannot import name_remove_dead_weakref
Reason
This is because the current Python installation is somehow screwed up. In my case the mess was caused by an upgrade from Ubuntu 16.04 to 18.04.

Solution
With Virtual Environment
In the best case you were already using a virtual environment (this was my case). The solution here would be to recreate/setup your venv again (step-by-step):

a. mv ~/devenv/ ~/devenv_backup
b. virtualenv --no-site-packages devenv
c. source ~/devenv/bin/activate
d. cp ~/devenv_backup/lib/python2.7/site-packages/* ~/devenv/lib/python2.7/site-packages/
```

```
Problem: Fix rabbitmq issue(fail to start for 18.04. For 20.04 just "sudo apt install rabbitmq-server")

1. uninstall rabbitmq: sudo apt-get purge rabbitmq-server
2. follow this link to install: https://medium.com/@thucnc/how-to-install-rabbitmq-on-ubuntu-18-04-d002a347764e

If this solution works, add it to ubuntu_install_guide.sh
```

```
Problem: Fix python mysql problem:

source ~/devenv/bin/activate
easy_install MySQL-python==1.2.5
vi ~/devenv/local/lib/python2.7/site-packages/easy-install.pth, remove "MySQL-python"
remove ~/devenv/local/lib/python2.7/site-packages/MySQL-python
wget https://bootstrap.pypa.io/get-pip.py
python get-pip.py
pip install mysqlclient   (you may need to first `pip uninstall mysqlclient && sudo apt install default-libmysqlclient-dev`)

For Ubuntu 20.04, Chinese character problem, go to ~/devenv/lib/python2.7/site-packages/MySQLdb/connections.py, update def set_character_set(self, charset) as below:

def set_character_set(self, charset):
    # jiawzhang changed for Ubuntu 20.04, https://github.com/PyMySQL/mysqlclient/issues/504
    py_charset = _charset_to_encoding.get(charset, charset)
    super(Connection, self).set_character_set(charset)
    self.encoding = py_charset
    
    
For MySql 8.0.36, SQLAlchemy 0.7.9 need to be updated like below:

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

```
Problem: Fix ImportError: cannot import name _uuid_generate_random
 pip uninstall Celery
 pip uninstall amqp
 pip uninstall kombu
 pip install Celery=3.1.18
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
