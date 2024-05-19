# Zybuluo Improvement

## TODO Tasks

- [ ] dockerize the entirement runtime env to docker based on Ubuntu 22.04
- [ ] Move productproject from bitbucket to github
- [ ] sync ./note-taking to zybuluo.com
- [ ] Integrate ChatGPT with Cmd Markdown
- [ ] Integrate Markmap into Cmd Markdown
- [ ] Check help doc link, Mermaid link is invalid for example
- [ ] copy paste a vim guide for cmd markdown
- [ ] Don't allow the customer to refresh the read count forever
- [ ] production-uwsgi.ini and production-logging.ini are duplicated for most content. after that, search "production.ini" and "development.ini" in git repo to make sure they have been renamed to something like "production-uwsgi.ini"
- [ ] There is a visible bar showing in preview area in chrome and edge, need to remove it.
- [ ] check if celery keeps adding queues by running "sudo rabbitmqctl list_queues"
- [ ] Upgrade new softwares based on docs on my local Ubuntu 20.04 and make sure https://locahost/ is able to start and reduce the deploy.py in my git repo for productproject
- [ ] Feature enhancement to detect bad note realtime and block too many registered users from the same ip to post too many notes
- [ ] window.applicationCache has been deprecated and zybuluo.com is showing js error due to this.
- [ ] clear up password auto-fill when publishing note
- [ ] Send emails to all the customer ask them to come back.
- [ ] Move Cmd Guide to about menu? Or refine about menu?
- [ ] add Cmd Markdown formulate to official link (https://ericp.cn/cmd), ask permission from the original author.
- [ ] How to show cmd desktop client and its version in google analytics?
- [ ] Upgrade python2 to python3 and other libs like SQLAlchemy and Pyramid.
- [ ] When updating the version of NW.js make sure it works in offline mode, this issue is reported here: https://github.com/nwjs/nw.js/issues/3361
- [ ] buy baidu keywords and spread zybuluo anywhere
- [ ] update appcache by making mdeditor js/css changes to update ssl validation date, read details in SETUP.markdown

## Done Tasks

### Restore Zybuluo

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
