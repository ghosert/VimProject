# How does Python benefit the Java based StubHub ?

Since 2006, Python is pretty popular from then on. You could see more and more startup companies choose Python as the primary language when they start their business, for example:

**Netflix** - TV Show & Movies online company  
**Dropbox** - The most popular file synchronization and sharing tools  
**YouTube** - Sharing videos online  
**Disqus** - online discussion and commenting service  
**OpenStack** - an all-open-source, all-Python infrastructure for building public and private clouds

While the startup companies get more and more benefits from this elegant and neat language as the infrastructure to support their fast growing business, I'm thinking how we, StuhbHub - a Java ecosystem based company could benefit from it either to save our working hours and enhance the productivity significantly, I will explain what and why is Python and show you the Python solutions in our daily work.

## What and Why is Python ?

Python is an interpreted, object-oriented dynamic language, and of cause like Java, it's cross-platform language as well. Compared with the traditional mainstream language like Java/C++, programmers adore it because of the reasons below:

### 1. Python is versatile language

As we know, every language has its own advantage or disadvantage, like People will write C++ codes for games on Windows operation system, but no one will build a website by writing C++. The good news is that you could leverage Python for almost any task such as: web application, GUI desktop application, linux script or any other handy tools, and on the top of that, as "glue language" you could even invoke the other language like Java/C++ in your python code, that means your existing code base could be reused.

### 2. Python is more productivity

General speaking, the distinct difference when we talking about Python and Java is that as the dynamic language, there is no compilation step necessary. Under this hood, that means "productivity".

Remember how we verify a modification in Java code ? Especially in StubHub, we own a extremely large code base. 

1. Modify your Java code. (1 min)
2. ant/maven to compile your Java code to byte code. (5 mins)
3. Restart JBoss/Tomcat to deploy your application. (5 mins)
4. Open your browser and see the changes.

Here is the pain point: suppose you have a bug fix takes you 1 minute, but you have to wait at least 10 minutes to see the changes in your browser, the worse is that the fix is not working, therefore, another 10 minutes just for build/deploy is awaiting for you.

While you are working with Python, it's pretty easier.

1. Modify your Python code. (1 min)
2. F5 to refresh your browser to see the changes.

**Congrats! You are saving 10 minutes for each iteration on your code change**.

Consider how many times in each day, each developer will have the code change and how many developers in a large organization like StubHub, you could calculate how many working hours you could save in all. That is huge and more than your imagine.

### 3. Python is elegant, neat and compact

And there is another major advantage. The syntax of Python is pretty cool, I had a chance to implement the same function with both Python and Java twice, it takes me only 50% code line to do the same thing by Python compared with Java. Base on this, that's why people like to write pseudo code to verify ideas or implement a rapid prototype by writing Python code. It's quicker to let you know whether your ideas or prototypes are doable or viable, and after that, you could then rewrite your codes in Java for production. That is better than, from the very beginning, you start codes with Java but find out your prototype is not viable.

## Python Stories In StubHub

StubHub go with the technical route like below:

Generation 1: coldfusion  
Generation 2: Java, flow based framework.  
Generation 3: Java, Tapestry + Spring + Hibernate, all kinds of modern technical framework.

You could see the whole technical ecosystem thrives based on Java. Sometimes it's impossible as the engineer you could convince the team or architects to discard all the existing code base or convert the underneath fundamentals from Java to another one, but still, there is some room you could get things done better and quicker, let me show you the stories from my personal working scenarios in StubHub.

### Story 1: Handle the Java source codes by Python

In 2011, StubHub Tech Team kicked off a one-week activity named **fixit**, the activity required all the developers try to write as many as possible test cases for existing codes to enhance the overall test coverage within one week. But before that, we need to mark some test cases as "broken" first, because if there is any test case failed, the analysis tool can't produce the coverage report.

The point is that it's easy to mark the test cases as "broken": just add @Test annotations with broken attribute like below:

From
```java
public class SomeTest {
...
}
```
To
```java
@Test(groups = {"broken"})
public class SomeTest {
...
}
```

But there are hundreds of test cases source codes in StubHub code base.  That means you have to find them all first, check one case out from repository, modify the source code, check in back the changes and then repeat the steps for the next case until hundreds of cases have been handled.

This is boring. And I think it's Python could handle file operations repeatedly well not me. Thus I have the Python code snippet below to handle it automatically:

```python
def start(path_name):
    for root, dirs, files in os.walk('src/' + path_name):
        if files:
            for file in files:
                filename = root + '/' + file
                print 'filename: ' + filename
                os.system('p4 edit //depot/project/pb_fixit_2011/gen31/test/' + filename)
                with open(filename, 'r+') as javafile:
                    fileContent = javafile.read()
                    matcher = re.search(r'@Test([\w\W]*?)public class', fileContent)
                    if not matcher:
                        fileContent = re.sub(r'public class', '@Test(groups = {"broken"} )\npublic class',  fileContent)
                        javafile.seek(0) # return to 0 file position
                        javafile.write(fileContent)
```
Just 14 lines, the codes walk through the given path and check out files from Perforce by `p4 edit ...`, read file content from Java source file, use the regular expression to mark the test case as broken and then write back the changes to the source file. I suppose this script saved me one day work.

### Story 2: Testing on 3rd party API

In 2012, I joined a project named Gift Card, there is a 3rd party technical partner named Black Hawk who provides the redeem/reversal money Web Service APIs. At the very beginning of this project, they want to make sure the API calls from StubHub testing server is accessible to Black Hawk server. Since it's just a verification, it's not that serious to write down the formal, exception-well-handled Java codes, deploy the codes to somewhere we have the JRE installed and HttpClient libs there as the prerequisites and then begin to test APIs (By the way, Python is pre-installed for almost all the linux distributions as the fundamental). When I think it's not formal codes in the future it could be reused or maintained by others, Here is what Python could do for the same thing:

```python
from httplib2 import Http
def call_bh(url):
    try:
        http = Http()

        # Post request parameters
        body = """<?xml version="1.0" encoding="UTF-8"?> 
            <bhnums:request 
            ......
            </bhnums:request> 
        """
        headers = {'Content-type': 'text/xml;charset=UTF-8'}

        response, content = http.request(url, "POST", headers=headers, body=body)

        # Expected post response
        print content
    except Exception, e:
        print e
        print 'fail to call black hawk.'
    else:
        print 'success to call black hawk.'
```
Calling the service via Python is joyful. Although at first, it failed, But since the code is pretty short and readable, I just copied the Python snippet to the email and asked the 3rd party guys: "Is there anything wrong"? They replied me with some correction in the http request body, then I edit the codes on our testing server, tried again, it worked. There is no JRE/IDE/compilation/build/deployment here, just ssh to your linux server, edit your codes with vi/emacs editor and **Run !**.

### Story 3: Resend Rewards Emails to customers

In 2012, there was another project named Rewards which would give our users some discounts if they engaged this campaign. And when they engaged it, they would be supposed to receive a mail about the details of this campaign. Unfortunately, because of the environment issue, there were 1285 users fail to receive this mail so that they may not know how to get discounts, although they had already joined Rewards campaign. Thus, I was required to resend the mails to make up for this issue.

The task is emergent and valuable for the user if we could resend the mails as soon as possible. But if we go with the traditional Java codes, we need to have the SQLs to pick up the missing users from database first, invoke this SQL by JDBC or Hibernate, write down codes for resending email and figure out somewhere to build/deploy codes in production, and then roll back the deployment, since it's supposed to be used once. That is ugly and I could imagine, it at least takes us 2 days for development and deployment.

While choosing Python, the thing is easier. It's still some kind of script running on linux server. To avoid reading database, I could pick up the user id from database first and write them into the script as the list like this.

```python
users = [556483, 556480, 556477, 556379, 556378, 556471, 556374, 469686, 556369, 556466, 556365, 556364, 556462, 556460, 556362, 556360, 556456, ...]
```
It looks like 1285 user ids is a large amount,  but still it's acceptable they are all in a python list as the part of script, then let's resend mail to the users:

```python
def sendmail_to_all(users, url):
    for user in users:
        time.sleep(1)
        sendmail(user, url)
```
At last, just one night, I guess all our users got their missing rewards mails.

### Story 4: Recommendation Prototype

In 2010, one of my colleagues, who is a master at Mathematics, proposed a recommendation algorithm for StubHub, basically, the idea is that when the user is browsing the genre/event information on Stubhub, the system will guess what's the most interested genres/events the user maybe purchase as well. I think the chances are we need Python once again to build a rapid application and see whether his recommendation algorithm works or not.

So I took 15 minutes to discuss algorithm with him and try to understand the underneath magic rules, I was crystal after 15 minutes and started coding with Python.

First, I just want to make the prototype work for verifying algorithm, I'm not going to go with Java to do some server side changes, data modal changes, that's more like a formal project to me not a prototype. Thus, I think I could have a GUI desktop application with the built-in browser, people could operate the built-in browser and recommended information could be displayed on GUI application instead of browser, so that we avoid server side changes. Fortunately, It's not difficult for Python to build a GUI app.

Second, I need to process the historical order info from database to calculate the data, and then apply the algorithm. As you might know, Python is good at processing data as well.

Finally, two days later, with the unrelenting coding with Python, I derived a coarse prototype like below:

The left side of the GUI app is a built-in browser which is rendering the events of Los Angeles Lakes, and the right side of the GUI app is the recommended events which people may be interested based on the current event in the built-in browser. The top event of the recommended events is the match between 76ers and Lakers in 2010, guess you could tell me whether this make sense or not to NBA fans. :)

![此处输入图片的描述][1]


Let's just suppose the result is not that ideal, but at least within two days, we have the chance to know whether the algorithm is viable or not. This is even more important than we derive a good algorithm by Java within two weeks. The quicker you prove you are wrong, the quicker you could sift through a better algorithm. That's the point. What if your Java prototype fails? Instead of two days, you lost two weeks, the cost is expensive.


## The End Of The Python Stories

These are all my Python Stories in StubHub, maybe they are trivial but I believe the stories will go on continuously. The thing is that we are not talking about which one is better: Java or Python or C++, I don't care, I know only the more I could arm myself, the more powerful I could be, to tackle with all kinds of scenarios and not only work hard but also work smart.

This makes the life easier.

  [1]: https://lh6.googleusercontent.com/-adGChNMTZxM/UcuRJzYDQrI/AAAAAAAAAOU/iotr53MsUPE/s0/recomm_U.jpg "recomm_U.jpg"

