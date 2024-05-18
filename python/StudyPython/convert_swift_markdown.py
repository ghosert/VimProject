#encoding: utf-8

"""
This py will be used to convert the official <The Swift Programming Language> located at 

https://raw.githubusercontent.com/muxuezi/iOS-and-OS-X-apps-Swift-Programming-Language/master/The%20Swift%20Programming%20Language%20for%20iOS%26OS.md

The converter will be mainly focus on converting below code snippet:

  * `var s = 1`

    TO

```
var s = 1
```


"""

import re

file_content = ''
block_begin = False
with open('./doc', 'r') as file:
    for line in file:
        matched = re.search(r'^\s{2}\*\s`(.*?)`$', line)
        if matched:
            line = matched.group(1)
            if block_begin == False:
                block_begin = True
                line = '```\n' + line
            else:
                line = '\n' + line
        else:
            if block_begin == True:
                block_begin = False
                line = '\n```\n' + line

        file_content = file_content + line

with open('./newdoc', 'w') as file:
    file.write(file_content)

