# coding: utf-8

'''
Created on Jul 11, 2013

@author: jiawzhang
'''

import os
import re

def check_comma(debug_clause):
    # skip the form like: log.debug("xxx", yy); or log.debug(xxx, yyy) for existing slf4j and something like log.error("error message", e);
    # and make sure info.debug("xx,xx"); will not be counted.
    try:
        commaPosition = 0
        while True:
            commaPosition = debug_clause.index(',', commaPosition)
            num_quotation_mark = debug_clause[0:commaPosition].count(r'"')
            if num_quotation_mark % 2 == 0:
                return True
            commaPosition = commaPosition + 1
    except:
        return False

def handle_parameter_string(parameter_string):
    isParamOnly = True
    newString = ''
    params = ''
        
    if check_comma(parameter_string):
        return parameter_string
            
    fields = re.split(r'\+', parameter_string)
    for field in fields:
        field = re.sub(r'[\r\n\t]', '', field, flags = re.DOTALL).strip()
        matches = re.search(r'^"(.*)"', field)
        if matches:
            newString = newString + matches.group(1)
            isParamOnly = False
        else:
            newString = newString + '{}'
            params = params + ', ' + field
    if isParamOnly:
        return parameter_string
    else:
        return "\"{0}\"{1}".format(newString, params)
    
def handle_finding(finding):
    # print
    # print finding.group(0)
    variable = finding.group(1)
    debug_level = finding.group(2)
    parameter_string = finding.group(3)
    parameter_string = handle_parameter_string(parameter_string)
    handled_debug_clause = '{0}.{1}({2});'.format(variable, debug_level, parameter_string)
    # print handled_debug_clause
    return handled_debug_clause
        
def log4j_handler(content, filename):
    content = re.sub(r'import org.apache.log4j.Logger', 'import org.slf4j.Logger;\nimport org.slf4j.LoggerFactory', content)
    content = re.sub(r'Logger.getLogger', 'LoggerFactory.getLogger', content)
    pre_log = re.search(r'Logger\s+(\S+?)\s*=\s*LoggerFactory.getLogger', content).group(1)
	# LogSF.debug(log,"PDF parsing result errorCodes={} eventId={} listingId={} sellerId={}" , errorCodes , eventId.toString()
    def replace_LogSF(matches):
        if matches.group(0).count(',') == 1:
            return '{0}.{1}("'.format(pre_log, matches.group(1))
        else:
            return matches.group(0)
    content = re.sub(r'LogSF\.(debug|info|warn|error|fatal)\(.*?,.*?"', replace_LogSF, content, flags = re.DOTALL)
    content = re.sub(r'({0})'.format(pre_log) + r'\.(debug|info|warn|error|fatal)\((.*?)\);', handle_finding, content, flags = re.DOTALL)
    content = re.sub(pre_log + r'\.fatal\(', pre_log + r'.error(', content, flags = re.DOTALL)
    if re.search(r'LogSF\.(debug|info|warn|error|fatal)\(', content):
        print 'LogSF in this file fail to clean completely, check it: {0}'.format(filename)
        print
    return content
        
def commons_logging_handler(content):
    content = re.sub(r'import org.apache.commons.logging.Log', 'import org.slf4j.Logger', content)
    content = re.sub(r'import org.apache.commons.logging.LogFactory', 'import org.slf4j.LoggerFactory', content)
    pre_log = re.search(r'Log\s+(\S+?)\s*=\s*LogFactory.getLog', content).group(1)
    content = re.sub(r'\bLog\b.*?LogFactory.getLog', 'Logger ' + pre_log + ' = LoggerFactory.getLogger', content)
    content = re.sub(r'({0})'.format(pre_log) + r'\.(debug|info|warn|error|fatal)\((.*?)\);', handle_finding, content, flags = re.DOTALL)
    content = re.sub(pre_log + r'\.fatal\(', pre_log + r'.error(', content, flags = re.DOTALL)
    return content

def handler(filename):
    content = None
    file_content = None
    with open(filename, 'r') as input_file:
        content = input_file.read()
        file_content = content
        
    # for log4j
    if re.search(r'import org.apache.log4j.Logger', content):
        content = log4j_handler(content, filename)
    # for Jakarta commons logging
    elif re.search(r'import org.apache.commons.logging.Log', content):
        content = commons_logging_handler(content)
        
    # replace the non-standard clause getLog().debug();
    content = re.sub(r'(getLog\(\))\.(debug|info|warn|error|fatal)\((.*?)\);', handle_finding, content, flags = re.DOTALL)
    content = re.sub(r'getLog\(\)\.fatal\(', r'getLog().error(', content, flags = re.DOTALL)
        
    # Check whether the changes work
    matches = re.search(r'.*?\.(debug|info|warn|error|fatal)\(.*?\+.*?\);', content)
    if matches:
        if not check_comma(matches.group(0)):
            print matches.group(0)
            print 'manually check this file to see whether all the + in log clause has been cleaned or not: {0}'.format(filename)
            print
        
    if file_content != content:
        with open(filename, 'w') as output_file:
            output_file.write(content)

if __name__ == '__main__':
    # Recursivly find out java file and check them.
    for root, dirs, files in os.walk(os.getcwd()):
        if files:
            for file in files:
                filename = root + os.sep + file
                if re.search(r'\.java$', filename):
                    try:
                        # print 'handling {0}'.format(filename)
                        handler(filename)
                    except Exception as e:
                        print 'error happens when handling file {0}'.format(filename)
                        print e
            
