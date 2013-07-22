# coding: utf-8

'''
Created on Jul 11, 2013

@author: jiawzhang
'''

import os
import re
import sys
import traceback

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
        if params:
            if params.count(',') >= 3:
                return "\"{0}\", new Object[]{{{1}}}".format(newString, params[2:])
            else:
                return "\"{0}\"{1}".format(newString, params)
        else:
            return parameter_string
    
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
        
def log4j_handler(pre_log, content, filename):
    # clean up jakarta commons logging if any first.
    content = re.sub(r'import org.apache.commons.logging.Log;\n', '', content)
    content = re.sub(r'import org.apache.commons.logging.LogFactory;\n', '', content)

    content = re.sub(r'import org.apache.log4j.LogSF;\n', '', content)
    content = re.sub(r'import org.apache.log4j.Logger', 'import org.slf4j.Logger;\nimport org.slf4j.LoggerFactory', content)
    content = re.sub(r'Logger\s*?.\s*?getLogger', 'LoggerFactory.getLogger', content)
	# LogSF.debug(log,"PDF parsing result errorCodes={} eventId={} listingId={} sellerId={}" , errorCodes , eventId.toString()
    def replace_LogSF(matches):
        prefix_matches = re.search(r'^(.*?)(".*)$', matches.group(2), flags = re.DOTALL)
        if prefix_matches:
            comma_count = prefix_matches.group(1).count(',')
            if comma_count == 1:
                log_params = prefix_matches.group(2)
                return '{0}.{1}({2});'.format(pre_log, matches.group(1), log_params)
            elif comma_count == 2:
                exception_params = re.search(r',\s*?(\S+)\s*?,', prefix_matches.group(1)).group(1)
                log_params = prefix_matches.group(2)
                return '{0}.{1}({2}, {3});'.format(pre_log, matches.group(1), log_params, exception_params)
        return matches.group(0)

    content = re.sub(r'LogSF\.(debug|info|warn|error|fatal)\((.*?)\);', replace_LogSF, content, flags = re.DOTALL)
    content = re.sub(r'({0})'.format(pre_log) + r'\.(debug|info|warn|error|fatal)\((.*?)\);', handle_finding, content, flags = re.DOTALL)
    content = re.sub(pre_log + r'\.fatal\(', pre_log + r'.error(', content, flags = re.DOTALL)
    if re.search(r'LogSF\.(debug|info|warn|error|fatal)\(', content):
        print 'LogSF in this file fail to clean completely, check it: {0}'.format(filename)
        print
        # return None means no changes.
        return None
    return content
        
def commons_logging_handler(pre_log, content):
    # clean up log4j if any first.
    content = re.sub(r'import org.apache.log4j.LogSF;\n', '', content)
    content = re.sub(r'import org.apache.log4j.Logger;\n', '', content)

    content = re.sub(r'import org.apache.commons.logging.Log', 'import org.slf4j.Logger', content)
    content = re.sub(r'import org.apache.commons.logging.LogFactory', 'import org.slf4j.LoggerFactory', content)
    content = re.sub(r'\bLog\b.*?LogFactory\s*?.\s*?getLog', 'Logger ' + pre_log + ' = LoggerFactory.getLogger', content)
    content = re.sub(r'({0})'.format(pre_log) + r'\.(debug|info|warn|error|fatal)\((.*?)\);', handle_finding, content, flags = re.DOTALL)
    content = re.sub(pre_log + r'\.fatal\(', pre_log + r'.error(', content, flags = re.DOTALL)
    return content

def slf4j_handler(pre_log, content):
    content = re.sub(r'({0})'.format(pre_log) + r'\.(debug|info|warn|error|fatal)\((.*?)\);', handle_finding, content, flags = re.DOTALL)
    return content

def handler(filename):
    content = None
    file_content = None
    with open(filename, 'r') as input_file:
        content = input_file.read()
        file_content = content

    # pre-check
    # NOTICE jiawzhang: Uncomment this check if you want to mannually fix the issue, you could fix the issue by running 'mvn clean install -DskipTests'
    #pre_matches = re.search(r'\.(debug|info|warn|error|fatal)\(([^"]*?)\);', content, flags = re.DOTALL)
    #if pre_matches:
    #    print pre_matches.group(0)
    #    print 'manually check this file to see why there is no "" in debug/info/warn/error/fatal clause: {0}'.format(filename)
    #    return

        
    # for slf4j
    matches = re.search(r'Logger\s+(\S+?)\s*=\s*LoggerFactory\s*?.\s*?getLogger', content)
    if matches:
        pre_log = matches.group(1)
        changed_content = slf4j_handler(pre_log, content)
        if changed_content:
            content = changed_content
        else:
            return

    # for log4j
    matches = re.search(r'Logger\s+(\S+?)\s*=\s*Logger\s*?.\s*?getLogger', content)
    if matches:
        pre_log = matches.group(1)
        changed_content = log4j_handler(pre_log, content, filename)
        if changed_content:
            content = changed_content
        else:
            return

    # for Jakarta commons logging
    matches = re.search(r'Log\s+(\S+?)\s*=\s*LogFactory\s*?.\s*?getLog', content)
    if matches:
        pre_log = matches.group(1)
        changed_content = commons_logging_handler(pre_log, content)
        if changed_content:
            content = changed_content
        else:
            return


    # Check whether the changes work
    def check_changes_failed(content):
        matches = re.search(r'.*?\.(debug|info|warn|error|fatal)\(.*?\+.*?\);', content)
        if matches:
            if not check_comma(matches.group(0)):
                return matches
        return None
        
    if check_changes_failed(content):
        # replace the non-standard clause getLog().debug();
        content = re.sub(r'(getLog\(\))\.(debug|info|warn|error|fatal)\((.*?)\);', handle_finding, content, flags = re.DOTALL)
        content = re.sub(r'getLog\(\)\.fatal\(', r'getLog().error(', content, flags = re.DOTALL)

        content = re.sub(r'(log)\.(debug|info|warn|error|fatal)\((.*?)\);', handle_finding, content, flags = re.DOTALL)
        content = re.sub(r'log\.fatal\(', r'log.error(', content, flags = re.DOTALL)

        content = re.sub(r'(logger)\.(debug|info|warn|error|fatal)\((.*?)\);', handle_finding, content, flags = re.DOTALL)
        content = re.sub(r'logger\.fatal\(', r'logger.error(', content, flags = re.DOTALL)

        failed_matches = check_changes_failed(content)
        if failed_matches:
            print failed_matches.group(0)
            print 'manually check this file to see whether all the + in log clause has been cleaned or not: {0}'.format(filename)
            print
        
    # Write to file
    if file_content != content:
        file_depot_path = '//' + re.search(r'depot.*$', filename).group(0)
        os.system('p4 edit {0}'.format(file_depot_path))
        with open(filename, 'w') as output_file:
            output_file.write(content)
        pass
    else:
        # print 'No logging in this file, skip handling: {0}'.format(filename)
        pass

if __name__ == '__main__':
    depot_path = None
    depot_matches = re.search(r'depot.*$', os.getcwd())
    if depot_matches:
        depot_path = '//' + depot_matches.group(0)
    else:
        print 'Run this py in peforce depot path.'
        sys.exit(1)
    # Recursivly find out java file and check them.
    for root, dirs, files in os.walk(os.getcwd()):
        # NOTICE jiawzhang: skip gen2 codes
        if re.search(r'{0}'.format(os.getcwd() + os.sep + 'gen2'), root):
            continue
        if files:
            for file in files:
                filename = root + os.sep + file
                if re.search(r'\.java$', filename):
                    try:
                        # print 'handling {0}'.format(filename)
                        handler(filename)
                    except Exception as e:
                        print 'error happens when handling file {0}'.format(filename)
                        print traceback.format_exc()
            
