#! /usr/bin/env python
import os, sys
from os.path import *

def getRealDirPath(path):
  return dirname(realpath(path))

def execute(cmd):
  print cmd
  os.system(cmd)

if __name__ == '__main__':
  scriptpath = sys.argv[0]
  if len(sys.argv) < 2:
    print 'usage: %s <UnbalancedBranchDetection.jar args...>' % scriptpath
    exit(1)
  scriptDirPath = getRealDirPath(scriptpath)
  CAGE = dirname(dirname(scriptDirPath))
  SOOTJAR    = join(CAGE,'tools','soot-trunk.jar')
  JAVA7_HOME = join(CAGE,'tools','jdk1.7.0_79')
  RT_JAR     = join(JAVA7_HOME,'jre','lib','rt.jar')
  BASELIBS   = pathsep.join([RT_JAR, '.'])
  UNBALANCED_DETECTION_JAR = join(scriptDirPath,'UnbalancedBranchDetection.jar')
  args = ' '.join(sys.argv[1:])
  cmd = 'java -jar %s -soot-class-path %s %s' % \
    (UNBALANCED_DETECTION_JAR, BASELIBS, args)
  execute(cmd)
  

