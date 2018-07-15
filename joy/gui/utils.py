import os

from dulwich.errors import NotGitRepository
from dulwich.repo import Repo


def init_home():
  '''
  Find and initialize the Joy home directory and repository.
  '''
  JOY_HOME = os.environ.get('JOY_HOME')
  if JOY_HOME is None:
    JOY_HOME = os.path.expanduser('~/.joypy')
    if not os.path.isabs(JOY_HOME):
      JOY_HOME = os.path.abspath('./JOY_HOME')
    #print 'JOY_HOME=' + JOY_HOME

  if not os.path.exists(JOY_HOME):
    #print 'creating...'
    os.makedirs(JOY_HOME, 0700)
    #print 'initializing git repository...'
    repo = Repo.init(JOY_HOME)

  else:  # path does exist
    try:
      repo = Repo(JOY_HOME)
    except NotGitRepository:
      #print 'initializing git repository...'
      repo = Repo.init(JOY_HOME)
    #else:
      #print 'opened git repository.'
  return JOY_HOME, repo


class FileFaker(object):

  def __init__(self, T):
    self.T = T

  def write(self, text):
    self.T.insert('end', text)
    self.T.see('end')

  def flush(self):
    pass
