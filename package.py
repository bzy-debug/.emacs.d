import tomllib
import os
import sys
import subprocess as sp

with open('./package.toml', 'rb') as f:
  metadata = tomllib.load(f)
  emacs_dir = os.path.expanduser(metadata['emacs']['emacs_dir'])
  config_dir = metadata['emacs']['config_dir']
  packages_dir = metadata['emacs']['packages_dir']
  del metadata['emacs']

def install():
  for key in metadata:
    repo = metadata[key]['repo']
    clone_path = os.path.join(emacs_dir, packages_dir, key)
    if os.path.isdir(clone_path):
      continue
    sp.run(['git', 'clone', '--depth', '1', repo, clone_path])

def update(repo):
  print(f'Update {repo}')
  clone_path = os.path.join(emacs_dir, packages_dir, repo)
  os.chdir(clone_path)
  sp.run(['git', 'pull', '--rebase'])

def updateAll():
  for key in metadata:
    update(key)

if __name__ == '__main__':
  sub_command = sys.argv[1]
  match sub_command:
    case 'install':
      install()
    case 'update':
      repo = sys.argv[2]
      if repo == 'all':
        updateAll()
      else:
        update(repo)
