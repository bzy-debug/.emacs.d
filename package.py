import tomllib
import subprocess as sp

with open("./package.toml", "rb") as f:
  data = tomllib.load(f)
  config_dir = data['emacs']['config_dir']
  packages_dir = data['emacs']['packages_dir']
  del data['emacs']
  for key in data:
    repo = data[key]['repo']
    clone_path = f'{packages_dir}/{key}'
    sp.run(['git', 'clone', '--depth', '1', repo, clone_path])
