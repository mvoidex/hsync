# hsync

Tool I'm using to sync directories. The key feature is that it can ask git for modifications and therefore comparing local changes to remote directory (without commiting it to git of course) is fast since it doesn't compare all files.

```
hsync — synchronize folders

Usage: hsync.exe src dst [--git] [-u|--untracked] [-n|--noaction] [-c|--combine]
                 [-m|--mirror] [--newest] [--prefer ARG] [--ignore] [-d|--diff]
                 [-e|--exclude ARG] [-v|--verbose]

Available options:
  -h,--help                Show this help text
  src                      source, either local path or remote [host]:[path]
  dst                      destination, either local path either remote
                           [host]:[path]
  --git                    ask git for modifications
  -u,--untracked           show untracked files, git-only
  -n,--noaction            don't perform any actions, just show what to be done
  -c,--combine             combine mode, can produce conflicts
  -m,--mirror              mirror mode: `dst` will become in same state as
                           `src`, i.e. new files will be deleted, unexistant
                           will be created etc.
  --newest                 resolving: prefer newest
  --prefer ARG             resolving: prefer 'left' or 'right'
  --ignore                 resolving: ignore conflict, don't do anything for
                           them
  -d,--diff                show diff, don't performs any actions
  -e,--exclude ARG         exclude directories and files
  -v,--verbose             verbose output

synchronize destination folder state with source one
it can ask git for modifications with no need to fully traverse directories

there are two main modes of syncing:
    mirror - destination folder will become in same state as source (created files will be deleted, modifications will be reverted etc.)
    combine - try to merge folder states, this can produce conflicts, which have to be resolved:
        newest - prefer file with latest modification time
        ignore - don't do anything for conflicted files
        prefer left|right - prefer source of destination file

examples:

    hsync src dst --mirror
        mirror-copy src to dst

    hsync src dst --combine --newest
        copy src to dst, overwrites older files, but doesn't touch newest ones; doesn't delete anything

    hsync src dst --git --mirror
        mirror-copy src to dst, also restores deleted files, reverts renaming etc.
        but NOTE, hsync doesn't interact with git (doesn't invoke git's add, rm, mv commands) it only copies/deletes files```
```