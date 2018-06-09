### Git Notes
[Git documentation](https://git-scm.com/) had detailed descriptions of
the commands found here.

### dotfiles you can configured
#### .gitignore_global
Global to all repositories on your computer.  These are files git ignores when
you issue a command.

### .gitconfig
Use this file to configure alias to shorten git commands.

### .gitignore
Use this file to inform git there are repository specific files that should be
ignored.

### .ssh/config
git and ssh protocols use this file for aliases, user name, host name, port
numbers, and identify files.

### Initialize a new git repository
```bash
mkdir ~/git/foo
cd ~/git/foo
git init
```

### Examples use [dotfiles](https://github.com/Traap/dotifles.git) as illustrations.
#### Cloning a git repository is done normally once or to start over.
```bash
rm -rf git/dotfiles
git clone http://github.com/Traap/dotfiles.git
```

#### Navigate to git working directory.
```bash
cd ~/git/dotfies
```

#### Determine current status.
```bash
git status
```

#### Checkout a new branch name barbaz and set the stream remote branch.
```bash
git checkout -b barbaz
git branch --set-upstream-to origin/foobar foobar
```

#### Review the last 7 commists made.
```bash
git log --oneline -7
```

#### Add all untracked files
```bash
touch a
touch b
git add .
```

#### Add a single untracked file
```bash
touch c
git add c 
```

#### Update stage area with comments and push to remote.
```bash
git commit -m "Example comment"
git push
```

#### Move back to master branch
```bash
git checkout master
```

### Initialize global git configuration options.
```bash
git config  --global  user.name Traap
git config  --global  user.email gary.a.howard@mac.com
git config  --global push.default simple
```

### Initialize local git configuration options.
```bash
git config  --global  user.name fred
git config  --global  user.email fred.flinstone@bedrock.net
git config  --global push.default simple
```

### Rebase a branch with master
```bash
cd ~/git/dotfiles
git chechout foobar
git fetch origin
git rebase -i master
```

### Rename local and remote branch
```bash
git branch -m old-name new-name
git push origin :old-name
git push --set-upstream origin new-name
```

### Find git commands I used.
```bash
history | grep "git " > Notes.txt
```
