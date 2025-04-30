### (For developers)

create ssh key

```
ssh-keygen -t ed25519 -C "oftendeck01"
```

show ssh key

```
cat ~/.ssh/id_ed25519.pub
```

copy to GitHub account under Settings > SSH keys

change repo to use ssh instead of https

```
cd /path/to/your/repo
git remote set-url origin git@github.com:oftenzhan/OftenDeck.git

```

push with this

```
git add .
git commit -m "Your commit message here"
git push
```

The first time it commits, it will ask you for Author Identity Info. Put your Name and email address. 

```
git config --global user.name "Often Zhan"
git config --global user.email "oftenzhan@gmail.com"
```