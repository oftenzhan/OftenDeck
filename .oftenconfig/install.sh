
# Install applications using apt
sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y
sudo apt install git emacs-nox hunspell hunspell-en-us syncthing -y

# Symlinks files
sudo ln -sf /home/oftendeck/.oftenconfig/scripts/bin/* /usr/local/bin/*
sudo ln -sf /home/oftendeck/.oftenconfig/scripts/sbin/* /usr/local/bin/*


