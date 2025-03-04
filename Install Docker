sudo apt update
sudo apt upgrade

curl -sSL https://get.docker.com | sh

sudo usermod -aG docker $USER

logout

groups

docker run hello-world > output.txt
rm output.txt

docker run -v `pwd`:/data -p 5000:5000 --rm sigoden/dufs /data -A
