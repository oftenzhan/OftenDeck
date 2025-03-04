# Install Dufs

1. Update Package Lists and Upgrade Installed Packages:

Open your terminal and execute the following commands to update the package lists and upgrade all installed packages:

```
sudo apt update
sudo apt upgrade
```

2. Install Docker:

Run the following command to install Docker using the official installation script:

```
curl -sSL https://get.docker.com | sh
```
or (less ideal)

```
sudo apt update
sudo apt install docker.io
```

3. Add Your User to the Docker Group:

To manage Docker as a non-root user, add your user to the Docker group:

```
sudo usermod -aG docker $USER
```

4. Apply Group Membership Changes:

Log out and log back in to apply the group membership changes:

```
logout
```

After logging back in, you can verify your group memberships with:

```
groups
```

5. Test Docker Installation:

Verify that Docker is installed correctly by running the "hello-world" container. Since the screen is so short, you cannot see the whole output so you have to export the output as a file so you can scroll up.

```
docker run hello-world > output.txt
```

To view the output, you can use:

```
nano output.txt
```

After reviewing to see that it works, remove the output file:

```
rm output.txt
```

6. Run Dufs with Docker:

Use Docker to run Dufs, a file-sharing application, by executing:

```
docker run -v /home/microjournal/documents/:/data -p 5000:5000 --rm sigoden/dufs /data -A
```

Ensure that Dufs is configured correctly to suit your needs. To close, type <kbd>ctrl</kbd>+<kbd>c</kbd>.

