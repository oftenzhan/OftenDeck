# Install Dufs

1. Update Package Lists and Upgrade Installed Packages:

Open your terminal and execute the following commands to update the package lists and upgrade all installed packages:

```
sudo apt update
sudo apt upgrade
```

2. Install Docker:

Run the following command to install Docker using the official installation script:

curl -sSL https://get.docker.com | sh

3. Add Your User to the Docker Group:

To manage Docker as a non-root user, add your user to the Docker group:

sudo usermod -aG docker $USER

4. Apply Group Membership Changes:

Log out and log back in to apply the group membership changes:

logout

After logging back in, you can verify your group memberships with:

groups

5. Test Docker Installation:

Verify that Docker is installed correctly by running the "hello-world" container and redirecting the output to a file:

docker run hello-world > output.txt

To view the output, you can use:

cat output.txt

After reviewing, remove the output file:

rm output.txt

6. Run Dufs with Docker:

Use Docker to run Dufs, a file-sharing application, by executing:

docker run -v "$(pwd)":/data -p 5000:5000 --rm sigoden/dufs /data -A

This command does the following:

-v "$(pwd)":/data mounts the current directory to /data inside the container.

-p 5000:5000 maps port 5000 on your host to port 5000 in the container.

--rm removes the container once it stops.

sigoden/dufs specifies the Docker image to use.

/data -A runs Dufs on the mounted directory with the -A flag.


Ensure that Dufs is configured correctly to suit your needs. To close, use ctrl+c

Note: The logout command will terminate your current session. Ensure you have saved all your work before proceeding.

