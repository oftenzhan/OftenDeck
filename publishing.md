This section is for the aspects of the OftenDeck for publishing.

# Blogging
For blogging, we used Emacs Projects function with Hugo.

## Step 1: Install Snap and Hugo 

1. Install Snap:

Do not install Hugo via `atp` because the file is too old and not all themes will run.

```sudo apt-get install snapd```

2. Install Hugo using Snap:

Install the latest version of Hugo:

```sudo snap install hugo --classic```

When I did it, Hugo was not working. I needed to link the hugo file to the correct bin file:

```
nano ~/.bashrc
```

Add to the bottom of the file:

```export PATH=$PATH:/snap/bin```

Then reload shell config:
```source ~/.bashrc```

3. Fix ld.so error

There is an error that pops up with the ld.so preload error because the libarmmem.so file was preloaded, causing an error message. This file is unnecessary for running Hugo. (I think the error is because my RasPi OS is 32-bit OS instead of 64-bit.) To fix it, the line in /etc/ld.so.preload is commented out thereby disabling it:

```sudo nano /etc/ld.so.preload```

Then, comment out the line by adding a hashtag in the beginning.

``` #/usr/lib/arm-linux-gnueabihf/libarmmem.so```

## Step 2: Setting up a Hugo theme

1. Create a New Hugo Site:

Navigate to the directory where you want to create your blog:

```
cd ~
mkdir oftenblog
hugo new site ~/oftenblog
cd oftenblog
```

2. Clone the Hugo theme:

In this case, I am using the theme `Book`. Use Git to clone the Hugo Book theme:


```
git clone https://github.com/alexandrevicenzi/hugo-book.git themes/hugo-book
```

3. Configure Your Site:

Open the config.toml file:

```nano config.toml```

Replace the contents with the following basic configuration:

```
baseURL = "http://example.org/"
languageCode = "en-us"
title = "My Hugo Blog"
theme = "hugo-book"
```

## Step 3: Create Blog Posts

1. Create the First Blog Post:

You can create a new posts using the Hugo command:

```
hugo new posts/001.md
hugo new posts/002.md
hugo new posts/003.md
hugo new posts/004.md
hugo new posts/005.md
```

This will create a Markdown file for the new post in `content/posts/0001.md`, etc. Open the file to add content:

```emacs ~/oftenblog/content/posts/001.md```

Edit the front-matter and content to your liking.

## Step 4: Preview Your Site Locally

2. Run the Hugo Development Server:

To determine your IP address: 

```ip a | grep inet```

It should look something like `192.168.x.x`

Preview your site on your local wifi network by running:

```hugo server -D --bind 0.0.0.0```

This will build the site and serve it on http://{ip-address}:1313/.

2. Access Your Blog:

Open your web browser and go to http://{ip-address}:1313/ to see your Hugo blog in action.

## Step 5: Build and Deploy Your Site

1. Build the Site for Production:

Once you're happy with your posts and the site's look, you can generate the static files by running:

```hugo```

This will generate the public folder containing all your site files.


2. Deploy to GitHub Pages or Other Hosting:

