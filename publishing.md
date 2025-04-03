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

Step 3: Create Blog Posts

1. Create the First Blog Post:

You can create a new post using the Hugo command:

hugo new posts/my-first-post.md

This will create a Markdown file for the new post in content/posts/my-first-post.md. Open the file to add content:

nano content/posts/my-first-post.md

Add the following sample content (you can modify this to your liking):

---
title: "My First Post"
date: 2025-04-03T12:00:00Z
draft: true
---
Welcome to my first blog post! This is a Hugo-powered site using the Hugo Book theme.



2. Create Four More Posts:

Repeat the same process for four more posts. Example commands:

hugo new posts/my-second-post.md
hugo new posts/my-third-post.md
hugo new posts/my-fourth-post.md
hugo new posts/my-fifth-post.md

Edit each file and add content similarly.





---

Step 4: Preview Your Site Locally

1. Run the Hugo Development Server:

To preview your site locally, run:

hugo server -D

This will build the site and serve it on http://localhost:1313/.



2. Access Your Blog:

Open your web browser and go to http://localhost:1313/ to see your Hugo blog in action.





---

Step 5: Build and Deploy Your Site

1. Build the Site for Production:

Once you're happy with your posts and the site's look, you can generate the static files by running:

hugo

This will generate the public folder containing all your site files.



2. Deploy to GitHub Pages or Other Hosting:

If you want to deploy your blog to GitHub Pages or another hosting service, follow the appropriate deployment instructions.


For GitHub Pages, you would need to push the contents of the public/ folder to a gh-pages branch in your GitHub repository.




---

Recap of What We've Done

1. Installed Hugo using Snap on Raspberry Pi OS Lite.


2. Cloned the Hugo Book theme from GitHub.


3. Created 5 blog posts using Hugo's hugo new command.


4. Ran a local server to preview the blog.


5. Built the site and discussed deployment options.




---

This should give you a fully functional Hugo blog on your Raspberry Pi with the Hugo Book theme. Let me know if you need more details or help with any part of the process!

