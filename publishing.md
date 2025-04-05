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

1. Run the Hugo Development Server:

To determine your IP address: 

```ip a | grep inet```

It should look something like `192.168.x.x`

Preview your site on your local wifi network by running:

```hugo server -D --bind 0.0.0.0```

This will build the site and serve it on http://{ip-address}:1313/.

2. Access Your Blog:

Open your web browser and go to http://{ip-address}:1313/ to see your Hugo blog in action.

## Step 5: Build and Deploy Your Site Locally

1. Build the Site for Production:

Once you're happy with your posts and the site's look, you can generate the static files by running:

```hugo```

This will generate the public folder containing all your site files.

## Step 4. Deploy to Netlify

There are three options that you can do to deploy to Netlify. First two options are to use the existing templates built in from Netlify and customize it from there. The third was is my quick-and-dirty approach to setting up a blog on Netlify.

For this approach, I will be using the theme [Hugo Book](https://github.com/alex-shpak/hugo-book) 

#### Step 1. Download `hugo-book` theme with example site

This guide will help you set up a Hugo blog locally using the theme and make the example site your actual site.

##### 1.01. Create a New Hugo Site

Open your terminal and run:

```
hugo new site my-blog    
```


```sh
hugo server -D --bind ##### 2. Download the Hugo Book Theme

Then you need to move into the theme directory root folder:
```sh
cd my-blog
```

Clone the theme into the `themes` directory:

```sh
git clone --depth 1 https://github.com/alex-shpak/hugo-book themes/hugo-book
```

The `--depth 1` tag means that it is only downloading the most recent commit, or the most recent version.
##### 3. Copy the Example Site as Your Main Site

Just to make sure it works, we will make the example site the actual blog. We can adjust the words in the blog later, but we just want to see if every aspect of the theme will work correctly.

Overwrite your default Hugo setup with the example site:

```sh
cp -r themes/hugo-book/exampleSite/* .  
```

In this particular theme, there are two files: hugo.yaml and hugo.toml. We don't need both, so we will delete hugo.yaml and rename hugo.toml to config.toml.

```sh
rm hugo.yaml
mv hugo.toml config.toml
```

##### 4. Run the Site Locally

Run Hugo to generate the static files.
```
cd path/to/blog
hugo
```

The new files will be in `/public`.

Start the Hugo development server:

```sh
hugo server -D --bind 0.0.0.0
```
The site will be available at your ip address with the ending `:1313`

To find your IP address, switch to a new tty using `cmd+→`, login, and type in:

```
ip a | grep 'inet '
```

It should look something like `192.168.x.xx`, so you would put in your web browser something like: `192.168.x.xx:1313`


##### 5. Initialize a Git Repository

You want to use Git for version control. 

```
git init  
git add .  
git commit -m "Initial commit with Hugo Book theme"  
```



#### Step 2. Add Netlify.toml

Make a new file on your root folder named `netlify.toml`.

Insert the following.

```
# example netlify.toml
[build]
  command = "hugo"
  functions = "netlify/functions"
  publish = "public"

[build.environment]
  HUGO_VERSION = "0.145.0"

```

#### Step 3. Deploy on Netlify & 🙏 that it works.

You need to go to the website, make an account, and ask them to deploy it. 

#### Step 4. Edit the Blog

Now that everything is working, all you have to do is slowly build your website and use the following to update your website once you made changes.

```
cd /path-to-your-blog
hugo
git add .
git commit -m "Describe your edit here"
git push origin main
```
#### Step 5. Automate on Emacs
Lastly, we want to automate this process so that we don't have to leave Emacs after editing a document. When we finish our edits, we can just send out a shortcut which will automatically do everything in Step 4.

Paste this into your init.el (or evaluate it in Emacs):

```
(defun my-hugo-deploy ()
  "Build Hugo site, commit, and push—all from within Emacs."
  (interactive)
  (let* ((default-directory "/path-to-your-blog/") ;; <--- Change this!
         (commit-msg (read-string "Describe your edit: ")))
    (async-shell-command "hugo")
    (shell-command "git add .")
    (shell-command (format "git commit -m \"%s\"" commit-msg))
    (shell-command "git push origin main")
    (message "Deployed Hugo blog with commit: %s" commit-msg)))

(defun my-markdown-mode-setup ()
  "Custom keybindings for markdown-mode."
  (local-set-key (kbd "C-c C-d") 'my-hugo-deploy))

(add-hook 'markdown-mode-hook 'my-markdown-mode-setup)
```

Now, when you're editing any markdown file (.md)—like blog posts—you can press:

`C-c C-d`
to deploy directly from Emacs.