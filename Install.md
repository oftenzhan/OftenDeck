# Install Dufs

1. Update Package Lists and Upgrade Installed Packages:

Open your terminal and execute the following command to update the package lists and upgrade all installed packages:

```
sudo apt update -y && sudo apt upgrade -y
```

2. Install Rust

In order to install `Dufs`, Rust and Cargo need to be installed. 

```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

3. Install Dufs

Afterwards, Dufs is installed using cargo.

```
cargo install dufs
```

This takes a long time because it is compiling it. It takes around an hour.

4. Run Dufs

Do a test run with Dufs by running

```
dufs -A
```

5. Edit the html and CSS

Dufs is very barebones but that is what makes it work very well. You can change the look and feel of it. I changed the CSS to be a bit more mobile friendly. This is my file for `index.css`.

```
html {
  font-family: -apple-system, BlinkMacSystemFont, Roboto, Helvetica, Arial, sans-serif;
  line-height: 1.5;
  color: #24292e;
}

body {
  margin: 0;
}

.hidden {
  display: none !important;
}

.head {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  padding: 0.6em 1em;
  position: sticky;
  top: 0;
  background-color: white;
}

.breadcrumb {
  font-size: 1.25em;
  padding-right: 0.6em;
  word-break: break-word;
}

.breadcrumb>a {
  color: #0366d6;
  text-decoration: none;
}

.breadcrumb>a:hover {
  text-decoration: underline;
}

.breadcrumb>b {
  color: #24292e;
}

.breadcrumb>.separator {
  color: #586069;
  padding: 0 0.25em;
}

.toolbox {
  display: flex;
  margin-right: 10px;
}

.toolbox>a,
.toolbox>div {
  height: 1.1rem;
}

.toolbox .control {
  cursor: pointer;
  padding-left: 0.25em;
}

.upload-file input {
  display: none;
}

.upload-file label {
  cursor: pointer;
}

.searchbar {
  display: flex;
  flex-wrap: nowrap;
  width: 246px;
  height: 22px;
  background-color: #fafafa;
  transition: all .15s;
  border: 1px #ddd solid;
  border-radius: 15px;
  margin-bottom: 2px;
}

.searchbar #search {
  box-sizing: border-box;
  width: 100%;
  height: 100%;
  font-size: 16px;
  line-height: 16px;
  padding: 1px;
  background-color: transparent;
  border: none;
  outline: none;
}

.searchbar .icon {
  color: #9a9a9a;
  padding: 3px 3px;
  cursor: pointer;
}

.main {
  padding: 0 1em;
}

.empty-folder {
  font-style: italic;
}

.uploaders-table th,
.paths-table th {
  text-align: left;
  font-weight: unset;
  color: #5c5c5c;
  white-space: nowrap;
}

.uploaders-table td,
.paths-table td {
  white-space: nowrap;
}

.uploaders-table .cell-status {
  width: 80px;
  padding-left: 0.6em;
}

.cell-status span {
  display: inline-block;
}

.paths-table thead a {
  color: unset;
  text-decoration: none;
}

.paths-table thead a>span {
  padding-left: 2px;
}

.paths-table tbody tr:hover {
  background-color: #fafafa;
}

.paths-table .cell-actions {
  width: 90px;
  display: flex;
  padding-left: 0.5em;
}

.paths-table .cell-mtime {
  width: 120px;
  padding-left: 0.5em;
  font-variant-numeric: tabular-nums;
}

.paths-table .cell-size {
  text-align: right;
  width: 70px;
  padding-left: 0.5em;
  font-variant-numeric: tabular-nums;
}

.path svg {
  height: 16px;
  fill: rgba(3, 47, 98, 0.5);
  padding-right: 0.5em;
  vertical-align: text-top;
}

.path {
  list-style: none;
}

.path a {
  color: #0366d6;
  text-overflow: ellipsis;
  white-space: nowrap;
  overflow: hidden;
  display: block;
  text-decoration: none;
  max-width: calc(100vw - 375px);
  min-width: 170px;
}

.path a:hover {
  text-decoration: underline;
}

.action-btn {
  padding-right: 0.3em;
  cursor: pointer;
}

.uploaders-table {
  padding: 0.5em 0;
}

.uploader {
  padding-right: 1em;
}

.editor {
  width: 100%;
  height: calc(100vh - 5rem); /* Adjust this if needed */
  border: 1px solid #ced4da;
  outline: none;
  padding: 5px;
}

.toolbox-right {
  margin-left: auto;
  margin-right: 2em;
}

.login-btn {
  cursor: pointer;
}

.save-btn {
  cursor: pointer;
  -webkit-user-select: none;
  user-select: none;
}

.logout-btn {
  cursor: pointer;
  display: flex;
  align-items: center;
}

.user-name {
  padding-left: 3px;
}

.not-editable {
  font-style: italic;
}

.retry-btn {
  cursor: pointer;
}

@media (max-width: 768px) {
  .breadcrumb {
    font-size: 1.1em;
  }
  .searchbar {
    width: 100%;
  }
}

@media (prefers-color-scheme: dark) {
  body {
    background-color: #000;
  }

  html,
  .breadcrumb>b,
  .searchbar #search {
    color: #fff;
  }

  .uploaders-table th,
  .paths-table th {
    color: #ddd;
  }

  svg,
  .path svg,
  .breadcrumb svg {
    fill: #fff;
  }

  .head {
    background-color: #111;
  }

  .searchbar {
    background-color: #111;
    border-color: #fff6;
  }

  .searchbar svg {
    fill: #fff6;
  }

  .path a {
    color: #3191ff;
  }

  .paths-table tbody tr:hover {
    background-color: #1a1a1a;
  }

  .editor {
    background: black;
    color: white;
  }
}
```

All the assets should be put in the assets folder and link it to Dufs by typing in terminal:

```
dufs -A --assets ~/microjournal/.config/Dufs/assets
```

# Install Emacs

Since there is no gui, just install Emacs (no X).

```
sudo apt-get install emacs-nox
```

# Install Syncthing

To install Syncthing on your Raspberry Pi and configure it for Wi-Fi access, follow these steps:

1. Install Syncthing:

```
sudo apt install syncthing
```

2. Configure Syncthing for Wi-Fi Access:

By default, Syncthing's web GUI is accessible only from the Raspberry Pi itself. To allow access over Wi-Fi, modify the GUI's listen address.

Open the Syncthing configuration file:

```
nano ~/.config/syncthing/config.xml
```

Locate the <gui> section and change the <address> to `0.0.0.0:8384`:

```
<gui enabled="true" tls="false">
    <address>0.0.0.0:8384</address>
    ...
</gui>
```
Save and close the file (press <kbd>Ctrl+X</kbd>, then <kbd>Y</kbd>, and Enter).
