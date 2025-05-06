# Emacs Configuration By Mugsimon
This repository contains my personal Emacs configuration, tailored to enhance productivity, improve workflow, and support various development tasks.

## Customization ##
- [timu-macos-theme](https://emacsthemes.com/themes/timu-macos-dark-&-light.html) with some custom faces.
- **rainbow-mode-toggle-hex-only** [rainbow-mode](https://elpa.gnu.org/packages/rainbow-mode.html) works for only hexadecimal colors.
- **ms:corfu-expand** combination of `corfu-expand` and `corfu-complete` triggered by TAB key.
- **pyright-env** changes conda environment for pyright(eglot). It works for both local and remote(TRAMP).
- **ms:move-beginning-of-line** toggle beginning of line and indent head. Enhanced `move-beginning-of-line` binded `C-a`.
- **mozc** Japansese IME on with 変換, IME off with 無変換

## Requirements ##
- Ubuntu or Debian-based system.
- Snap package manager.
- Git

## Setup
1. Clone the Repository:
    ``` shell
    git clone https://github.com/mugsimon/.emacs.d.git
    ```
2. Install Emacs
    ``` shell
    sudo apt install emacs
    ```
## Install Dependencies
``` shell
sudo apt update
```
1. Install emacs mozc (for Japanese user)
    ``` shell
    sudo apt install emacs-mozc
    ```
2. Install markdown
    ``` shell
    sudo apt-get install markdown
    ```

## Xresources configuration
``` shell
touch $HOME/.Xresources
echo "Emacs*UseXIM: false" >> $HOME/.Xresources
```

## Install LSP (Optional)
### pyright
``` shell
npm install -g pyright
```
### clangd

``` shell
sudo apt install clangd
```
### docker-langserver

``` shell
npm install -g dockerfile-language-server-nodejs
```
### vscode-html-language-server

``` shell
npm i -g vscode-langservers-extracted
```
