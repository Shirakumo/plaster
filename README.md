## About Plaster
This is a paste service application for the [Radiance](https://shirakumo.github.io/radiance) environment. It provides an editor and syntax highlighting for a variety of languages by using [CodeMirror](http://codemirror.net/). The main features of this application are also subject of the [Radiance tutorial](https://github.com/Shirakumo/radiance-tutorial/blob/master/Part%200.md) where you'll learn how to build Plaster step by step.

## Features

* Public, unlisted, and private pastes
* Password protection for pastes
* Syntax highlighting and advanced code editing
* Public and per-user paste listing
* Paste annotations and editing
* Complete REST API

## Emacs integration [![MELPA](https://melpa.org/packages/plaster-badge.svg)](https://melpa.org/#/plaster)
Using the [plaster.el](https://github.com/Shirakumo/plaster/blob/rewrite/plaster.el), you can manage pastes on Plaster directly within Emacs:

* `plaster-login`  
  If you have a plaster account, use this
  to log yourself in.
* `plaster-visit`  
  Opens an existing paste in a buffer
* `plaster-paste-buffer`  
  Pastes the current buffer to a new paste
* `plaster-paste-region`  
  Pastes the current region to a new paste
* `plaster-new`  
  Opens a new buffer for a new paste
* `plaster-annotate`  
  Create an annotation for the current paste (C-x C-a)
* `plaster-save`  
  Save the current buffer as a paste (C-x C-s)
* `plaster-delete`  
  Delete the current buffer's paste (C-x C-k)

The keybindings are for when you're in a buffer with `plaster-mode` active. When you create a new paste, the URL is automatically placed into your kill-ring.

You can also use the customize-group `plaster` to edit some settings:

* `plaster-root`  
  The root URL for the Plaster server if you're hosting your own instance.
* `plaster-session-token`  
  The token for the Radiance session that authenticates you.
* `plaster-type-mode-map`  
  A map that decides which Emacs mode to use for which paste types.

## Demo Setup
An existing Plaster installation is publicly available at [plaster.tymoon.eu](https://plaster.tymoon.eu).
