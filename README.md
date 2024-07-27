# README #

Pick the leetcode quiz description

## Prepare ##

+ SBCL
+ ASDF > 3.1
+ Quicklisp
+ [cl-str](https://github.com/vindarel/cl-str/)

Add `vault` folder in **compiling path** (which is the path of this project), copy `csrftoken` value from browser, paste it in `vault/leetcode-token`.

Copy `LEETCODE_SESSION` from browser and paste it in `vault/leetcode-session`

The vault path will hardcode in lisp image after compiling.

Token and session value will hardcode inside lisp image, but the cache will stay in value folder.

[cl-str](https://github.com/vindarel/cl-str/) need the master branch because the quicklisp might not updated the master version yet.

## Install ##

`./make.sh`

Will generate the executive lisp image named `leetcode-picker`

## Usage ##

> leetcode-picker get-description -n 3075 -o ./README.md

will get the quiz 3075 from leetcode and generate markdown content in the `README.md` in **current path**

> leetcode-picker --help

shows more help
