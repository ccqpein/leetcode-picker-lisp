# README #

Pick the leetcode quiz description

## Prepare ##

+ SBCL
+ ASDF > 3.1
+ Quicklisp

Add `vault` folder in **compiling path** (which is the path of this project), copy `csrftoken` value from browser, paste it in `vault/leetcode-token`.

Copy `LEETCODE_SESSION` from browser and paste it in `vault/leetcode-session`

The vault path will hardcode in lisp image after compiling.

Token and session value will hardcode inside lisp image, but the cache will stay in value folder.

## Usage ##

`./make.sh`

Will generate the lisp image named `leetcode-picker`

`leetcode-picker 3075` will get the quiz 3075 from leetcode and generate the `README.md` in **current path**
