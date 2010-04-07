the script to generate an erlang application skeleton.
the app template is in the app.in directory.

## Usage

    ./gen_app 

will print the help info

    ./gen_app AppName [Option]
    Option:
    -a             author info (e.g. litaocheng)
    -c             copyright info (e.g. abc.com)
    -m             email info (e.g. somebody@gmail.com)
    -o             application output path. default is current path

    AppName:
    the application name, must be an erlang atom (e.g. demo, hello)


## examples

    ./gen-app demo

will create an application named `demo` in the current directory.

    ./gen-app demo2 -a litaocheng -c litao.com -m litaocheng@gmail.com -o /tmp/

will create an applicaton named `demo2` in the /tmp directory. the application
author is `litaocheng`, the copyright is owned to litao.com, the develper email
is `litaocheng@gmail.com`, the output path is `/tmp/`



