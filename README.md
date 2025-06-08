# cmdmemo

This is a simple program that stores snippets of frequently used commands in a .conf file

## Compiling

- Install the Free Pascal compiler [fpc](https://www.freepascal.org/download.html)
- Run the command below

```bash
cd cmdmemo
fpc -Mfpc -Sh -Cg -O3 -l -vewnhibq -Filib/x86_64-linux -Fl/usr/lib/gcc/x86_64-pc-linux-gnu/15.1.1 -Fu./ -FUlib/x86_64-linux -FE. -ocmdmemo cmdmemo.lpr
```

## Usage

```
Usage: cmdmemo [args]

Options:
  -h, --help show this help message and exit
  -a, --add  add a command snippet
  -l, --list list all entries or for a specific category

Program Description:
  This is a simple program that stores snippets of frequently used commands

Examples:
  1) Add the command used to list services to the systemd category
  cmdmemo -a
  Enter a command: systemctl --type=service
  Enter a category: systemd
  Enter a description: List services

  2) List command snippets from systemd category
  cmdmemo -l systemd

  Name    | Command                  | Description
  --------|--------------------------|--------------
  systemd | systemctl --type=service | List services
```
