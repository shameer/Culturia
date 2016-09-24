# Culturia

![Babel tower](https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/Tower_of_Babel_cropped_square.jpg/480px-Tower_of_Babel_cropped_square.jpg)

Culturia is an intelligence augmentation software.

The name is a reference to
[Culture and Empire by Pieter Hintjens](http://cultureandempire.com) and
[Intelligence Augmentation](https://en.wikipedia.org/wiki/Intelligence_amplification).

## Roadmap

Create a framework that makes it easy to tape into NLP algorithm and provide an
interface for common tasks.

## Getting started

### Ubuntu and others

Install wiredtiger develop branch and that is all.

### guix and guixd

Use the following command starting from the git repository:

```bash
culturia $ guix build -f guix.scm
culturia $ patch -p1 < guix-wiredtiger.diff
culturia $ guix environment --ad-hoc --pure guile-next
culturia $ cd src
culturia/src $ CHECK=t guile -L . grf3.scm 
culturia/src $ CHECK_WSH=t guile -L . wsh.scm
```

## Files

Source files can be found in `src` directory.
Documentation can be found in `doc` directory.

## Contact

[Say héllo!](mailto:amirouche@hypermove.net)
