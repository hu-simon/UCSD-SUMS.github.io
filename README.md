# UCSD-SUMS.github.io
This repository contains the files that generate the SUMS website.

Built with [Hakyll](https://jaspervdj.be/hakyll/) [![CircleCI](https://circleci.com/gh/UCSD-SUMS/UCSD-SUMS.github.io/tree/hakyll.svg?style=svg)](https://circleci.com/gh/UCSD-SUMS/UCSD-SUMS.github.io/tree/hakyll)

# Installation
- Install [Stack](https://www.haskellstack.org/)
```
curl -sSL https://get.haskellstack.org/ | sh
```

- Use Stack to install GHC
```
stack setup
```


- Use Stack to build and deploy locally
```
git clone https://github.com/UCSD-SUMS/UCSD-SUMS.github.io
stack build
stack exec site watch
```

Once it is finished, you can then navigate to `localhost:8000` to see the site.


# Updating Events

All changes should be pushed to the `hakyll` branch, which kicks off the automated
build and deploy process.

After making a change, run the following:
```
stack exec site clean
stack exec site watch
```
