## Features

* Automagically collect package information such as
	* package name
	* GHC compatibility
	* stack Project
	* Hackage / Stackage status
	* License
	* Examples
	* Cli Usage
* Typecheck examples
* Generate informative README.md (Can be extended using a MORE.md)
* Generate .travis.yml (cabal or stack based)
* Generate .gitignore

The generated `.travis.yml` and `.gitignore` are intended as starting templates, while the generated `README.md` should not be modified by hand. Rerun `hackmanager readme` before every commit (commit hook?) to keep it up to date. If you would like to add custom sections, create a `MORE.md`.

## Roadmap

There's no real roadmap - I will add features as needed. I am open to any contributions!