# Reactor template for Purescript

A project template for using [grid-reactors](https://github.com/Eugleo/purescript-grid-reactors). For the time being, the best way to use reactors in Purescript is to donwload the package from its repo, because it has not yet been published in a package set. The file `packages.dhall` in this template takes care of this.

## How to use
One-time setup:


1. Install [nvm](https://github.com/nvm-sh/nvm), or [nvm-windows](https://github.com/coreybutler/nvm-windows).
2.  List the `14.x.x` versions of `node` by running `nvm ls-remote v14` (on windows, `nvm list available`).
3.  Install the latest `14.x.x` version of `node` by running `nvm install [chosen version]`.  The template is tested with `v14.17.6`.

Now, get a local copy of this template:

4. Sign-in to your GitHub account.
5. Click **Use this template** above the list of files. GitHub will walk you through creating your own repository with same folder structure as this one.
6. Clone your new repository using `git clone [repository url]`.
7. Move to the cloned repository with `cd [directory of the cloned repo]`.
8. Open the current folder in VS Code by running `code .`.

Setup `npm` for our project:

9. Make sure the active folder is the project folder. Activate the correct `node` version (`v14.x.x`) by running `nvm use [chosen version]`.
2.  Verify that `node --version` now returns the version you activated.

Finally, install Purescript and other required dependencies:

11. Run `npm install` to install the javascript dependencies, such as `spago` and `purescript`.
6. Run `npm run spago-build` to build the project and install the purescript dependencies. Anytime you want to build the project, use this command instead of `spago build`.
7.  Finally, run `npm start` to start a server that will be serving your webpage. Update the code in `src/Main.purs`, and the webpage will change accordingly.

If you want to __install additional spago packages__, do so by running `npm run spago-install [package names]` instead of simply running `spago install [...]`.

### Changing the name of the project

Once you have a name for your project, make updates in the following three places:

- Inbetween the `title` tags in `dev/index.html`. This will change the title of the reactor webpage.
- The field `name` in `spago.dhall`.
- The field `name` in `package.json`.