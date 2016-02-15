### Hypercube

This project implements the necessary logic to display and rotate cubes of any
dimension. A demonstration is included through `main.js` and `index.html` which
uses THREE js to display the cubes.

The logic for creating the rotation matrices and vertices, and for finding the
pairs of vertices which correspond to an edge, is written in Elm and can be
found in `src/Hypercube.elm`. The file `elm-hypercube.js` is just the compiled
file that gets imported into HTML.

To glue this logic to WebGL, I've used my simulation library, sim-shim-js. This
simplifies the role of `main.js` to just having to build the THREE js objects
and provide an update function. More details about how it works can be found on
the [sim-shim-js project page](https://github.com/codemaker1999/sim-shim-js).

A more complete demo is up [here](http://hbar.ca/projects/hypercube).

### Build

`elm make src/Hypercube.elm --output elm-hypercube.js`
