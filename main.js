/*\
|*| math helpers
\*/


function multAll (things) {
  return things.reduce(function (acc, m) { return math.multiply(acc,m) })
}


/*\
|*| THREE helpers
\*/


function proj3(arr) {
  // very simple projection for now
  return new THREE.Vector3(arr[0],arr[1],arr[2]);
}


/*\
|*| Main
\*/


function setupHypercubeCallback( dt, dim, simShimInst ) {
  return function (hypercube) {
    // unpack
    var rotMats = hypercube.rotationMatrices;
    var edges = hypercube.edges;
    // adjust cube to be centered on 0
    var verts = hypercube.vertices.map(function(v) {
      return v.map(function (component) {
        return component - 0.5;
      })
    });

    // generate a random ish rotation matrix
    var M = multAll( rotMats.map(function (m) {
      return rotMats[ Math.floor(Math.random()*rotMats.length) ];
    }));

    ////////////////// THREE stuff

    var material = new THREE.LineBasicMaterial({
    	color: 0x7777ff
    });

    var simShimObjects = edges.map(function (edge,i) {
      var e0 = edge[0], e1 = edge[1]; // vertex pair for an edge

      var geometry = new THREE.Geometry();
      geometry.dynamic = true;
      geometry.vertices.push(
      	proj3( verts[e0] ),
      	proj3( verts[e1] )
      );

      var edgeLine = new THREE.Line( geometry, material );
      edgeLine.frustumCulled = false;

      return {
        update: function () {
          if (i==0) {
            verts = verts.map(function (v) {
              return math.multiply(M, v);
            });
          }

          edgeLine.geometry.vertices = edge.map(function (ei) {
            return proj3( verts[ei] );
          });

          edgeLine.geometry.verticesNeedUpdate = true;
        },
        threeObj: edgeLine
      };
    });

    simShimObjects.map(function (sso) { simShimInst.addObject(sso) });
    // simShimInst.setPaused(false);
  }
}


// using a few globals here because I'm lazy, nothing weird should happen, just look the other way
function reset(dim) {
  ss.kill();
  hypercube.ports.requestDimension.send( [dt, dim] );
  ss.setPaused(false);
}


// == INIT == //

var ss = new SimShim(
  document.getElementById("plot"),
  {
    "cameraPosn": [ 3, 3, 3 ],
    "orbitTarget": [ 0, 0, 0 ],
    "clearColor": "#161616"
  }
);
ss.start();

var dt = 1/700,
    dim = 3,
    cb = setupHypercubeCallback(dt, dim, ss),
    hypercube = Elm.worker(Elm.Hypercube, { requestDimension:[0, 0] })
    ;
hypercube.ports.responseDimension.subscribe(cb);
reset(dim);


// == Listen == //


function resetFromHtml() {
  var val = document.getElementById("reset-input").value,
      num = parseInt(val)
      ;
  if (num <= 1) {
    alert("Please enter a number greater than 1");
    return;
  }
  reset(num);
}

document.getElementById("reset").addEventListener('click', resetFromHtml, false);

document.getElementById("reset-input").addEventListener('keypress', function (e) {
    var key = e.which || e.keyCode;
    if (key === 13) { // 13 is enter
      resetFromHtml();
    }
});
