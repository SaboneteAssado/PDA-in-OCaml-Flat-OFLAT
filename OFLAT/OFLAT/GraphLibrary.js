/*
 * OFLAT/GraphLibrary.js
 *
 * This file is part of the OFLAT app
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Rita Macedo
 */

var number = 0;

var cy = null;
var cy2 = null;
var direction = null;

var layoutDir = null;

/* -------------------- Language -------------------- */

var language = 'en';

function changeLanguage (lang) {
  language = lang;
  if (cy != null) {
    cy.reset ();
  }
}

function textRemove () {
  if (language == "en") {
    return 'Remove'
  } else if (language == "pt") {
    return 'Remover'
  } else {
    return 'Remove'
  }
}

function textAdd () {
  if (language == "en") {
    return 'Add state'
  } else if (language == "pt") {
    return 'Adicionar estado'
  } else {
    return 'Add state'
  }
}

function textAddInitial () {
  if (language == "en") {
    return 'Add initial state'
  } else if (language == "pt") {
    return 'Adicionar estado inicial'
  } else {
    return 'Add initial state'
  }
}

function textAddFinal () {
  if (language == "en") {
    return 'Add final state'
  } else if (language == "pt") {
    return 'Adicionar estado final'
  } else {
    return 'Add final state'
  }
}

function textAddTransition () {
  if (language == "en") {
    return 'Add transition'
  } else if (language == "pt") {
    return 'Adicionar transição'
  } else {
    return 'Add transition'
  }
}

function textTurnFinal () {
  if (language == "en") {
    return 'Make final'
  } else if (language == "pt") {
    return 'Tornar final'
  } else {
    return 'Make final'
  }
}

function textRemoveFinal () {
  if (language == "en") {
    return 'Remove final'
  } else if (language == "pt") {
    return 'Remover final'
  } else {
    return 'Remove final'
  }
}

function textTurnInitial () {
  if (language == "en") {
    return 'Make initial'
  } else if (language == "pt") {
    return 'Tornar inicial'
  } else {
    return 'Make initial'
  }
}

function textEnterState () {
  if (language == "en") {
    return 'Please enter the state name'
  } else if (language == "pt") {
    return 'Por favor indique o nome do estado'
  } else {
    return 'Please enter the state name'
  }
}

function textEnterStartState () {
  if (language == "en") {
    return 'Please enter the start state'
  } else if (language == "pt") {
    return 'Por favor indique o nome do estado de partida'
  } else {
    return 'Please enter the start state'
  }
}

function textEnterTransition () {
  if (language == "en") {
    return 'Please enter the transition symbol'
  } else if (language == "pt") {
    return 'Por favor indique o simbolo da transição'
  } else {
    return 'Please enter the transition symbol'
  }
}

function textEnterEndState () {
  if (language == "en") {
    return 'Please enter the end state'
  } else if (language == "pt") {
    return 'Por favor indique o estado de chegada'
  } else {
    return 'Please enter the end state'
  }
}

function textMaximumSize () {
  if (language == "en") {
    return 'Please enter the maximum size for the words'
  } else if (language == "pt") {
    return 'Por favor indique o tamanho máximo para as palavras'
  } else {
    return 'Please enter the maximum size for the words'
  }
}

function textRegex () {
  if (language == "en") {
    return 'Please enter the regular expression'
  } else if (language == "pt") {
    return 'Por favor indique a expressão regular'
  } else {
    return 'Please enter the regular expression'
  }
}

function textWord () {
  if (language == "en") {
    return 'Please enter the word to test'
  } else if (language == "pt") {
    return 'Por favor indique a palavra para testar'
  } else {
    return 'Please enter the word to test'
  }
}

/* -------------------- Automatos -------------------- */

function start () {

  var cy = window.cy = cytoscape({
    container: document.getElementById('cy'),
    layout: {
      name: 'grid',
      rows: 2,
      cols: 2
    },
    style: [
      {
        selector: 'node[name]',
        style: {
          'content': 'data(name)',
          'width': '40px',
          'height': '40px',
          'text-valign': 'bottom',
          'text-halign': 'center'
        }
      },
      {
        selector: 'edge[symbol]',
        style: {
          'content': 'data(symbol)'
        }
      },
      {
        selector: 'edge',
        style: {
          'curve-style': 'bezier',
          'target-arrow-shape': 'triangle'
        }
      },
    {
      selector: '#transparent',
      style: {
        'visibility': 'hidden'
      }
    },
    {
      selector: '.SUCCESS',
      style: {
        'border-width': '7px',
        'border-color': 'black',
        'border-style': 'double'
      }
    },
    // some style for the extension

    {
      selector: '.eh-handle',
      style: {
        'background-color': 'red',
        'width': 12,
        'height': 12,
        'shape': 'ellipse',
        'overlay-opacity': 0,
        'border-width': 12, // makes the handle easier to hit
        'border-opacity': 0
      }
    },

    {
      selector: '.eh-hover',
      style: {
        'background-color': 'red'
      }
    },

    {
      selector: '.eh-source',
      style: {
        'border-width': 2,
        'border-color': 'red'
      }
    },

    {
      selector: '.eh-target',
      style: {
        'border-width': 2,
        'border-color': 'red'
      }
    },

    {
      selector: '.eh-preview, .eh-ghost-edge',
      style: {
        'background-color': 'red',
        'line-color': 'red',
        'target-arrow-color': 'red',
        'source-arrow-color': 'red'
      }
    },

    {
      selector: '.eh-ghost-edge.eh-preview-active',
      style: {
        'opacity': 0
      }
    }
    ],
    elements: {
      nodes: [
        {data: { id: 'transparent', name: 'transparent' }}
      ]
    },
  });
  var eh = cy.edgehandles();

  function onComplete (event, sourceNode, targetNode, addedEles) {
    var theSource = sourceNode.id();
    var theTarget = targetNode.id();
    var ele = addedEles.id();
    console.log(theSource);
    console.log(theTarget);
    console.log(ele);
    var symb = prompt(textEnterTransition (), "c");
    var getEdge = cy.getElementById(ele);
    getEdge.remove();
    jscode.m7 (theSource, theTarget, symb);
  }

  cy.on('ehcomplete', onComplete) 

  var defaults = {
    menuRadius: function menuRadius(ele) {
      return 70;
    }, // the radius of the circular menu in pixels
    commands: [// an array of commands to list in the menu or a function that returns the array
      /*
      { // example command
        fillColor: 'rgba(200, 200, 200, 0.75)', // optional: custom background color for item
        content: 'a command name' // html/text content to be displayed in the menu
        contentStyle: {}, // css key:value pairs to set the command's css in js if you want
        select: function(ele){ // a function to execute when the command is selected
          console.log( ele.id() ) // `ele` holds the reference to the active element
        },
        enabled: true // whether the command is selectable
      }
      */
    ], // function( ele ){ return [ /*...*/ ] }, // example function for commands
    fillColor: 'rgba(0, 0, 48, 0.75)', // the background colour of the menu
    activeFillColor: 'rgba(176,190,197, 0.75)', // the colour used to indicate the selected command
    activePadding: 20, // additional size in pixels for the active command
    indicatorSize: 20, // the size in pixels of the pointer to the active command, will default to the node size if the node size is smaller than the indicator size, 
    separatorWidth: 3, // the empty spacing in pixels between successive commands
    spotlightPadding: 0, // extra spacing in pixels between the element and the spotlight
    adaptativeNodeSpotlightRadius: false, // specify whether the spotlight radius should adapt to the node size
    minSpotlightRadius: 15, // the minimum radius in pixels of the spotlight (ignored for the node if adaptativeNodeSpotlightRadius is enabled but still used for the edge & background)
    maxSpotlightRadius: 25, // the maximum radius in pixels of the spotlight (ignored for the node if adaptativeNodeSpotlightRadius is enabled but still used for the edge & background)
    openMenuEvents: 'cxttapstart taphold', // space-separated cytoscape events that will open the menu; only `cxttapstart` and/or `taphold` work here
    itemColor: 'white', // the colour of text in the command's content
    itemTextShadowColor: 'transparent', // the text shadow colour of the command's content
    zIndex: 9999, // the z-index of the ui div
    atMouse: false // draw menu at mouse position
  };

  function config (c) {
    var empty = {};
    var a = Object.assign (empty, defaults);
    return Object.assign (a, c);
  };

  var nodeConfig = config ({
    selector: 'node', // elements matching this Cytoscape.js selector will trigger cxtmenus
    commands: [ 
      {
        content: textRemove (),
        select: function (ele) {
          var name = ele.id();
          jscode.m3 (name);
        }
      },

      {
        content: textTurnFinal (),
        select: function (ele) {
          var name = ele.id();
          jscode.m15 (name)
        }
      },

      {
        content: textRemoveFinal (),
        select: function (ele) {
          var name = ele.id();
          jscode.m16 (name)
        },
      },

      {
        content: textTurnInitial (),
        select: function (ele) {
          var name = ele.id();
          jscode.m6 (name)
        }
      }
    ]
  });

  var menu = cy.cxtmenu(nodeConfig);

  /* menu = cy.cxtmenu({
    selector: 'node',

    commands: [
      {
        content: textRemove (),
        select: function (event) {
          var target = event.target || event.cyTarget;
          var name = target.id();
          jscode.m3 (name);
        }
      },

      {
        content: textTurnFinal (),
        select: function (event) {
          var target = event.target || event.cyTarget;
          var name = target.id();
          jscode.m15 (name)
        }
      },

      {
        content: textRemoveFinal (),
        select: function (event) {
          var target = event.target || event.cyTarget;
          var name = target.id();
          jscode.m16 (name)
        },
      },

      {
        content: textTurnInitial (),
        select: function (event) {
          var target = event.target || event.cyTarget;
          var name = target.id();
          jscode.m6 (name)
        }
      }
    ], 
    fillColor: 'rgba(5, 8, 9, 0.75)'
  }); */

  var nodeConfig1 = config({
    menuRadius: function menuRadius(ele) {
      return 100;
    }, // the radius of the circular menu in pixels
    selector: 'core',

    commands: [
      {
        content: textAdd (),
        select: function(ele){
          var person = prompt(textEnterState (), "A");
          jscode.m4 (person);
        
        }
      },
      {
        content: textAddInitial (),
        select: function (event) {
          var person = prompt(textEnterState (), "A");
          jscode.m6 (person);
        }
        
      },
      {
        content: textAddFinal (),
        select: function (event) {

          var person = prompt(textEnterState (), "A");
          jscode.m5 (person);
        }
        
      },

      {
        content: textAddTransition (),
        select: function (event) {
          var person = prompt(textEnterStartState(), "A");
          var symb = prompt(textEnterTransition (), "c");
          var state2 = prompt(textEnterEndState (), "A");
          jscode.m7 (person, state2, symb);
        }
      }
    ]
  });

  var menu2 = cy.cxtmenu(nodeConfig1);

  var nodeConfig3 = config({
    menuRadius: function menuRadius(ele) {
      return 55;
    }, // the radius of the circular menu in pixels
    selector: 'edge',

    commands: [
      {
        content: textRemove (),
        select: function (ele) {
          var name = ele.source().id();
          var name2 = ele.target().id();
          var s = ele.data('symbol');
          alert (s);
          jscode.m8 (name, name2, s);
        }
      }
    ]
  });

  var menu3 = cy.cxtmenu(nodeConfig3);

  cy.autounselectify( false );
  cy.$('#transparent').position('y', 200);
  cy.$('#transparent').position('x', -200);
  cy.$('#transparent').lock();

  cy.onresize = function(){jscode.m11 ()};
};

window.addEventListener('resize', checkForChanges);


function checkForChanges () {
  if (cy != null) {
    cy.resize();
    cy.fit();
  } 
}

function turnFinal (name) {
  var na = '#' + name;
  cy.$(na).classes('SUCCESS');
}

function removeFinal (name) {
  var na = '#' + name;
  cy.$(na).removeClass('SUCCESS');
}

function makeNode (nm, isStart, final)  {
  var  verify = cy.getElementById (nm);
  if (verify.length < 1) { 
    if (final == "true") {
    if (isStart == "true") {
      cy.add({
        data: { id: nm, name: nm }, classes: 'SUCCESS'
      });
      cy.$('#' + nm).position('y', 200);
      cy.$('#' + nm).position('x', -100);
      cy.$('#' + nm).lock();
      makeEdge ('transparent', nm, '')
    } else {
        cy.add({
          data: { id: nm, name: nm },
          position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }, classes: 'SUCCESS'
        });
    }
    } else {
      if (isStart == "true") {
        cy.add({
          data: { id: nm, name: nm }
        });
        cy.$('#' + nm).position('y', 200);
        cy.$('#' + nm).position('x', -100);
        cy.$('#' + nm).lock();
        makeEdge ('transparent', nm, '')
      } else {
          cy.add({
            data: { id: nm, name: nm },
            position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }
          });
      }
    }
    cy.fit();
  }
};

function makeEdge (first, second, third)  {

  var nId = first + second;
  var getEdge = cy.getElementById(nId);

  if (getEdge.length  == 0) {
    cy.add({
      data: { id: nId, source: first, symbol: third, target: second }
    });
  } else {
    var k = getEdge.data('symbol');
    getEdge.remove();
    var newsymbol = k + ', ' + third;
    cy.add({
      data: { id: nId, source: first, symbol: newsymbol, target: second }
    })
  }
};

function removeNode (node) {
  var getNode = cy.getElementById(node);
  getNode.remove();
}

function removeEdge (first, second, third)  {

  var nId = first + second;
  var getEdge = cy.getElementById(nId);

  var k = getEdge.data('symbol');

  var g = "";

  for (i = 0; i < k.length; i++) {
    var j = k.charAt (i);
    if (j != ',' && j != ' ' && j != third) {
      if (g.length == 0) {
        g = j;
      } else {
        g = g + ', ' + j;
      }
    }
  }
  getEdge.remove();
  if (g.length > 0) {
    cy.add({
      data: { id: nId, source: first, symbol: g, target: second}
    })
  }
};

function resetStyle () {
  cy.style()
    .resetToDefault()
    .selector ('node[name]')
    .style ({'content': 'data(name)',
          'width': '40px',
          'height': '40px', 'text-valign': 'bottom',
          'text-halign': 'center'})
    .selector( 'edge[symbol]')
    .style ( {
                'content': 'data(symbol)'
              })
    .selector( 'edge')
    .style ({
            'curve-style': 'bezier',
            'target-arrow-shape': 'triangle'
          })
    .selector ('.SUCCESS')
    .style ({
                'border-width': '10px',
                'border-color': 'black',
                'border-style': 'double'
              })
    .selector( '#transparent')
    .style ({
          'visibility': 'hidden'
    })
    .selector( '.eh-handle')
    .style ({
      'background-color': 'red',
      'width': 12,
      'height': 12,
      'shape': 'ellipse',
      'overlay-opacity': 0,
      'border-width': 12, // makes the handle easier to hit
      'border-opacity': 0
    })
    .selector( '.eh-hover')
    .style ({
      'background-color': 'red'
    })
    .selector( '.eh-source')
    .style ({
      'border-width': 2,
      'border-color': 'red'
    })
    .selector( '.eh-target')
    .style ({
      'border-width': 2,
      'border-color': 'red'
    })
    .selector( '.eh-preview, .eh-ghost-edge')
    .style ({
      'background-color': 'red',
    'line-color': 'red',
    'target-arrow-color': 'red',
    'source-arrow-color': 'red'
    })
    .selector('.eh-ghost-edge.eh-preview-active')
    .style ({
      'opacity': 0
    })
    .update()
}


function paintNode (node, color) {
  cy.style ()
    .selector('#' + node)
    .style ( {
      'background-color': color
    })
    .update()
}

function start2 () {
  cy2 = window.cy2 = cytoscape({
    container: document.getElementById('cy2'),
    layout: {
      name: 'grid',
      rows: 2,
      cols: 2
    },
    style: [
      {
        selector: 'node[name]',
        style: {
          'content': 'data(name)',
          'width': '40px',
          'height': '40px',
          'text-valign': 'bottom',
          'text-halign': 'center'
        }
      },
      {
        selector: 'edge[symbol]',
        style: {
          'content': 'data(symbol)'
        }
      },
      {
        selector: 'edge',
        style: {
          'curve-style': 'bezier',
          'target-arrow-shape': 'triangle'
        }
      },
    {
      selector: '#transparent1',
      style: {
        'visibility': 'hidden'
      }
    },
    {
      selector: '.SUCCESS',
      style: {
        'border-width': '7px',
        'border-color': 'black',
        'border-style': 'double'
      }
  },
    ],
    elements: {
      nodes: [
        {data: { id: 'transparent1', name: 'transparent1' }}
      ]
    }
  });
  cy2.$('#transparent1').position('y', 200);
  cy2.$('#transparent1').position('x', -200);
  cy2.$('#transparent1').lock();
}

function makeNode2 (nm, isStart, final)  {
  var  verify = cy2.getElementById (nm);
  if (verify.length < 1) { 
    if (final == "true") {
    if (isStart == "true") {
      cy2.add({
        data: { id: nm, name: nm }, classes: 'SUCCESS'
      });
      cy2.$('#' + nm).position('y', 200);
      cy2.$('#' + nm).position('x', -100);
      cy2.$('#' + nm).lock();
      makeEdge2 ('transparent1', nm, '')
    } else {
        cy2.add({
          data: { id: nm, name: nm },
          position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }, classes: 'SUCCESS'
        });
    }
    } else {
      if (isStart == "true") {
        cy2.add({
          data: { id: nm, name: nm }
        });
        cy2.$('#' + nm).position('y', 200);
        cy2.$('#' + nm).position('x', -100);
        cy2.$('#' + nm).lock();
        makeEdge2 ('transparent1', nm, '')
      } else {
          cy2.add({
            data: { id: nm, name: nm },
            position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }
          });
      }
    }
    cy2.fit();
  }
};

function makeEdge2 (first, second, third)  {

  var nId = first + second;
  var getEdge = cy2.getElementById(nId);

  if (getEdge.length  == 0) {
    cy2.add({
      data: { id: nId, source: first, symbol: third, target: second }
    });
  } else {
    var k = getEdge.data('symbol');
    getEdge.remove();
    var newsymbol = k + ', ' + third;
    cy2.add({
      data: { id: nId, source: first, symbol: newsymbol, target: second }
    })
  }

  cy2.fit();
};

function paintNode1 (node, color) {
  cy2.style ()
    .selector('#' + node)
    .style ( {
      'background-color': color
    })
    .update()
}


/* -------------------- Regular Expressions -------------------- */

var stack = new Array ();
var conjunto = [[], []];
var counter = 0;
var number = 0;

function makeNewTree (s) {
  var index = s.indexOf("|");
  var index2 = s.indexOf ("/");
  var st = "|";
  if (index > index2) {
    index = index2;
    st = "/";
  }
  var str = "";
  var newString = "";
  if (index != -1) {
    str = (s.substring(0, index));
    newString = (s.substring(index + 1, s.length));
  } 
  if (st == "/") {
    stack.pop();
  } else {
    var newId = "n" + number;
    if (str.length != 0) {
    if (stack.length != 0) {
      conjunto[1].push ({data: {source: stack[stack.length-1], target: newId}});
    }
      conjunto[0].push ({data: {id: newId, name: str}, classes: str});
      stack.push (newId);
      number++;
    }
  }
  if (newString.length != 0) {
    makeNewTree (newString);
  }
}

function makeTree1 (s) {
  var idgen = "n" + number;
  number ++;
  var str = s;
  var st = str[0];
  switch (st) {
    case 'E': return [idgen, [{data: {id: idgen, name: "()"}}], [], str.substr(1)];
    case '+': case '.': {let [lid, lnode, ledge, lret] = makeTree1 (str.substr(1));
      let [rid, rnode, redge, rret] = makeTree1 (lret);
      return [idgen, [{data: {id: idgen, name: st }}].concat(lnode).concat(rnode), [{data: {source: idgen, target: lid}}].concat([{data: {source: idgen, target: rid}}]).concat(ledge).concat(redge), rret];
      }
    case '*': let [cid, cnode, cedge, cret] = makeTree1 (str.substr(1));
              return [idgen, [{data: {id: idgen, name: "*"}}].concat(cnode), [{data: {source: idgen, target: cid}}].concat(cedge), cret];
    default: return [idgen, [{data: {id: idgen, name: st}}], [], str.substr(1)];
  }
}

function startTree(nString) {
  var teste = makeTree1 (nString);
  layoutDir = "LR";
  cy = window.cy = cytoscape({
    container: document.getElementById('cy'),
    boxSelectionEnabled: false,
    autounselectify: true,
    layout: {
      name: 'dagre',
      rankDir: 'LR'
    },
    style: [
      {
        selector: 'node[name]',
        style: {
          'content': 'data(name)',
          'width': '40px',
          'height': '40px',
          'text-valign': 'center',
          'text-halign': 'center',
          'font-size': '20px'
        }
      },
      {
        selector: 'node',
        style: {
          'background-color': 'white'
        }
      },
      {
        selector: 'edge',
        style: {
          'width': 4,
          'target-arrow-shape': 'triangle',
          'line-color': '#9dbaea',
          'target-arrow-color': '#9dbaea',
          'curve-style': 'bezier'
        }
      }
    ],
    elements: {
      nodes: teste[1],
      edges: teste[2]
    }
  });
  direction = 'LR';

};

function startTree1(nString) {
  stack = new Array ();
  conjunto = [[], []];
  counter = 0;
  number = 0;
  makeNewTree (nString);
  cy2 = window.cy2 = cytoscape({
    container: document.getElementById('cy2'),
    boxSelectionEnabled: false,
    autounselectify: true,
    layout: {
      name: 'dagre',
      rankDir: 'TB'
    },
    style: [
      {
        selector: 'node[name]',
        style: {
          'content': 'data(name)',
          'width': '110px',
          'height': '40px',
          'text-valign': 'center',
          'text-halign': 'center',
          'font-size': '20px'
        }
      },
      {
        selector: '.Fail',
        style: {
          'color': 'red'
        }
      },
      {
        selector: 'node',
        style: {
          'background-color': 'white'
        }
      },
      {
        selector: 'edge',
        style: {
          'width': 4,
          'target-arrow-shape': 'triangle',
          'line-color': '#9dbaea',
          'target-arrow-color': '#9dbaea',
          'curve-style': 'bezier'
        }
      }
    ],
    elements: {
      nodes: conjunto[0],
      edges: conjunto[1]
    }
  });

};

function changeToHorizontal () {
  cy.layout ({name: 'dagre', rankDir: 'LR'}).run()
};

function changeToVertical () {
  cy.layout ({name: 'dagre', rankDir: 'TB'}).run()
};

function changeDirection () {
  if (layoutDir == "LR") {

    layoutDir = "TB";
    cy.layout ({name: 'dagre', rankDir: 'TB'}).run();
    
  } else {
    layoutDir = "LR";
    cy.layout ({name: 'dagre', rankDir: 'LR'}).run();
  };
}

/* ----------------- General Functions ------------------ */

function destroy1 () {
  if (cy != null) {
    cy.destroy();
    cy = null;
  }

};

function fit () {
  if (cy != null) {
    cy.resize();
    cy.fit();
  }
  if (cy2 != null) {
    cy2.resize();
    cy2.fit();
  }

};

function destroy2 () {
  if (cy2 != null) {
    cy2.destroy();
  }
  cy2 = null;

}; 

/* --------- Input Functions ----------------- */

function getNumber() {
  var person = prompt(textMaximumSize (), "4");
  jscode.m10 (person);
}

function startRegex() {
  var person = prompt(textRegex (), "ab");
  jscode.m12 (person);
}

function complete() {
  var person = prompt(textWord (), "ab");
  jscode.m13 (person);
}

function startStep() {
  var person = prompt(textWord (), "ab");
  jscode.m14 (person);
}

/* ------- Not Used ---------- */
function makeTreeNode2 (nm)  {
  var newId = 'n' + number;
    ids.push (newId);
    cy2.add({
      data: { id: newId, name: nm }
    });

  cy.fit();
};

function makeTreeEdge2 (first, second)  {

    cy2.add({
      data: { source: first, target: second }
    });
  cy2.fit();
};

