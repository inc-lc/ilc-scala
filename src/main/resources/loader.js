// Grid dimensions in pixels
var width = 20
var height = 20

// globals
var running
var state

function load (){
  // Set globals
  running = false
  $("#trace").empty()
  $("#runbreak").html(" Run ")

  var lambdaman = $("#lambda").val()
  var map       = $("#map").val()
  var ghosts    = $(".g").map(function(){return this.value}).get()
  state = loadGame(map, lambdaman, ghosts)
  if (state.error != null){
    $(".run").attr("disabled", "disabled")
    updateStatus("Error: " + state.error) 
  }
  else {
    $(".run").removeAttr("disabled")
    updateStatus("Program Loaded")
    setupBoard()
    updateBoard(true)
    updateState()
  }
}

function stepProg(o){
  h$runSync( h$c2( h$ap1_e
                 , h$mainZCMainzigameStepWrapper
                 , h$c1(h$ghcjszmprimZCGHCJSziPrimziJSRef_con_e, o)
                 )
           , false
           );
}

function loadGame (gameBoard, lmanProg, gs){
  var o = { gameboard: gameBoard, lmanprog: lmanProg, ghostprogs: gs };

  h$runSync( h$c2( h$ap1_e
                 , h$mainZCMainziloadGameWrapper
                 , h$c1(h$ghcjszmprimZCGHCJSziPrimziJSRef_con_e, o)
                 )
           , false
           );
  return o;
}

function setupBoard(){
  var board = state.board
  var y = board.length * width
  var x = board[0].length * height
  var maze = document.getElementById("maze")
  maze.width = x
  maze.height = y
}

function updateBoard(firsttime){
  var board = state.board
  var y = board.length
  var x = board[0].length

  var ctx = document.getElementById("maze").getContext("2d")
  var img = document.getElementById("alltiles")

  for (var j = 0; j < y; j++){
    for (var i = 0; i < x; i ++) {
      var tileno = board[j][i]
      if (tileno != 0 || firsttime) {
        ctx.drawImage(img, tileno * width, 0, width, height, i * width, j * height, width, height)
      }
    }
  }
}

function step(){
  runStep()
  if (!state.gameOver) {
    updateStatus("Single step")
  }
}

function runbreak(){
  if (running) {
    dobreak()
  } else {
    run()
  }
}

function run(){
  running = true
  $("#step").attr("disabled", "disabled")
  $("#runbreak").html(" Break ")
  updateStatus("Game running")
  runLoop()
}

function dobreak(){
  running = false
  $(".run").removeAttr("disabled")
  $("#runbreak").html(" Run ")
  updateBoard(false)
  updateStatus("Broken by user")
  updateState()
}

function runStep(){
  stepProg(state)
  if (state.gameOver) {
    running = false
    victor = state.gameWin ? "You won" : "You lost"
    $(".run").attr("disabled", "disabled") 
    updateBoard(false)
    updateStatus("Game Over: " + victor)
    updateState()
  }
  else {
    updateState()
    updateBoard(false)
  } 
 }

function runLoop(){
  if (running) {
    runStep()
    setTimeout(runLoop, 50)
  }
}

function updateState() {
  $("#lives").html(state.lives)
  $("#ticks").html(state.ticks)
  $("#score").html(state.score)
  if (state.traceval != null) {
    for (var index = 0; index < state.traceval.length; ++index) {
      output(state.traceval[index]);
    }
  }
}

function updateStatus(s){
  $("#status").html(s)
}

function output(v){
  $("#trace").append(v + "<br>")
}

$(document).ready(function(){
  $(".run").attr("disabled", "disabled")
  $("#load").click(load)
  $("#step").click(step)
  $("#runbreak").click(runbreak)
})
