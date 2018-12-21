//
// F# program to generate random Mondrian Art.
//
// <<Jakub>>
// U. of Illinois, Chicago
// CS 341, Spring 2018
// Project #05
//

#light

//
// randomInt LB UB
//
// generates random integer in range LB..UB, inclusive.
//
// NOTE: if you want repeatable random numbers for testing,
// uncomment "let seed = 0".  If you want random images 
// every time, uncomment the other "let seed = ..." line.
//
let seed = System.DateTime.Now.Millisecond
//let seed = 0
let ranInt = new System.Random(seed)
let randomInt LB UB =
  ranInt.Next(LB, UB+1)

  //Function Generates random Width between 120 and width*1.5
let randomNumFuncWidth width =
   let upperB = (1.5 * (float(width)))
   if upperB < 120.0 then 
     120
   else 
     let newV = randomInt 120 ((int)upperB)
     newV
   
   //Function Generates random height between 120 and width*1.5
let randomNumFuncHeight height =
   let upperB = (1.5 * (float(height)))
   if upperB < 120.0 then 
     120
   else 
     let newV = randomInt 120 ((int)upperB)
     newV

//
// randomRect
//
// An example of generating a random-colored rectangle
// in HTML SVG format.
//

//selects either color red, skyblue, yellow, or white
let selectRandomColor x L=
  if x < 9 then 
    let L = [242;11;8]
    L
  else if x < 17 then 
    let L = [114;166;236]
    L
  else if x < 25 then 
    let L = [216;243;11]
    L
  else 
    let L = [255;255;255]
    L
  
  //Gets the 1st items in List
let get1st L =
  (List.item 0 L)
  //Gets the 2nd items in List
let get2nd L =
  (List.item 1 L)
  //Gets the 3rd items in List
let get3rd L =
  (List.item 2 L)

let randomRect x1 y1 x2 y2 = 
  let r = randomInt 0 100
  //Selecting which color to use
  let randColor = selectRandomColor r []

  //gets the R,B,G values from list
  let red = get1st randColor
  let green = get2nd randColor
  let blue = get3rd randColor
  
  let html = 
       "<rect x=" + (string x1) +
       " y=" + (string y1) + 
       " width=" + (string ((float (x2-x1))+1.0)) + 
       " height=" + (string ((float (y2-y1))+1.0)) + 
       " stroke=" + "black" +
       " stroke-width=" + string(3) +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
  html




//
// _mondrian x1 y1 x2 y2 canvasWidth canvasHeight
//
// Recursive helper function that randomly generates an image
// for the area denoted by the rectange (x1,y1) and (x2,y2),
// where (x1,y1) is the upper-left corner and (x2,y2) is the 
// lower-right corner.  The image is in HTML SVG format.
//
let rec _mondrian x1 y1 x2 y2 canvasWidth canvasHeight =  
  //let html = randomRect x1 y1 x2 y2
  let currentWidth = ((float(x2 - x1) + 1.0))
  let currentHeight = ((float(y2 - y1) + 1.0))

  let Width1 = x2 - x1
  let Height1 = y2 - y1

  let WidthCheck =  randomNumFuncWidth Width1
  let HeightCheck =  randomNumFuncHeight Height1
 
  //printfn "%A" currentWidth
  //printfn "%A" currentHeight
//If the region is wider than half the initial canvas width and the region is taller than half the initial canvas
//height: Split into smaller regions
  if (currentWidth) > ((float)canvasWidth / 2.0) && (currentHeight) > ((float)canvasHeight / 2.0) then 
  //generates random Width and multiplies it
    let randomWidth = randomInt 33 67
    let convertWidthPercent = ((float)randomWidth / 100.0)
    let newWidth =  currentWidth * convertWidthPercent
    //generates random Height and multiplies it
    let randomHeight = randomInt 33 67
    let convertHeightPercent = ((float)randomHeight / 100.0)
    let newHeight =  currentHeight * convertHeightPercent
    //Sets new X and Y cord
    let newXcord = (float)x1 + (float)newWidth
    let newYcord = (float)y1 + (float)newHeight
    //Function calls
    _mondrian x1 y1 newXcord newYcord canvasWidth canvasHeight +
    _mondrian newXcord y1 x2 newYcord canvasWidth canvasHeight +
    _mondrian x1 newYcord newXcord y2 canvasWidth canvasHeight +
    (_mondrian newXcord newYcord x2 y2 canvasWidth canvasHeight) 

//Else if the region is wider than half the initial canvas width: split it
  else if (currentWidth) > ((float)canvasWidth / 2.0) then 
    let randomWidth = randomInt 33 67
    let convertWidthPercent = ((float)randomWidth / 100.0)
    let newWidth =  canvasWidth * convertWidthPercent
    let newXcord = (float)x1 + (float)newWidth
    _mondrian x1 y1 newXcord y2 canvasWidth canvasHeight +
    _mondrian newXcord y1 x2 y2 canvasWidth canvasHeight

// Else if the region is taller than half the initial canvas height: split
  else if (currentHeight) > ((float)canvasHeight / 2.0) then
    let randomHeight = randomInt 33 67
    let convertHeightPercent = ((float)randomHeight / 100.0)
    let newHeight =  canvasHeight * convertHeightPercent
    let newYcord = (float)y1 + (float)newHeight
    _mondrian x1 y1 x2 newYcord canvasWidth canvasHeight +
    _mondrian x1 newYcord x2 y2 canvasWidth canvasHeight

// Else if the region is big enough to split both horizontally and vertically, and both a horizontal and vertical
//split are randomly selected, then split
  else if currentWidth > 120.0 && ((float)WidthCheck) < (x2 - x1) && currentHeight > 120.0 && ((float)HeightCheck) < (y2 - y1) then 
    //generates random Width and multiplies it
    let randomWidth = randomInt 33 67
    let convertWidthPercent = ((float)randomWidth / 100.0)
    let newWidth =  currentWidth * convertWidthPercent
    //generates random Height and multiplies it
    let randomHeight = randomInt 33 67
    let convertHeightPercent = ((float)randomHeight / 100.0)
    let newHeight =  currentHeight * convertHeightPercent
    //Sets new X and Y cord
    let newXcord = (float)x1 + (float)newWidth
    let newYcord = (float)y1 + (float)newHeight
    //Function calls
    _mondrian x1 y1 newXcord newYcord canvasWidth canvasHeight +
    _mondrian newXcord y1 x2 newYcord canvasWidth canvasHeight +
    _mondrian x1 newYcord newXcord y2 canvasWidth canvasHeight +
    (_mondrian newXcord newYcord x2 y2 canvasWidth canvasHeight) 
//Else if the region is wide enough to split horizontally, and a horizontal split is randomly selected split
  else if currentWidth > 120.0 && ((float)WidthCheck) < (x2 - x1) then
  //generates random Width and multiplies it
    let randomWidth = randomInt 33 67
    let convertWidthPercent = ((float)randomWidth / 100.0)
    let newWidth =  currentWidth * convertWidthPercent
    //Sets new X
    let newXcord = (float)x1 + (float)newWidth
    //Function calls
    _mondrian x1 y1 newXcord y2 canvasWidth canvasHeight +
    _mondrian newXcord y1 x2 y2 canvasWidth canvasHeight
//Else if the region is tall enough to split vertically, a vertical split is randomly selected split
  else if currentHeight > 120.0 && ((float)HeightCheck) < (y2 - y1) then
  //generates random Height and multiplies it
    let randomHeight = randomInt 33 67
    let convertHeightPercent = ((float)randomHeight / 100.0)
    let newHeight =  currentHeight * convertHeightPercent
    //Sets new Y
    let newYcord = (float)y1 + (float)newHeight
    //Function calls
    _mondrian x1 y1 x2 newYcord canvasWidth canvasHeight +
    _mondrian x1 newYcord x2 y2 canvasWidth canvasHeight

//Fill the current region
  else 
     //printfn "I am in the else statement"  //DEBUG PURPOSES
     randomRect x1 y1 x2 y2
     


 // html


//
// mondrian canvasWidth canvasHeight
//
// Randomly generates an image in the spirit of Piet Mondrian.
// Returns an HTML document containing an SVG image of the given
// canvas width and height.  
//
// SVG: https://www.w3schools.com/html/html5_svg.asp
//
let mondrian canvasWidth canvasHeight = 
  let prefix = "<html>\n<head></head>\n<body>\n" +
               "<svg width=\"" + (string canvasWidth) + 
               "\" height=\"" + (string canvasHeight) + "\">\n"
  //
  let image = _mondrian 0.0 0.0 ((float canvasWidth) - 1.0) ((float canvasHeight) - 1.0) (float(canvasWidth)) (float(canvasHeight))
  //
  let suffix = "</svg>\n</body>\n</html>\n"
  let html = prefix + image + suffix
  html


//
// main:
//
[<EntryPoint>]
let main argv =
  printfn "** Starting **"
  //
  let width = 1024
  let height = 768
  let filename = "..\\..\\..\\mondrian.html"  // same folder as F# code:
  //
  printfn "** Generating image... "
  let html = mondrian width height
  System.IO.File.WriteAllText(filename, html) 
  //
  printfn "** Done **"
  0
